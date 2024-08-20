//! Use babble to learn common patterns in a list of expressions.

use babble::Teachable;

use crate::halide_ir::ast;

use super::{
    cost::InstructionSelect, lang::HalideExprOp, AntiUnified, HalideLang, Init, InstructionState,
};

pub type LibraryPattern = egg::Pattern<babble::AstNode<HalideExprOp>>;

pub struct Instructions<S: InstructionState> {
    egraph: egg::EGraph<babble::AstNode<HalideExprOp>, ()>,
    roots: Vec<egg::Id>,
    state: S,
}

impl Default for Instructions<Init> {
    fn default() -> Self {
        Self {
            egraph: egg::EGraph::default(),
            roots: Vec::default(),
            state: Init,
        }
    }
}

/// Replace every identifier in a partial expression with a hole
fn replace_idents(
    au: &babble::PartialExpr<HalideExprOp, egg::Var>,
) -> babble::PartialExpr<HalideExprOp, egg::Var> {
    /// Keep track of how many holes we have created, so that we can give
    /// the holes distinct names
    fn f(
        au: &babble::PartialExpr<HalideExprOp, egg::Var>,
        i: &mut u32,
    ) -> babble::PartialExpr<HalideExprOp, egg::Var> {
        match au {
            babble::PartialExpr::Node(node)
                if matches!(node.operation(), HalideExprOp::Ident(_)) =>
            {
                let hole = babble::PartialExpr::Hole(egg::Var::from_u32(*i));
                *i += 1;
                hole
            }
            babble::PartialExpr::Node(node) => {
                let (op, args) = node.as_parts();
                babble::PartialExpr::Node(babble::AstNode::new(
                    op.clone(),
                    args.iter().map(|a| f(a, i)),
                ))
            }
            x @ babble::PartialExpr::Hole(_) => x.clone(),
        }
    }

    f(au, &mut (au.num_holes() as u32))
}

impl Instructions<Init> {
    pub fn add_expr(&mut self, expr: &egg::RecExpr<HalideLang>) {
        let root = self.egraph.add_expr(expr);
        self.roots.push(root);
    }

    pub fn anti_unify(mut self) -> Instructions<AntiUnified> {
        // we need to rebuild the e-graph before learning a library
        self.egraph.rebuild();

        let mut learned_library = babble::LearnedLibraryBuilder::default()
            .learn_trivial(true)
            .ban_op(HalideExprOp::Cast(vec![]))
            .ban_op(HalideExprOp::FunCall(ast::Id::new("")))
            .with_roots(self.roots.clone())
            .build(&self.egraph);

        // I want to rewrite all instructions that contain variables into instructions that use pattern vars.
        learned_library.for_each_anti_unification(replace_idents);

        // finds patterns that can apply in the same places, and only keeps the smaller
        // pattern. I think that this only matters when we are using rewrite rules. Haven't
        // quite gotten there yet.
        println!("Deduplicating {} patterns...", learned_library.size());
        learned_library.deduplicate(&self.egraph);
        println!("Reduced to {} patterns", learned_library.size());

        let Self {
            egraph,
            roots,
            state: _,
        } = self;

        Instructions {
            egraph,
            roots,
            state: AntiUnified {
                learned_lib: learned_library,
            },
        }
    }
}

impl Instructions<AntiUnified> {
    pub fn apply(&self) -> egg::RecExpr<babble::AstNode<HalideExprOp>> {
        // extract the best program
        let mut egraph = self.egraph.clone();
        let root = egraph.add(babble::AstNode::new(
            HalideExprOp::list(),
            self.roots.clone(),
        ));

        let rewrites = self.rewrites().collect::<Vec<_>>();

        let runner = egg::Runner::default().with_egraph(egraph).run(&rewrites);

        let cost = InstructionSelect::new(&runner.egraph);
        let extractor = egg::Extractor::new(&runner.egraph, cost);
        let (_, best) = extractor.find_best(root);

        best
    }

    pub fn rewrites(&self) -> impl Iterator<Item = egg::Rewrite<HalideLang, ()>> + '_ {
        self.state
            .learned_lib
            .anti_unifications()
            .enumerate()
            .map(|(i, au)| {
                let pattern: egg::Pattern<_> = au.clone().into();

                let searcher: egg::Pattern<_> = pattern.clone();

                let head = HalideExprOp::Instruction(i as u64);
                let vars = pattern.vars().into_iter().map(babble::PartialExpr::Hole);
                let applier_partial_expr: babble::PartialExpr<_, _> =
                    babble::PartialExpr::Node(babble::AstNode::new(head, vars));
                let applier: egg::Pattern<_> = applier_partial_expr.into();

                let name = format!("instruction {i}");
                egg::rewrite!(name; searcher => applier)
            })
    }

    pub fn instructions(&self) -> impl Iterator<Item = (usize, egg::Pattern<HalideLang>)> + '_ {
        self.state
            .learned_lib
            .anti_unifications()
            .enumerate()
            .map(|(i, au)| (i, au.clone().into()))
    }
}
