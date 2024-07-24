//! Use babble to learn common patterns in a list of expressions.

use babble::Teachable;

use super::{cost::InstructionSelect, lang::HalideExprOp, AntiUnified, Init, InstructionState};

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

impl Instructions<Init> {
    pub fn add_expr(&mut self, expr: &egg::RecExpr<babble::AstNode<HalideExprOp>>) {
        let root = self.egraph.add_expr(expr);
        self.roots.push(root);
    }

    pub fn anti_unify(mut self) -> Instructions<AntiUnified> {
        // we need to rebuild the e-graph before learning a library
        self.egraph.rebuild();

        let mut learned_library = babble::LearnedLibraryBuilder::default()
            .learn_trivial(true)
            .ban_op(HalideExprOp::Cast(vec![]))
            .with_roots(self.roots.clone())
            .build(&self.egraph);

        // finds patterns that can apply in the same places, and only keeps the smaller
        // pattern
        learned_library.deduplicate(&self.egraph);

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

        let rewrites = self.instructions().collect::<Vec<_>>();

        let runner = egg::Runner::default().with_egraph(egraph).run(&rewrites);

        let extractor = egg::Extractor::new(&runner.egraph, InstructionSelect);
        let (_, best) = extractor.find_best(root);

        best
    }

    pub fn instructions(
        &self,
    ) -> impl Iterator<Item = egg::Rewrite<babble::AstNode<HalideExprOp>, ()>> + '_ {
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
}
