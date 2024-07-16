//! Use babble to learn common patterns in a list of expressions.

use babble::Teachable;
use egg::AstSize;

use crate::rewrite_recexpr::RecExprRewriter;

use super::{lang::HalideExprOp, AntiUnified, Init, InstructionState};

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

    pub fn anti_unify(self) -> Instructions<AntiUnified> {
        let learned_library = babble::LearnedLibraryBuilder::default()
            .learn_trivial(true)
            .ban_op(HalideExprOp::Cast(vec![]))
            .with_roots(self.roots.clone())
            .build(&self.egraph);

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
        let extractor = egg::Extractor::new(&egraph, AstSize);
        let (_, best) = extractor.find_best(root);

        // extract list of rewrites from learned_lib
        let rewrites: Vec<egg::Rewrite<_, ()>> =
            self.state.learned_lib.rewrites().collect::<Vec<_>>();

        // lift the instructions to the top of the program
        babble::extract::lift_libs(&best.destructively_rewrite(rewrites))
    }

    pub fn instructions(&self) -> impl Iterator<Item = LibraryPattern> + '_ {
        self.state.learned_lib.libs()
    }

    // pub fn learn(&mut self) -> Vec<LibraryPattern> {
    //     // log::info!("Deduplicating patterns...");
    //     // learned_lib.deduplicate(&self.egraph);

    //     let lib_rewrites: Vec<_> = self
    //         .state
    //         .learned_lib
    //         .rewrites()
    //         .inspect(|rw| log::info!("{rw:?}"))
    //         .collect();

    //     log::info!("Adding libs and running beam search... ");
    //     let runner = egg::Runner::<_, _, ()>::new(PartialLibCost::new(
    //         400, 400, // beams
    //         1,   // the number of libs to learn at a time
    //     ))
    //     .with_egraph(self.egraph.clone())
    //     .with_iter_limit(3) // set to 1 in babble experiments
    //     .with_time_limit(Duration::from_secs(60))
    //     .with_node_limit(1_000_000)
    //     .run(lib_rewrites.iter());

    //     let mut egraph = runner.egraph;
    //     let root = egraph.add(babble::AstNode::new(
    //         HalideExprOp::list(),
    //         self.roots.clone(),
    //     ));
    //     let mut cs = egraph[egraph.find(root)].data.clone();
    //     cs.set.sort_unstable_by_key(|elem| elem.full_cost);

    //     log::info!("learned libs");
    //     let all_libs: Vec<_> = self.state.learned_lib.libs().collect();
    //     let mut chosen_rewrites = Vec::new();
    //     for lib in &cs.set[0].libs {
    //         log::info!("{}: {}", lib.0, &all_libs[lib.0 .0]);
    //         chosen_rewrites.push(lib_rewrites[lib.0 .0].clone());
    //     }

    //     log::info!("Extracting... ");
    //     let lifted =
    //         babble::extract::apply_libs(self.egraph.clone(), &self.roots, &chosen_rewrites);

    //     println!("final: {}", lifted.pretty(80));
    //     println!("== end ==");

    //     self.state.learned_lib.libs().collect()
    // }
}
