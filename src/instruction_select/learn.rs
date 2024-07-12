//! Use babble to learn common patterns in a list of expressions.

use std::time::{Duration, Instant};

use babble::Teachable;
use egg::Language;

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
        log::debug!("Running co-occurence analysis...");
        let co_time = Instant::now();
        let co_ext: babble::COBuilder<HalideExprOp, _> =
            babble::COBuilder::new(&self.egraph, &self.roots);
        let co_occurs = co_ext.run();
        log::debug!("Finished in {}ms", co_time.elapsed().as_millis());

        log::debug!("Running anti-unification... ");
        let au_time = Instant::now();
        let learned_library = babble::LearnedLibrary::new(&self.egraph, false, None, co_occurs);
        log::debug!(
            "Found {} patterns in {}ms",
            learned_library.size(),
            au_time.elapsed().as_millis()
        );

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
        // 3 step plan
        // 1: make a new egraph from the old one
        // 2: run all rewrites from the learned library
        // 3: extract program that prefers using libraries over not

        // extract list of rewrites from learned_lib
        let rewrites = self.state.learned_lib.rewrites().collect::<Vec<_>>();

        // apply them to the e-graph
        let mut runner = egg::Runner::<_, _, ()>::new(())
            .with_egraph(self.egraph.clone())
            .with_iter_limit(3)
            .with_time_limit(Duration::from_secs(60))
            .run(rewrites.iter());

        // get the root of the graph by adding all of our other roots
        // into a list node. we do this after applying rewrite rules
        // so that we can't apply any rules to the toplevel list.
        // this is probably an unnecessary pre-caution
        let root = runner.egraph.add(babble::AstNode::new(
            HalideExprOp::list(),
            self.roots.clone(),
        ));

        // extract the best program
        let extractor = egg::Extractor::new(&runner.egraph, ApplyInstructions);
        let (_, best) = extractor.find_best(root);

        babble::extract::lift_libs(&best)
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

/// A cost function the prefers instructions over anything else
struct ApplyInstructions;

impl egg::CostFunction<babble::AstNode<HalideExprOp>> for ApplyInstructions {
    type Cost = f64;

    fn cost<C>(&mut self, enode: &babble::AstNode<HalideExprOp>, mut costs: C) -> Self::Cost
    where
        C: FnMut(egg::Id) -> Self::Cost,
    {
        let op_cost = match enode.operation() {
            HalideExprOp::Babble(_) => 0.0,
            _ => 1.0,
        };

        enode.fold(op_cost, |sum, id| sum + costs(id))
    }
}
