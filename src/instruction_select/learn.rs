//! Use babble to learn common patterns in a list of expressions.

use std::time::Instant;

use super::lang::HalideExprOp;

#[derive(Default)]
pub struct Instructions {
    egraph: egg::EGraph<babble::AstNode<HalideExprOp>, ()>,
    roots: Vec<egg::Id>,
}

#[allow(unused)]
impl Instructions {
    pub fn add_expr(&mut self, expr: &egg::RecExpr<babble::AstNode<HalideExprOp>>) {
        let root = self.egraph.add_expr(expr);
        self.roots.push(root);
    }

    pub fn learn(&self) -> Vec<egg::Pattern<babble::AstNode<HalideExprOp>>> {
        log::info!("Running co-occurence analysis...");
        let co_time = Instant::now();
        let co_ext: babble::COBuilder<HalideExprOp, ()> =
            babble::COBuilder::new(&self.egraph, &self.roots);
        let co_occurs = co_ext.run();
        log::info!("Finished in {}ms", co_time.elapsed().as_millis());

        log::info!("Running anti-unification... ");
        let au_time = Instant::now();
        let learned_lib = babble::LearnedLibrary::new(&self.egraph, true, None, co_occurs);
        log::info!(
            "Found {} patterns in {}ms",
            learned_lib.size(),
            au_time.elapsed().as_millis()
        );

        learned_lib.libs().collect()
    }
}
