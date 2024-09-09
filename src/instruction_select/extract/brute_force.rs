//! A brute-force implementation of finding the minimal ISA. This algorithm runs in
//! exponential time, but is guaranteed to produce the correct result. Or at least
//! that's the goal.

use crate::utils::IntoNamedDot;
use std::collections::HashSet;

use crate::{halide_ir::ast::Instr, instruction_select::Learned, Instructions};

use super::{
    covering_options::{ChoiceSet, EGraphCovering},
    minimal_isa::{IsaPruner, MinimalIsa},
};

pub struct BruteForceIsa<'a> {
    pub isa: HashSet<Instr>,
    pub learned: &'a Instructions<Learned>,
    pub covering: EGraphCovering<'a>,
    pruner: Box<dyn IsaPruner + 'a>,
}

impl<'a> MinimalIsa<'a> for BruteForceIsa<'a> {
    fn new(learned: &'a Instructions<Learned>) -> Self {
        let mut covering = EGraphCovering::from(&learned.egraph);

        for r in &learned.roots {
            covering.compute_from_root(*r);
        }

        learned
            .egraph
            .named_dot()
            .to_dot("simple.dot")
            .expect("dooootttt!!");

        // println!("{covering:#?}");

        Self {
            isa: HashSet::default(),
            learned,
            covering,
            pruner: Box::new(()),
        }
    }

    fn set_pruner(&mut self, pruner: impl IsaPruner + 'a) -> &mut Self {
        self.pruner = Box::new(pruner);
        self
    }

    fn minimize(&mut self) {
        let options: Vec<_> = self
            .learned
            .roots
            .iter()
            .map(|r| self.covering.get(r).expect("No ISA for root."))
            .filter(|r| !r.is_empty())
            .map(|choices| self.pruner.prune(choices.clone()))
            .collect();

        // decide if this will be computationally feasible
        let number_options = options
            .iter()
            .map(|choices| choices.len())
            .fold(1usize, usize::saturating_mul);

        if number_options > 100_000_000 {
            panic!("Too many options to compute minimal ISA by brute force.");
        }

        self.isa = options
            .iter()
            .fold(ChoiceSet::from([]), ChoiceSet::cross_product)
            .into_iter()
            .min_by_key(|iset| iset.len())
            .unwrap_or_default()
            .into_inner();
    }

    fn isa(&self) -> &HashSet<Instr> {
        &self.isa
    }
}
