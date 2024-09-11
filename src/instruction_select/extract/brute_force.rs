//! A brute-force implementation of finding the minimal ISA. This algorithm runs in
//! exponential time, but is guaranteed to produce the correct result. Or at least
//! that's the goal.

use indicatif::ProgressIterator;
use itertools::Itertools;

#[allow(unused)]
use crate::utils::IntoNamedDot;
use std::collections::{HashMap, HashSet};

use crate::{halide_ir::ast::Instr, instruction_select::Learned, utils, Instructions};

use super::{
    covering_options::{ChoiceSet, EGraphCovering},
    minimal_isa::{IsaPruner, MinimalIsa},
};

pub struct BruteForceIsa<'a> {
    pub isa: HashSet<Instr>,
    pub learned: &'a Instructions<Learned>,
    pruner: Box<dyn IsaPruner + 'a>,
}

impl<'a> BruteForceIsa<'a> {
    pub fn new(learned: &'a Instructions<Learned>) -> Self {
        Self {
            isa: HashSet::default(),
            learned,
            pruner: Box::new(()),
        }
    }

    pub fn set_pruner(&mut self, pruner: impl IsaPruner + 'a) -> &mut Self {
        self.pruner = Box::new(pruner);
        self
    }
}

impl<'a> MinimalIsa<'a> for BruteForceIsa<'a> {
    fn minimize(&mut self) {
        let mut covering = EGraphCovering::new(&self.learned.egraph, &(*self.pruner));

        for r in self.learned.roots.iter().progress_with(
            utils::progress_bar(self.learned.roots.len() as u64)
                .with_message("Computing options..."),
        ) {
            covering.compute_from_root(*r);
        }

        let options: Vec<_> = self
            .learned
            .roots
            .iter()
            .map(|r| covering.get(r).expect("No ISA for root."))
            // filter out empty sets (I suppose this shouldn't happen actually)
            .filter(|r| !r.is_empty())
            .cloned()
            .collect();

        println!("Done pruning");

        // decide if this will be computationally feasible
        let number_options = options
            .iter()
            .map(|choices| choices.len())
            .fold(1usize, usize::saturating_mul);

        if number_options > 100_000_000 {
            panic!("Too many options to compute minimal ISA by brute force: {number_options}");
        } else {
            println!("n: {number_options}");
        }

        let sorted_isas: Vec<_> = options
            .iter()
            .progress_with(utils::progress_bar(options.len()).with_message("Final product"))
            .fold(ChoiceSet::from([]), ChoiceSet::cross_product)
            .into_iter()
            .sorted_by_key(|iset| iset.len())
            .collect();

        let instructions: HashMap<_, _> = self.learned.instructions().collect();
        if let Some(smallest) = sorted_isas.first() {
            let best_isas: Vec<_> = sorted_isas
                .iter()
                .take_while(|iset| iset.len() == smallest.len())
                .inspect(|iset| println!("{:?}", iset.dump(&instructions)))
                .collect();
            self.isa = best_isas[0].clone().into_inner();
        }
    }

    fn isa(&self) -> &HashSet<Instr> {
        &self.isa
    }
}
