use std::collections::{HashMap, HashSet};

use indicatif::ProgressIterator;
use itertools::Itertools;

use crate::{halide_ir::ast, instruction_select::Learned, utils, Instructions};

use super::{
    instruction_covering::{ChoiceSet, CoveringDataMerge, EGraphCovering},
    minimal_isa::{BestIsa, IsaPruner},
};

pub struct BeamSearchIsa<'a> {
    isa: HashSet<ast::Instr>,
    learned: &'a Instructions<Learned>,
    beam_size: usize,
}

impl<'a> BeamSearchIsa<'a> {
    pub fn new(learned: &'a Instructions<Learned>) -> Self {
        Self {
            isa: HashSet::default(),
            learned,
            beam_size: usize::MAX,
        }
    }

    pub fn with_beam_size(mut self, beam_size: usize) -> Self {
        self.beam_size = beam_size;
        self
    }
}

impl<'a> IsaPruner<usize> for BeamSearchIsa<'a> {
    /// Only keep `self.beam_size` lowest cost coverings
    fn prune(&self, choices: ChoiceSet<usize>) -> ChoiceSet<usize> {
        choices
            .into_iter()
            .sorted_by_key(|covering| covering.data)
            .take(self.beam_size)
            .collect()
    }
}

impl CoveringDataMerge for usize {
    fn merge(self, other: &Self) -> Self {
        self.saturating_add(*other)
    }

    fn one() -> Self {
        1
    }

    fn zero() -> Self {
        0
    }
}

impl<'a> BestIsa<'a> for BeamSearchIsa<'a> {
    fn select(&mut self) {
        let covering: EGraphCovering<usize> =
            EGraphCovering::new(&self.learned.egraph, &*self).with_roots(&self.learned.roots);

        // println!("{covering:#?}");

        // TODO: abstract this code so that this is shared with bruteforce
        let options: Vec<_> = self
            .learned
            .roots
            .iter()
            .map(|r| covering.get(r).expect("No ISA for root."))
            // filter out empty sets (I suppose this shouldn't happen actually)
            .filter(|r| !r.is_empty())
            .cloned()
            .collect();

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
            .fold(ChoiceSet::empty(), ChoiceSet::cross_product)
            .into_iter()
            .sorted_by_key(|iset| iset.data)
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

    fn isa(&self) -> &HashSet<ast::Instr> {
        &self.isa
    }
}
