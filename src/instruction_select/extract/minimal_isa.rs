use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::{
    halide_ir::ast::Instr, instruction_select::Learned, utils::PrefixLines, HalideLang,
    Instructions,
};

use super::covering_options::ChoiceSet;

pub trait MinimalIsa<'a> {
    fn new(learned: &'a Instructions<Learned>) -> Self;
    #[allow(unused_variables)]
    fn set_pruner(&mut self, pruner: impl IsaPruner + 'a) -> &mut Self {
        self
    }
    fn minimize(&mut self);
    fn isa(&self) -> &HashSet<Instr>;
    fn dump<'s>(
        &'s self,
        instructions: &'s HashMap<Instr, egg::Pattern<HalideLang>>,
    ) -> MinimalIsaDump<'s> {
        MinimalIsaDump {
            isa: self.isa(),
            instructions,
        }
    }
}

pub struct MinimalIsaDump<'a> {
    isa: &'a HashSet<Instr>,
    instructions: &'a HashMap<Instr, egg::Pattern<HalideLang>>,
}

impl<'a> std::fmt::Debug for MinimalIsaDump<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "MinimalIsa {{")?;
        for i in self.isa.iter().sorted() {
            let prefix = format!("  {i}: ");
            writeln!(
                f,
                "{prefix}{}",
                self.instructions[i]
                    .pretty(80)
                    .prefix_lines(" ".repeat(prefix.len()))
            )?;
        }
        writeln!(f, "}}")?;

        Ok(())
    }
}

pub trait IsaPruner {
    fn prune(&self, choices: ChoiceSet) -> ChoiceSet;
}

impl IsaPruner for () {
    fn prune(&self, choices: ChoiceSet) -> ChoiceSet {
        choices
    }
}
