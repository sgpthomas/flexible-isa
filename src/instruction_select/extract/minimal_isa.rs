use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::{
    halide_ir::ast::Instr, instruction_select::Learned, utils::PrefixLines, HalideLang,
    Instructions,
};

use super::covering_options::ChoiceSet;

pub trait MinimalIsa<'a> {
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

pub trait IntoMinimalIsa<'a> {
    fn make(self, learned: &'a Instructions<Learned>) -> Box<dyn MinimalIsa<'a> + 'a>
    where
        Self: Sized;
}

pub struct MinimalIsaDump<'a> {
    pub isa: &'a HashSet<Instr>,
    pub instructions: &'a HashMap<Instr, egg::Pattern<HalideLang>>,
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
    #[allow(unused_variables, unused)]
    fn set_message(&mut self, msg: String) {}
}

impl IsaPruner for () {
    fn prune(&self, choices: ChoiceSet) -> ChoiceSet {
        choices
    }
}
