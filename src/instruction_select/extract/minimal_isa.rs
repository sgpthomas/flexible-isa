use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::{
    halide_ir::ast::Instr, instruction_select::Learned, utils::PrefixLines, HalideLang,
    Instructions,
};

use super::instruction_covering::ChoiceSet;

pub trait BestIsa<'a> {
    fn select(&mut self);
    fn isa(&self) -> &HashSet<Instr>;
    fn dump<'s>(
        &'s self,
        instructions: &'s HashMap<Instr, egg::Pattern<HalideLang>>,
    ) -> BestIsaDump<'s> {
        BestIsaDump {
            isa: self.isa(),
            instructions,
        }
    }
}

pub trait IntoBestIsa<'a> {
    fn make(self, learned: &'a Instructions<Learned>) -> Box<dyn BestIsa<'a> + 'a>
    where
        Self: Sized;
}

pub struct BestIsaDump<'a> {
    pub isa: &'a HashSet<Instr>,
    pub instructions: &'a HashMap<Instr, egg::Pattern<HalideLang>>,
}

impl<'a> std::fmt::Debug for BestIsaDump<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "BestIsa {{")?;
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
