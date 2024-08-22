use derive_deftly::{define_derive_deftly, Deftly};

use super::HalideLang;

// This is unnecessary, but I wanted to play around with these deftly derive macros
define_derive_deftly! {
    InstructionState:

    impl InstructionState for $ttype {}
}

/// Type state for Instructions
pub trait InstructionState {}

/// Initial state used for adding expressions to the instruction selector
#[derive(Deftly)]
#[derive_deftly(InstructionState)]
pub struct Init;

/// State after we have learned a set of instructions
#[derive(Deftly)]
#[derive_deftly(InstructionState)]
pub struct Learned {
    pub instructions: Vec<(usize, egg::Pattern<HalideLang>)>,
}
