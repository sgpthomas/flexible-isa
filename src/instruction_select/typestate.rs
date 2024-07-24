use derive_deftly::{define_derive_deftly, Deftly};

use super::HalideExprOp;

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

/// State once we have run anti-unification. We now have a library of "instructions"
#[derive(Deftly)]
#[derive_deftly(InstructionState)]
pub struct AntiUnified {
    pub learned_lib: babble::LearnedLibrary<HalideExprOp, (egg::Id, egg::Id)>,
}
