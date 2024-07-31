use std::path::PathBuf;

#[derive(argh::FromArgs, Debug)]
/// Generate an ISA from a set of Halide files
pub struct Args {
    /// input .stmt file
    #[argh(positional)]
    pub input: Vec<PathBuf>,

    /// generate input / output types from halide generators
    #[argh(switch)]
    pub generator_types: bool,

    /// don't learn new instructions
    #[argh(switch)]
    pub dont_learn: bool,
}

pub fn cli() -> Args {
    argh::from_env()
}
