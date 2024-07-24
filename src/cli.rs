use std::path::PathBuf;

#[derive(argh::FromArgs, Debug)]
/// Generate an ISA from a set of Halide files
pub struct Args {
    /// input .stmt file
    #[argh(positional)]
    pub input: Vec<PathBuf>,
}

pub fn cli() -> Args {
    argh::from_env()
}
