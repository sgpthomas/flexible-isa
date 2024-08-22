use std::path::PathBuf;

#[derive(argh::FromArgs, Debug)]
/// Generate an ISA from a set of Halide files
pub struct Args {
    /// input .stmt file
    #[argh(positional)]
    pub input: Vec<PathBuf>,

    /// input .json metadata files
    #[argh(option)]
    pub types: Vec<PathBuf>,

    /// generate input / output types from halide generators
    #[argh(switch)]
    pub generator_types: bool,

    /// output halide ir
    #[argh(option)]
    pub output: Vec<OutputType>,

    /// save learned instructions to a file
    #[argh(option)]
    pub save: Option<PathBuf>,

    /// load saved instructions from a file
    #[argh(option)]
    pub load: Option<PathBuf>,

    /// learn new instructions
    #[argh(switch)]
    pub learn: bool,
}

#[derive(Debug, derive_more::FromStr)]
pub enum OutputType {
    Parse,
    Types,
    Raw,
    Instr,
}

impl Args {
    pub fn output_parse(&self) -> bool {
        self.output.iter().any(|x| matches!(x, OutputType::Parse))
    }

    pub fn output_types(&self) -> bool {
        self.output.iter().any(|x| matches!(x, OutputType::Types))
    }

    pub fn output_raw(&self) -> bool {
        self.output.iter().any(|x| matches!(x, OutputType::Raw))
    }

    pub fn output_instr(&self) -> bool {
        self.output.iter().any(|x| matches!(x, OutputType::Instr))
    }
}

pub fn cli() -> Args {
    argh::from_env()
}
