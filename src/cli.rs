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
    pub output_ir: Vec<OutputIr>,

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
pub enum OutputIr {
    Parse,
    Types,
    Instr,
}

impl Args {
    pub fn output_parse(&self) -> bool {
        self.output_ir.iter().any(|x| matches!(x, OutputIr::Parse))
    }

    pub fn output_types(&self) -> bool {
        self.output_ir.iter().any(|x| matches!(x, OutputIr::Types))
    }

    pub fn output_instr(&self) -> bool {
        self.output_ir.iter().any(|x| matches!(x, OutputIr::Instr))
    }
}

pub fn cli() -> Args {
    argh::from_env()
}
