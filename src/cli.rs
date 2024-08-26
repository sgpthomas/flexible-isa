use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use itertools::Itertools;

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

impl Args {
    pub fn new<P>(stmt_files: &[P]) -> Self
    where
        P: Into<PathBuf> + AsRef<OsStr>,
    {
        Self {
            input: stmt_files.iter().map(|x| PathBuf::from(&x)).collect_vec(),
            types: vec![],
            generator_types: false,
            output: vec![],
            save: None,
            load: None,
            learn: true,
        }
    }

    pub fn types<P>(mut self, type_files: &[P]) -> Self
    where
        P: Into<PathBuf> + AsRef<OsStr>,
    {
        self.types = type_files.iter().map(|x| PathBuf::from(&x)).collect_vec();
        self
    }

    pub fn generator_types(mut self, generator_types: bool) -> Self {
        self.generator_types = generator_types;
        self
    }

    pub fn show_parse(mut self) -> Self {
        self.output.push(OutputType::Parse);
        self
    }

    pub fn show_types(mut self) -> Self {
        self.output.push(OutputType::Types);
        self
    }

    pub fn show_raw(mut self) -> Self {
        self.output.push(OutputType::Raw);
        self
    }

    pub fn show_instr(mut self) -> Self {
        self.output.push(OutputType::Instr);
        self
    }

    pub fn save(mut self, path: &Path) -> Self {
        self.save = Some(path.to_path_buf());
        self
    }

    pub fn load(mut self, path: &Path) -> Self {
        self.load = Some(path.to_path_buf());
        self
    }

    pub fn learn(mut self, learn: bool) -> Self {
        self.learn = learn;
        self
    }
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
