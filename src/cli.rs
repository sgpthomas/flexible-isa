use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use itertools::Itertools;

use crate::{
    instruction_select::{
        BestIsa, BruteForceIsa, Experimental, IntoBestIsa, Learned, PairwisePrune, StrictOrdering,
    },
    Instructions,
};

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

    /// output halide ir. options: `parse, types, raw, instr`
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

    /// limit the number of learned instructions
    #[argh(option)]
    pub limit: Option<usize>,

    /// inline let expressions
    #[argh(switch)]
    pub no_inline: bool,

    /// algorithm to compute minimal ISA with
    #[argh(option, default = "BestIsaAlgo::BruteForce")]
    pub select_with: BestIsaAlgo,

    /// how to prune brute force options
    #[argh(option)]
    pub prune: Option<PruneType>,

    /// disable the type checker
    #[argh(switch)]
    pub disable_typechecker: bool,
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
            limit: None,
            no_inline: false,
            select_with: BestIsaAlgo::BruteForce,
            prune: None,
            disable_typechecker: false,
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
        self.output.push(OutputType::Rewritten);
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

    pub fn limit(mut self, limit: usize) -> Self {
        self.limit = Some(limit);
        self
    }

    pub fn no_inline(mut self, no_inline: bool) -> Self {
        self.no_inline = no_inline;
        self
    }

    pub fn select_with(mut self, algo: BestIsaAlgo) -> Self {
        self.select_with = algo;
        self
    }

    pub fn prune(mut self, prune: PruneType) -> Self {
        self.prune = Some(prune);
        self
    }

    pub fn disable_typechecker(mut self, val: bool) -> Self {
        self.disable_typechecker = val;
        self
    }
}

#[derive(Debug, derive_more::FromStr)]
pub enum OutputType {
    Parse,
    Types,
    Raw,
    Rewritten,
    Instrs,
    Miner,
    Dot,
    Pdf,
    BestIsa,
}

#[derive(Debug, derive_more::FromStr)]
pub enum PruneType {
    Pairwise,
    Experimental,
    Default,
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

    pub fn output_rewritten(&self) -> bool {
        self.output
            .iter()
            .any(|x| matches!(x, OutputType::Rewritten))
    }

    pub fn output_instrs(&self) -> bool {
        self.output.iter().any(|x| matches!(x, OutputType::Instrs))
    }

    pub fn output_miner(&self) -> bool {
        self.output.iter().any(|x| matches!(x, OutputType::Miner))
    }

    pub fn output_dot(&self) -> bool {
        self.output.iter().any(|x| matches!(x, OutputType::Dot))
    }

    pub fn output_pdf(&self) -> bool {
        self.output.iter().any(|x| matches!(x, OutputType::Pdf))
    }

    pub fn output_best_isa(&self) -> bool {
        self.output.iter().any(|x| matches!(x, OutputType::BestIsa))
    }
}

#[derive(Debug, derive_more::FromStr)]
pub enum BestIsaAlgo {
    BruteForce,
}

impl<'a> IntoBestIsa<'a> for &Args {
    fn make(self, learned: &'a Instructions<Learned>) -> Box<dyn BestIsa<'a> + 'a>
    where
        Self: Sized,
    {
        match self.select_with {
            BestIsaAlgo::BruteForce => {
                let mut isa = BruteForceIsa::new(learned);
                let pruner = StrictOrdering::new(learned);
                match self.prune {
                    Some(PruneType::Pairwise) => {
                        isa.set_pruner((pruner, PairwisePrune::default()));
                    }
                    Some(PruneType::Experimental) => {
                        isa.set_pruner((pruner, Experimental::default()));
                    }
                    Some(PruneType::Default) => {
                        isa.set_pruner((pruner, Experimental::default()));
                    }
                    None => (),
                }
                Box::new(isa)
            }
        }
    }
}

pub fn cli() -> Args {
    argh::from_env()
}
