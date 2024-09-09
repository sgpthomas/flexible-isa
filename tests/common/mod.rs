use std::{
    fmt::Display,
    path::{Path, PathBuf},
};

use flexible_isa::{cli, run, HalideExprOp, InstructionSelect};
use itertools::Itertools;

const BENCHMARKS_PATH: &str = "cache";
#[allow(unused)]
const BENCHMAKRS: &[&str] = &[
    "add",
    "average_pool",
    "blur3x3",
    "camera_pipe",
    "conv3x3a16",
    "conv3x3a32",
    "conv_nn",
    "depthwise_conv",
    "dilate3x3",
    // "elementwise",
    // "fully_connected",
    "gaussian3x3",
    "gaussian5x5",
    "gaussian7x7",
    "l2norm",
    "matmul",
    "max_pool",
    "mean",
    "median3x3",
    "mul",
    "sobel3x3",
    "softmax",
];

pub fn test_single_isa<S: AsRef<Path> + Display>(name: S) -> anyhow::Result<()> {
    let mut path = PathBuf::from(BENCHMARKS_PATH).join(&name);
    path.set_extension("stmt");

    let args: cli::Args = cli::Args::new(&[path]).learn(true);
    let isa = run(args)?;

    let instr_hist = InstructionSelect::from_recexpr(&isa.expressions);
    let hist: Vec<_> = instr_hist
        .iter()
        .filter_map(|(op, count)| {
            if let HalideExprOp::Instruction(i) = op {
                Some((i, count))
            } else {
                None
            }
        })
        // sort first by count, then by index
        .sorted_by_key(|(i, count)| (*count, *i))
        .map(|(i, count)| (i, count, isa.instructions[i].to_string()))
        .collect();

    insta::with_settings!({
        description => format!("generate isa for {name}"),
        omit_expression => true,
        snapshot_suffix => format!("{name}")
    }, {
        insta::assert_debug_snapshot!(hist)
    });

    Ok(())
}
