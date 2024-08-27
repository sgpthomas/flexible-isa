use std::{
    fmt::Display,
    path::{Path, PathBuf},
};

use flexible_isa::{cli, run, HalideExprOp, InstructionSelect};
use itertools::Itertools;

// #[test]
// fn parse_tests() {
//     todo!()
// }

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

fn test_single_isa<S: AsRef<Path> + Display>(name: S) -> anyhow::Result<()> {
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
        .map(|(i, count)| (i, count, isa.instructions[&((*i) as usize)].to_string()))
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

#[test]
fn add_isa() -> anyhow::Result<()> {
    test_single_isa("add")
}

#[test]
fn average_pool_isa() -> anyhow::Result<()> {
    test_single_isa("average_pool")
}

#[test]
fn blur3x3_isa() -> anyhow::Result<()> {
    test_single_isa("blur3x3")
}

#[test]
fn camera_pipe_isa() -> anyhow::Result<()> {
    test_single_isa("camera_pipe")
}

#[test]
fn conv3x3a16_isa() -> anyhow::Result<()> {
    test_single_isa("conv3x3a16")
}

#[test]
fn conv3x3a32_isa() -> anyhow::Result<()> {
    test_single_isa("conv3x3a32")
}

#[test]
fn conv_nn_isa() -> anyhow::Result<()> {
    test_single_isa("conv_nn")
}

#[test]
fn depthwise_conv_isa() -> anyhow::Result<()> {
    test_single_isa("depthwise_conv")
}

#[test]
fn dilate3x3_isa() -> anyhow::Result<()> {
    test_single_isa("dilate3x3")
}

#[test]
fn gaussian3x3_isa() -> anyhow::Result<()> {
    test_single_isa("gaussian3x3")
}

#[test]
fn gaussian5x5_isa() -> anyhow::Result<()> {
    test_single_isa("gaussian5x5")
}

#[test]
fn gaussian7x7_isa() -> anyhow::Result<()> {
    test_single_isa("gaussian7x7")
}

#[test]
fn l2norm_isa() -> anyhow::Result<()> {
    test_single_isa("l2norm")
}

#[test]
fn matmul_isa() -> anyhow::Result<()> {
    test_single_isa("matmul")
}

#[test]
fn max_pool_isa() -> anyhow::Result<()> {
    test_single_isa("max_pool")
}

#[test]
fn mean_isa() -> anyhow::Result<()> {
    test_single_isa("mean")
}

#[test]
fn median3x3_isa() -> anyhow::Result<()> {
    test_single_isa("median3x3")
}

#[test]
fn mul_isa() -> anyhow::Result<()> {
    test_single_isa("mul")
}

#[test]
fn sobel3x3_isa() -> anyhow::Result<()> {
    test_single_isa("sobel3x3")
}

#[test]
fn softmax_isa() -> anyhow::Result<()> {
    test_single_isa("softmax")
}
