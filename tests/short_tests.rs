use common::test_single_isa;

mod common;

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
fn mul_isa() -> anyhow::Result<()> {
    test_single_isa("mul")
}

#[test]
fn softmax_isa() -> anyhow::Result<()> {
    test_single_isa("softmax")
}
