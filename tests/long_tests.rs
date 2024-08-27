use common::test_single_isa;

mod common;

#[test]
fn conv_nn_isa() -> anyhow::Result<()> {
    test_single_isa("conv_nn")
}

#[test]
fn median3x3_isa() -> anyhow::Result<()> {
    test_single_isa("median3x3")
}

#[test]
fn camera_pipe_isa() -> anyhow::Result<()> {
    test_single_isa("camera_pipe")
}

#[test]
fn depthwise_conv_isa() -> anyhow::Result<()> {
    test_single_isa("depthwise_conv")
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
fn sobel3x3_isa() -> anyhow::Result<()> {
    test_single_isa("sobel3x3")
}

#[test]
fn dilate3x3_isa() -> anyhow::Result<()> {
    test_single_isa("dilate3x3")
}
