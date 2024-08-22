#!/bin/bash

# fail if any command fails
set -e

function isa() {
    cargo run --release -- test/$1.stmt
}

benchmarks=(add average_pool blur3x3 camera_pipe conv3x3a16 conv3x3a32 conv_nn depthwise_conv dilate3x3 \
                gaussian3x3 gaussian5x5 gaussian7x7 \
                l2norm matmul max_pool mean median3x3 mul sobel3x3 softmax)

# for b in ${benchmarks[@]}; do
#     isa $b
# done

w_ext=( "${benchmarks[@]/%/.stmt}" )
files=( "${w_ext[@]/#/test/}" )
# cargo run --release -- ${w_ext[@]/#/test/}
cargo run --release -- "${files[@]:0:5}"
