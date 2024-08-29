#!/bin/bash

# fail if any command fails
# set -e

function isa() {
    target/release/flexible-isa --learn cache/$1.stmt
}

benchmarks=(add average_pool blur3x3 conv3x3a16 conv3x3a32 depthwise_conv dilate3x3 \
                gaussian3x3 gaussian5x5 gaussian7x7 \
                camera_pipe conv_nn \
                l2norm matmul max_pool mean median3x3 mul sobel3x3 softmax)

# cargo build --release
# for b in ${benchmarks[@]}; do
#     isa $b
# done

w_ext=( "${benchmarks[@]/%/.stmt}" )
files=( "${w_ext[@]/#/cache/}" )
# cargo run --release -- ${w_ext[@]/#/test/}
cargo run --release -- "${files[@]:0:6}" $@
# cargo run --release -- --learn test/dilate3x3.stmt
