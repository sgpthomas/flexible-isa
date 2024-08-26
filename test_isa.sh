#!/bin/bash

# fail if any command fails
# set -e

function isa() {
    target/release/flexible-isa --learn test/$1.stmt
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
files=( "${w_ext[@]/#/test/}" )
# cargo run --release -- ${w_ext[@]/#/test/}
cargo run --release -- --learn "${files[@]:0:7}"
# cargo run --release -- --learn test/dilate3x3.stmt

# echo "2"
# target/debug/flexible-isa --learn ${files[@]:0:2}
# echo "done"
# echo ""

# echo "3"
# target/debug/flexible-isa --learn ${files[@]:0:3}
# echo "done"
# echo ""

# echo "4"
# target/debug/flexible-isa --learn ${files[@]:0:4}
# echo "done"
# echo ""

# echo "5"
# target/debug/flexible-isa --learn ${files[@]:0:5}
# echo "done"
# echo ""
