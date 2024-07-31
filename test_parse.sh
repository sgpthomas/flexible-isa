#!/bin/bash

# fail if any command fails
set -e

function stmt() {
    if [ ! -e "test/$1.stmt" ]; then
        echo "Generating test/$1.stmt"
        bisa --from benchmark benchmarks/$1 -o test/$1.stmt
    fi
}

function json() {
    if [ ! -e "test/$1.json" ]; then
        echo "Generating test/$1.json"
        bisa --from benchmark benchmarks/$1 -o test/$1.json
    fi
}

benchmarks=(add average_pool blur3x3 camera_pipe conv3x3a16 conv3x3a32 conv_nn depthwise_conv dilate3x3 \
                gaussian3x3 gaussian5x5 gaussian7x7 l2norm matmul max_pool mean median3x3 mul sobel3x3 softmax) 
broken_benchmarks=(elementwise fully_connected)

# generate the files
for b in ${benchmarks[@]}; do
    stmt $b
    json $b
done
