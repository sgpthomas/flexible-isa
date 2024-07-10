#!/bin/bash

set -e
set -v

# generate stmt files (manually caching things)
mkdir -p test_parse
# bisa --from benchmark -o test_parse/add.stmt benchmarks/add
# bisa --from benchmark -o test_parse/average_pool.stmt benchmarks/average_pool
# bisa --from benchmark -o test_parse/blur3x3.stmt benchmarks/blur3x3
# bisa --from benchmark -o test_parse/camera_pipe.stmt benchmarks/camera_pipe
# bisa --from benchmark -o test_parse/conv3x3a16.stmt benchmarks/conv3x3a16
# bisa --from benchmark -o test_parse/conv3x3a32.stmt benchmarks/conv3x3a32
# bisa --from benchmark -o test_parse/conv_nn.stmt benchmarks/conv_nn
# bisa --from benchmark -o test_parse/depthwise_conv.stmt benchmarks/depthwise_conv
# bisa --from benchmark -o test_parse/dilate3x3.stmt benchmarks/dilate3x3
# bisa --from benchmark -o test_parse/elementwise.stmt benchmarks/elementwise # BROKEN
# bisa --from benchmark -o test_parse/fully_connected.stmt benchmarks/fully_connected # BROKEN
# bisa --from benchmark -o test_parse/gaussian3x3.stmt benchmarks/gaussian3x3
# bisa --from benchmark -o test_parse/gaussian5x5.stmt benchmarks/gaussian5x5
# bisa --from benchmark -o test_parse/gaussian7x7.stmt benchmarks/gaussian7x7
# bisa --from benchmark -o test_parse/l2norm.stmt benchmarks/l2norm
# bisa --from benchmark -o test_parse/matmul.stmt benchmarks/matmul
# bisa --from benchmark -o test_parse/max_pool.stmt benchmarks/max_pool
# bisa --from benchmark -o test_parse/mean.stmt benchmarks/mean
# bisa --from benchmark -o test_parse/median3x3.stmt benchmarks/median3x3
# bisa --from benchmark -o test_parse/mul.stmt benchmarks/mul
# bisa --from benchmark -o test_parse/sobel3x3.stmt benchmarks/sobel3x3
# bisa --from benchmark -o test_parse/softmax.stmt benchmarks/softmax

# test parsing
bisa --to isa test_parse/add.stmt > /dev/null
bisa --to isa test_parse/average_pool.stmt > /dev/null
bisa --to isa test_parse/blur3x3.stmt > /dev/null
bisa --to isa test_parse/camera_pipe.stmt > /dev/null
bisa --to isa test_parse/conv3x3a16.stmt > /dev/null
bisa --to isa test_parse/conv3x3a32.stmt > /dev/null
bisa --to isa test_parse/conv_nn.stmt > /dev/null
bisa --to isa test_parse/depthwise_conv.stmt > /dev/null
bisa --to isa test_parse/dilate3x3.stmt > /dev/null
# bisa --to isa test_parse/elementwise.stmt > /dev/null # BROKEN
# bisa --to isa test_parse/fully_connected.stmt > /dev/null # BROKEN
bisa --to isa test_parse/gaussian3x3.stmt > /dev/null
bisa --to isa test_parse/gaussian5x5.stmt > /dev/null
bisa --to isa test_parse/gaussian7x7.stmt > /dev/null
bisa --to isa test_parse/l2norm.stmt > /dev/null
bisa --to isa test_parse/matmul.stmt > /dev/null
bisa --to isa test_parse/max_pool.stmt > /dev/null
bisa --to isa test_parse/mean.stmt > /dev/null
bisa --to isa test_parse/median3x3.stmt > /dev/null
bisa --to isa test_parse/mul.stmt > /dev/null
bisa --to isa test_parse/sobel3x3.stmt > /dev/null
bisa --to isa test_parse/softmax.stmt > /dev/null
