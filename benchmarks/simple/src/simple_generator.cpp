#include "Halide.h"
#include <cstdint>

using namespace Halide;
using namespace Halide::ConciseCasts;

class Simple : public Generator<Simple> {
public:
    Input<uint8_t> offset{"offset"};
    Input<Buffer<uint8_t, 1>> input{"input"};

    Output<Buffer<uint8_t, 1>> brighter{"brighter"};

    Var x;

    void generate() {
        brighter(x) = u8_sat(widening_mul(input(x), offset));

        // schedule
        brighter.vectorize(x, 16);
    }
};

HALIDE_REGISTER_GENERATOR(Simple, simple)
