#include "Halide.h"
#include <cstdint>

using namespace Halide;
using namespace Halide::ConciseCasts;

class Simple : public Generator<Simple> {
public:
    Input<uint16_t> offset{"offset"};
    Input<Buffer<uint16_t>> input{"input"};

    Output<Buffer<uint16_t>> brighter{"brighter"};

    Var x;

    void generate() {
        // brighter(x) = u8_sat(widening_mul(input(x), offset));
        brighter(x) = input(x) * offset;

        // schedule
        brighter.vectorize(x, 8);
    }
};

HALIDE_REGISTER_GENERATOR(Simple, simple)
