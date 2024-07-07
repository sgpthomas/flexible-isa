benchmarks = add

bin_dirs = $(addsuffix /bin,$(benchmarks))
out_dirs = $(addsuffix /out,$(benchmarks))

hannk = hannk/common_halide.cpp \
	    hannk/interpreter/elementwise_program.cpp

.PHONY: clean all

all: $(benchmarks)

$(benchmarks): %: %/bin %/out
	clang++ --std=c++17 -fno-rtti -O3 -DLOG2VLEN=7 \
		-I $(HALIDE_DIR)/include -I $(HALIDE_DIR)/tools \
		-g $@/src/$@_generator.cpp $(HALIDE_DIR)/tools/GenGen.cpp \
		$(hannk) \
		-o $@/bin/$@_generator \
		-L $(HALIDE_DIR)/lib -lHalide
	DYLD_LIBRARY_PATH=$(HALIDE_DIR)/lib LD_LIBRARY_PATH=$(HALIDE_DIR)/lib ./$@/bin/$@_generator \
		-o $@/out \
		-g $@ -e stmt,assembly \
		-f $@_hvx128 target=hexagon-32-noos-no_bounds_query-no_asserts-hvx_128-hvx_v66

$(out_dirs) $(bin_dirs): %:
	mkdir $@

clean:
	rm -rf $(bin_dirs) $(out_dirs) pmu_statsfile.txt stats_dump.iss.0
