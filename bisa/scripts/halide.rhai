/// represents an input halide generator
export const halide_state = state("halide", ["c", "cpp"]);

/// represents the produced generator binary
export const generator_state = state("generator", ["gen"]);

/// halide IR state
export const stmt_state = state("halide-ir", ["stmt"]);

/// assembly state
export const asm_state = state("asm", ["s"]);

/// flexible isa state
export const flex_isa_state = state("isa", ["isa"]);

fn generator_setup(e) {
  e.config_var("halide_dir", "halide.base");
  e.config_var_or("cc", "c.compiler", "clang++");

  e.var_("halide_c_flags", "--std=c++17 -fno-rtti -O3 -DLOG2VLEN=7");
  e.var_("halide_includes", "-I $halide_dir/include -I $halide_dir/tools");
  e.var_("halide_libs", "-L $halide_dir/lib -lHalide");
  e.var_("gen_gen", "$halide_dir/tools/GenGen.cpp");

  e.rule(
    "halide-clang",
    "$cc $halide_c_flags $halide_includes $halide_libs $in $gen_gen -o $out"
  );
}

fn halide_setup(e) {
  e.var_("halide_ld", "DYLD_LIBRARY_PATH=$halide_dir/lib LD_LIBRARY_PATH=$halide_dir/lib");
  e.var_("target", "target=hexagon-32-noos-no_bounds_query-no_asserts-hvx_128-hvx_v66");

  e.rule(
    "run-generator",
    "$halide_ld ./$in -o . -g $gen_name -e $output_type -f $gen_name $target && mv ./$gen_name.$output_ext $out"
  );
}

fn isa_setup(e) {
  e.rule(
    "run-isa",
    "cargo run -- $in > $out"
  );
}

rule(
  [generator_setup],
  halide_state,
  generator_state,
  "halide-clang"
);

op(
  "generate-stmt",
  [generator_setup, halide_setup],
  generator_state,
  stmt_state,
  |e, input, output| {
    let gen_name = input.split("_")[0];

    e.build_cmd([output], "run-generator", [input], []);
    e.arg("gen_name", gen_name);
    e.arg("output_type", "stmt");
    e.arg("output_ext", "stmt");
  }
);

op(
  "generate-asm",
  [generator_setup, halide_setup],
  generator_state,
  asm_state,
  |e, input, output| {
    let gen_name = input.split("_")[0];

    e.build_cmd([output], "run-generator", [input], []);
    e.arg("gen_name", gen_name);
    e.arg("output_type", "assembly");
    e.arg("output_ext", "s");
  }
);

rule(
  [isa_setup],
  stmt_state,
  flex_isa_state,
  "run-isa"
);
