/// represents a benchmark directory
export const benchmark_dir = state("benchmark", []);

/// represents an input halide generator
export const halide_state = state("halide_gen", ["cpp", "c"]);

/// represents the produced generator binary
export const generator_state = state("generator_bin", ["gen"]);

/// halide IR state
export const stmt_state = state("halide-ir", ["stmt"]);
export const metadata_state = state("metadata", ["json"]);

/// assembly state
export const asm_state = state("asm", ["s"]);

/// llvm ir state
export const llvm_state = state("llvm", ["ll"]);

/// flexible isa state
export const flex_isa_state = state("isa", ["isa"]);

fn benchmark_setup(e) {
  e.rule(
    "extract-halide",
    "find $in/src -name \\*.h -exec cp {} . \\; && cat $in/src/${generator_name}_generator.cpp > $out"
  );
}

fn generator_setup(e) {
  e.config_var("halide_dir", "halide.root");
  e.config_var_or("cc", "c.compiler", "clang++");

  e.var_("halide_c_flags", "--std=c++17 -fno-rtti -O3 -DLOG2VLEN=7");
  e.var_("halide_includes", "-I $halide_dir/include -I $halide_dir/tools");
  e.var_("halide_libs", "-L $halide_dir/lib -lHalide");
  e.var_("gen_gen", "$halide_dir/tools/GenGen.cpp");

  e.config_var("root_dir", "root");
  e.var_("hannk_dir", "$root_dir/benchmarks/hannk");
  e.var_("hannk_srcs", "$hannk_dir/hannk/common_halide.cpp $hannk_dir/hannk/interpreter/elementwise_program.cpp");
  e.var_("hannk_includes", "-I $hannk_dir");

  e.rule(
    "halide-clang",
    "$cc $halide_c_flags $halide_includes $hannk_includes $halide_libs -g $in $gen_gen $hannk_srcs -o $out"
  );
}

fn halide_setup(e) {
  e.var_("halide_ld", "DYLD_LIBRARY_PATH=$halide_dir/lib LD_LIBRARY_PATH=$halide_dir/lib");
  e.var_("target", "target=hexagon-32-noos-no_bounds_query-no_asserts-hvx_128-hvx_v66");

  e.rule(
    "run-generator",
    "$halide_ld ./$in -o . -g $generator_name -e $output_type -f $generator_name $target && mv ./$generator_name.$output_ext $out"
  );
}

fn generate_build(e, input, output, type, ext) {
    e.build_cmd([output], "run-generator", [input], []);
    e.arg("output_type", type);
    e.arg("output_ext", ext);
}


fn project_setup(e) {
  e.config_var("proj_src", "root");
  e.var_("flex_isa", "$proj_src/target/debug/flexible-isa")
}

// fn metadata_setup(e) {
//   e.rule(
//     "gen-metadata",
//     "$flex_isa $in --generator-types > $out"
//   );
// }

op(
  "extract-halide",
  [benchmark_setup],
  benchmark_dir,
  halide_state,
  |e, input, output| {
    let name = input.split("/")[-1];
    e.config_var_or("generator_name", "generator_name",  name);
    e.build_cmd([output], "extract-halide", [input], []);
    // e.arg("name", name);
  }
);

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
    generate_build(e, input, output, "stmt", "stmt");
  }
);

op(
  "generate-llvm",
  [generator_setup, halide_setup],
  generator_state,
  llvm_state,
  |e, input, output| {
    generate_build(e, input, output, "llvm_assembly", "ll");
  }
);

op(
  "generate-asm",
  [generator_setup, halide_setup],
  generator_state,
  asm_state,
  |e, input, output| {
    generate_build(e, input, output, "assembly", "s");
  }
);

defop run_isa(s: stmt_state, m: metadata_state) >> f: flex_isa_state {
  let proj_src = config("root");
  let flex_isa = `${proj_src}/target/debug/flexible-isa`;
  shell(`${flex_isa} ${s} --types ${m} > ${f}`)  
}

defop gen_metadata(h: halide_state) >> m: metadata_state {
  let proj_src = config("root");
  let flex_isa = `${proj_src}/target/debug/flexible-isa`;
  shell(`${flex_isa} ${h} --generator-types > ${m}`)
}
