use anyhow::{anyhow, Context};
#[doc(hidden)]
#[allow(clippy::single_component_path_imports)]
use derive_deftly;
#[allow(unused)]
use halide_ir::{
    HalideGeneratorParser, Inline, InsertCasts, LiftExpressions, MineExpressions, NumberNodes,
    Printer, RemoveCasts, Rewrite, StmtParser, TypeAnnotator, UniqueIdents, Visitor,
};
pub use instruction_select::{HalideExprOp, HalideLang, InstructionSelect, Instructions};
use itertools::Itertools;
use std::collections::HashMap;

pub mod cli;
mod halide_ir;
mod instruction_select;

#[derive(Default, Debug)]
pub struct Isa {
    pub expressions: egg::RecExpr<HalideLang>,
    // XXX: this doesn't need to be a hashmap
    pub instructions: HashMap<usize, egg::Pattern<HalideLang>>,
}

pub fn run(args: cli::Args) -> anyhow::Result<Isa> {
    if args.generator_types {
        // generate type metadata files from a generator file
        // this way, we have access to the types of input / output buffers
        for file in &args.input {
            HalideGeneratorParser::output_json(file)?;
        }

        // return early
        return Ok(Isa::default());
    }

    let files_w_types = if args.types.is_empty() {
        args.input.iter().map(|f| (f, None)).collect_vec()
    } else if args.input.len() == args.types.len() {
        args.input
            .iter()
            .zip(args.types.iter().map(Some))
            .collect_vec()
    } else {
        anyhow::bail!("Don't have types for all inputs")
    };

    let asts = files_w_types
        .iter()
        .map(|(file, types)| {
            let func_sig = HalideGeneratorParser::read_json(types.as_ref().unwrap_or(file))
                .with_context(|| format!("Don't have buffer types: {file:?}"))?;
            StmtParser::parse_file(file)
                .inspect(|ast| {
                    if args.output_parse() {
                        ast.stdout();
                        println!()
                    }
                })
                .map(Inline::do_pass_default)
                .map(UniqueIdents::do_pass_default)
                .map(|ast| TypeAnnotator::from(func_sig).do_pass(ast))
                .map(InsertCasts::do_pass_default)
                .map(NumberNodes::do_pass_default)
                .inspect(|ast| {
                    if args.output_types() {
                        ast.stdout();
                        println!()
                    }
                })
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    // gather expressions from all the asts
    let mut miner = MineExpressions::default();
    for ast in &asts {
        miner.mine_module(ast);
    }

    // add all of the expressions into the instruction selector
    let mut instr_sel = Instructions::default();
    miner.into_iter().for_each(|expr| {
        instr_sel.add_expr(expr.clone());
    });

    let instr_sel = if let Some(path) = &args.load {
        println!("Loading instructions from {path:?}...");
        instr_sel.load(path)?
    } else if args.learn {
        // run anti-unification to discover patterns (instructions)
        println!("Learning instructions...");
        let instrs = instr_sel.learn();

        // serialize the instructions to a file
        if let Some(path) = &args.save {
            instrs.serialize(path)?;
        }

        instrs
    } else {
        return Err(anyhow!("Need either `--learn` or `--load <path>`"));
    };

    let prog = instr_sel.apply();
    if args.output_raw() {
        println!("== Raw Egg Program (before mapping back to Halide) ==");
        println!("{}", prog.pretty(80));
    }

    // map expressions back into their program. we have a single `RecExpr` that
    // contains expressions from all of the asts that we were given initially
    let mut rewriter = Rewrite::new(prog.clone())?;
    asts.into_iter()
        .map(|ast| rewriter.do_pass(ast))
        .map(RemoveCasts::do_pass_default)
        .map(LiftExpressions::do_pass_default)
        .for_each(|ast| {
            if args.output_instr() {
                ast.stdout();
                println!()
            }
        });

    // print out a histogram of the instructions that were actually used
    Ok(Isa {
        expressions: prog,
        instructions: instr_sel.instructions().collect(),
    })
}
