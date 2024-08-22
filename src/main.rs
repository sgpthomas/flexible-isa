use anyhow::{anyhow, Context};
#[doc(hidden)]
#[allow(clippy::single_component_path_imports)]
use derive_deftly;
#[allow(unused)]
use halide_ir::{
    HalideGeneratorParser, Inline, InsertCasts, LiftExpressions, MineExpressions, NumberNodes,
    Printer, RemoveCasts, Rewrite, StmtParser, TypeAnnotator, UniqueIdents, Visitor,
};
use instruction_select::{HalideExprOp, InstructionSelect, Instructions};
use itertools::Itertools;
use std::collections::HashMap;

mod cli;
mod halide_ir;
mod instruction_select;

fn main() -> anyhow::Result<()> {
    let _ = env_logger::builder().try_init();

    let args = cli::cli();

    if args.generator_types {
        // generate type metadata files from a generator file
        // this way, we have access to the types of input / output buffers
        for file in &args.input {
            HalideGeneratorParser::output_json(file)?;
        }

        // return early
        return Ok(());
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

    let instrs = if args.learn {
        // gather expressions from all the asts
        let mut miner = MineExpressions::default();
        for ast in &asts {
            miner.mine_module(ast);
        }

        // gather all of the expressions into the instruction selector
        let mut inst_sel = Instructions::default();
        miner.into_iter().for_each(|expr| {
            inst_sel.add_expr(expr.clone());
        });

        // run anti-unification to discover patterns (instructions)
        println!("Learning instructions...");
        let instrs = inst_sel.anti_unify();

        // serialize the instructions to a file
        if let Some(path) = &args.save {
            instrs.serialize(path)?;
        }

        instrs
    } else if let Some(path) = &args.load {
        Instructions::load(path)?
    } else {
        return Err(anyhow!(
            "Need to specify either `--learn` or `--load <file>`"
        ));
    };

    println!("== Final Program ==");
    let prog = instrs.apply();

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
    println!("== Used instructions ==");
    let instr_hist = InstructionSelect::from_recexpr(&prog);
    // XXX: this doesn't need to be a hashmap
    let instr_map: HashMap<_, _> = instrs.instructions().collect();
    println!("len: {}", instr_hist.len());
    instr_hist
        .iter()
        .filter_map(|(op, count)| {
            if let HalideExprOp::Instruction(i) = op {
                Some((i, count))
            } else {
                None
            }
        })
        .sorted_by_key(|(_, count)| *count)
        .for_each(|(i, count)| {
            println!("{i}: {count}\n{}", instr_map[&((*i) as usize)].pretty(80));
        });

    Ok(())
}
