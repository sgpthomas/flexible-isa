use std::collections::HashMap;

use anyhow::Context;
#[allow(unused)]
use halide_ir::Printer;
#[allow(unused)]
use instruction_select::Simplify;

use halide_ir::{
    Inline, InsertCasts, LiftExpressions, MineExpressions, NumberNodes, RemoveCasts, Rewrite,
    StmtParser, TypeAnnotator, UniqueIdents, Visitor,
};
use instruction_select::{InstructionSelect, Instructions};

#[doc(hidden)]
#[allow(clippy::single_component_path_imports)]
use derive_deftly;
use itertools::Itertools;

use crate::{halide_ir::HalideGeneratorParser, instruction_select::HalideExprOp};

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

    if !args.dont_learn {
        // gather expressions from all the asts
        let mut miner = MineExpressions::default();
        for ast in &asts {
            miner.mine_module(ast);
        }

        // perform instruction selection
        let mut inst_sel = Instructions::default();

        miner.into_iter().for_each(|expr| {
            inst_sel.add_expr(expr.clone());
        });

        // run anti-unification to discover patterns
        println!("Learning instructions...");
        let instrs = inst_sel.anti_unify();

        let instr_map: HashMap<usize, egg::Pattern<babble::AstNode<HalideExprOp>>> =
            instrs.instructions().collect();

        println!("== Final Program ==");
        let prog = instrs.apply();
        // println!("{}", prog.pretty(80));

        // map expressions back into their program
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

        println!("== Used instructions ==");
        let instr_hist = InstructionSelect::from_recexpr(&prog);
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

        return Ok(());
    }

    Ok(())
}
