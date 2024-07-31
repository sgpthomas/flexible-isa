#[allow(unused)]
use halide_ir::Printer;
#[allow(unused)]
use instruction_select::Simplify;

use halide_ir::{Inline, InsertCasts, MineExpressions, StmtParser, TypeAnnotator, Visitor};
use instruction_select::{InstructionSelect, Instructions};

#[doc(hidden)]
#[allow(clippy::single_component_path_imports)]
use derive_deftly;

use crate::halide_ir::HalideGeneratorParser;

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
            HalideGeneratorParser::write_json(file)?;
        }

        // return early
        return Ok(());
    }

    let asts = args
        .input
        .iter()
        .map(|file| {
            let func_sig = HalideGeneratorParser::read_json(file)?;
            StmtParser::parse_file(file)
                .map(|ast| Inline::default().do_pass(ast))
                .map(|ast| TypeAnnotator::from(func_sig).do_pass(ast))
                // .inspect(|ast| println!("{ast:#?}"))
                .map(|ast| InsertCasts.do_pass(ast))
                .inspect(|ast| {
                    ast.stdout();
                    println!()
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
            let rec_expr = egg::RecExpr::from(expr.clone());
            inst_sel.add_expr(&rec_expr);
        });

        // run anti-unification to discover patterns
        println!("Learning instructions...");
        let instrs = inst_sel.anti_unify();

        println!("== Learned Patterns ==");
        instrs.instructions().for_each(|pat| {
            println!(
                "{}: {} => {}",
                pat.name,
                pat.searcher.get_pattern_ast().unwrap().pretty(80),
                pat.applier.get_pattern_ast().unwrap().pretty(80)
            );
        });

        println!("== Final Program ==");
        let prog = instrs.apply();
        println!("{}", prog.pretty(80));
        println!("{}", InstructionSelect::from_recexpr(&prog));

        return Ok(());
    }

    Ok(())
}
