#[allow(unused)]
use halide_ir::Printer;
#[allow(unused)]
use instruction_select::Simplify;

use halide_ir::{MineExpressions, StmtParser};
use instruction_select::Instructions;

use crate::halide_ir::{InsertCasts, TypeAnnotator};

#[doc(hidden)]
#[allow(clippy::single_component_path_imports)]
use derive_deftly;

mod cli;
mod halide_ir;
mod instruction_select;

fn main() -> anyhow::Result<()> {
    let _ = env_logger::builder().try_init();

    let args = cli::cli();

    let asts = args
        .input
        .iter()
        .map(|file| {
            StmtParser::parse_file(file)
                .map(|ast| TypeAnnotator::default().check_module(ast))
                .map(|ast| InsertCasts.cast_module(ast))
                .inspect(|ast| {
                    ast.stdout();
                    println!()
                })
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    let mut exprs = MineExpressions::default();
    for ast in &asts {
        exprs.mine_module(ast);
    }

    let mut inst_sel = Instructions::default();

    exprs.into_iter().for_each(|expr| {
        let rec_expr = egg::RecExpr::from(expr.clone());
        inst_sel.add_expr(&rec_expr);
    });

    // run anti-unification to discover patterns
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
    println!("{}", instrs.apply().pretty(80));

    Ok(())
}
