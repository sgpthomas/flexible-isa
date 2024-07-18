#[allow(unused)]
use halide_ir::Printer;
#[allow(unused)]
use instruction_select::Simplify;

use halide_ir::{MineExpressions, StmtParser};
use instruction_select::Instructions;

use crate::halide_ir::{InsertCasts, TypeAnnotator};

mod cli;
mod halide_ir;
mod instruction_select;
mod rewrite_recexpr;

fn main() -> anyhow::Result<()> {
    let _ = env_logger::builder().try_init();

    let args = cli::cli();

    let ast = StmtParser::parse_file(&args.input)?;

    let ast = TypeAnnotator::default().check_module(ast);
    let ast = InsertCasts.cast_module(ast);

    ast.stdout();
    println!();

    // let mut type_annot = TypeAnnotator::default();
    // let ast_tmp = ast.clone();
    // type_annot.check_module(&ast_tmp);

    // let ast = type_annot.annotate(ast);

    let mut exprs = MineExpressions::default();
    exprs.mine_module(&ast);

    let mut inst_sel = Instructions::default();

    exprs.into_iter().for_each(|expr| {
        let rec_expr = egg::RecExpr::from(expr.clone());
        inst_sel.add_expr(&rec_expr);
        println!("adding {}", rec_expr.pretty(80));
        println!("--");
    });

    // run anti-unification to discover patterns
    let instrs = inst_sel.anti_unify();

    println!("== Learned Patterns ==");
    instrs.instructions().for_each(|pat| {
        println!("{}", pat.pretty(80));
    });

    println!("== Final Program ==");
    println!("{}", instrs.apply().pretty(80));

    Ok(())
}
