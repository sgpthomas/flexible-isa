use halide_ir::{MineExpressions, Printer, StmtParser};

mod cli;
mod halide_ir;
mod instruction_select;

fn main() -> anyhow::Result<()> {
    let _ = env_logger::builder().try_init();

    let args = cli::cli();

    let ast = StmtParser::parse_file(&args.input)?;

    let mut exprs = MineExpressions::default();
    for func in &ast.funcs {
        exprs.mine_func(func);
    }

    exprs.into_iter().for_each(|expr| {
        expr.stdout();
        println!();
        let bexpr = babble::Expr::from(expr.clone());
        let rec_expr = egg::RecExpr::from(bexpr);
        println!("rec_expr: {}", rec_expr.pretty(80));
    });

    Ok(())
}
