use crate::halide_ir::{Printer, StmtParser};

mod cli;
mod halide_ir;

fn main() -> anyhow::Result<()> {
    let args = cli::cli();

    let ast = StmtParser::parse_file(&args.input)?;
    ast.stdout();
    Ok(())
}
