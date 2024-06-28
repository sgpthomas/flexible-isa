use crate::halide_ir::StmtParser;

mod cli;
mod halide_ir;

fn main() {
    let args = cli::cli();

    let ast = StmtParser::parse_file(&args.input);
    println!("{ast:#?}");
}
