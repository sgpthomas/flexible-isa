use flexible_isa::{cli, run, HalideExprOp, InstructionSelect};
use itertools::Itertools;

fn main() -> anyhow::Result<()> {
    let _ = env_logger::builder().try_init();

    let isa = run(cli::cli())?;

    let instr_hist = InstructionSelect::from_recexpr(&isa.expressions);

    // print out expressions that haven't been mapped to instructions
    instr_hist
        .iter()
        .filter_map(|(op, count)| match op {
            HalideExprOp::FunCall(_)
            | HalideExprOp::Instruction(_)
            | HalideExprOp::Named(_)
            | HalideExprOp::Babble(_)
            | HalideExprOp::PatternVar(_)
            | HalideExprOp::Cast(_) => None,
            op => Some((op, count)),
        })
        .sorted_by_key(|(op, count)| (*count, *op))
        .for_each(|(op, count)| println!("{op}: occurred {count} times"));

    println!("==========\n");

    // print out used instructions
    instr_hist
        .iter()
        .filter_map(|(op, count)| {
            if let HalideExprOp::Instruction(i) = op {
                Some((i, count))
            } else {
                None
            }
        })
        // sort first by count, then by index
        .sorted_by_key(|(i, count)| (*count, *i))
        .for_each(|(i, count)| {
            println!(
                "instruction {i}: occurred {count} times\n{}",
                isa.instructions[&((*i) as usize)].pretty(80)
            );
        });

    Ok(())
}
