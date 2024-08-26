use flexible_isa::{cli, run, HalideExprOp, InstructionSelect};
use itertools::Itertools;

fn main() -> anyhow::Result<()> {
    let _ = env_logger::builder().try_init();

    let isa = run(cli::cli())?;

    let instr_hist = InstructionSelect::from_recexpr(&isa.expressions);

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
        // sort first by count, then by index
        .sorted_by_key(|(i, count)| (*count, *i))
        .for_each(|(i, count)| {
            println!(
                "{i}: {count}\n{}",
                isa.instructions[&((*i) as usize)].pretty(80)
            );
        });

    Ok(())
}
