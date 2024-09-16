//! Use babble to learn common patterns in a list of expressions.

use std::{fs::File, mem, path::Path, time::Duration};

use anyhow::Context;
use babble::Teachable;
use itertools::Itertools;

#[allow(unused)]
use crate::utils::IntoNamedDot;
use crate::{
    halide_ir::ast::{self, Instr},
    instruction_select::HalideEqualities,
    utils,
};

use super::{
    extract::minimal_isa::IntoBestIsa, lang::HalideExprOp, BestIsa, HalideLang, Init,
    InstructionState, Learned,
};

pub type LibraryPattern = egg::Pattern<babble::AstNode<HalideExprOp>>;

pub struct Instructions<S: InstructionState> {
    pub egraph: egg::EGraph<babble::AstNode<HalideExprOp>, ()>,
    pub(super) roots: Vec<egg::Id>,
    pub(super) state: S,
}

impl Default for Instructions<Init> {
    fn default() -> Self {
        Self {
            egraph: egg::EGraph::default(),
            roots: Vec::default(),
            state: Init,
        }
    }
}

/// Replace every identifier in a partial expression with a hole
fn replace_idents(
    au: &babble::PartialExpr<HalideExprOp, egg::Var>,
) -> babble::PartialExpr<HalideExprOp, egg::Var> {
    /// Keep track of how many holes we have created, so that we can give
    /// the holes distinct names
    fn f(
        au: &babble::PartialExpr<HalideExprOp, egg::Var>,
        i: &mut u32,
    ) -> babble::PartialExpr<HalideExprOp, egg::Var> {
        match au {
            babble::PartialExpr::Node(node)
                if matches!(node.operation(), HalideExprOp::Ident(_)) =>
            {
                let hole = babble::PartialExpr::Hole(egg::Var::from_u32(*i));
                *i += 1;
                hole
            }
            babble::PartialExpr::Node(node) => {
                let (op, args) = node.as_parts();
                babble::PartialExpr::Node(babble::AstNode::new(
                    op.clone(),
                    args.iter().map(|a| f(a, i)),
                ))
            }
            x @ babble::PartialExpr::Hole(_) => x.clone(),
        }
    }

    f(au, &mut (au.num_holes() as u32))
}

impl Instructions<Init> {
    pub fn add_expr<R>(&mut self, expr: R)
    where
        R: Into<egg::RecExpr<HalideLang>>,
    {
        let root = self.egraph.add_expr(&expr.into());
        self.roots.push(root);
    }

    pub fn learn(mut self) -> Instructions<Learned> {
        // we need to rebuild the e-graph before learning a library
        self.egraph.rebuild();

        let mut learned_library = utils::wrap_spinner()
            .msg("Building library")
            .action(|| {
                babble::LearnedLibraryBuilder::default()
                    .learn_trivial(true)
                    .ban_op(HalideExprOp::Cast(vec![]))
                    .ban_op(HalideExprOp::FunCall(ast::Id::new("")))
                    .ban_op(HalideExprOp::Named(0))
                    .with_roots(self.roots.clone())
                    .with_dfta(false)
                    .build(&self.egraph)
            })
            .start();

        // I want to rewrite all instructions that contain concrete variables into
        // instructions that use pattern vars
        learned_library.for_each_anti_unification(replace_idents);

        // add an anti-unification for operations that we definitely
        // want to have instructions for
        learned_library
            .extend(HalideExprOp::essential_ops().map(|(op, typs)| op.partial_expr(typs)));

        // finds patterns that can apply in the same places, and only keeps the smaller
        // pattern. I think that this only matters when we are using rewrite rules. Haven't
        // quite gotten there yet.

        utils::wrap_spinner()
            .msg(format!("Deduplicating {} patterns", learned_library.size()))
            .action(|| {
                learned_library.deduplicate(&self.egraph);
            })
            .start();
        println!("\nReduced to {} patterns", learned_library.size());

        let Self {
            egraph,
            roots,
            state: _,
        } = self;

        // grab patterns from the learned library
        let instructions = learned_library
            .anti_unifications()
            .enumerate()
            .map(|(i, au)| (Instr(i), au.clone().into()))
            .collect();

        Instructions {
            egraph,
            roots,
            state: Learned { instructions },
        }
    }

    pub fn load(self, path: &Path) -> anyhow::Result<Instructions<Learned>> {
        let file = File::open(path).context("Loading instructions from file")?;

        let raw: Vec<(usize, String)> = serde_json::from_reader(file)?;

        let instructions: Vec<(Instr, egg::Pattern<HalideLang>)> = raw
            .into_iter()
            .map(|(i, pat)| Ok((Instr(i), pat.parse()?)))
            .collect::<anyhow::Result<_>>()?;

        let Self {
            egraph,
            roots,
            state: _,
        } = self;

        Ok(Instructions {
            egraph,
            roots,
            state: Learned { instructions },
        })
    }
}

impl Instructions<Learned> {
    /// Apply the set of learned rewrite rules to the egraph that we have
    pub fn apply(&mut self) -> egg::Id {
        println!("Performing instruction selection...");
        // extract the best program
        let mut egraph = mem::take(&mut self.egraph);
        let root = egraph.add(babble::AstNode::new(
            HalideExprOp::list(),
            self.roots.clone(),
        ));

        // run some equalities over the graph, to make some instructions unnecessary
        let runner = utils::pb_runner(
            egg::Runner::default()
                .with_egraph(egraph)
                .with_node_limit(1_000_000),
            "Running halide equalities",
            &HalideEqualities::rewrites(),
        );

        // run the instruction rewrites to perform essentially
        // instruction selection
        let instr_rewrites = self.rewrites().collect::<Vec<_>>();
        let runner = utils::pb_runner(
            egg::Runner::default()
                .with_egraph(runner.egraph)
                .with_node_limit(1_000_000)
                .with_time_limit(Duration::from_secs(60)),
            "Running instruction rules",
            &instr_rewrites,
        );

        // put the egraph back
        self.egraph = runner.egraph;

        root
    }

    pub fn minimal_isa<'a, I>(&'a self, isa: I) -> Box<dyn BestIsa<'a> + 'a>
    where
        I: IntoBestIsa<'a>,
    {
        let mut minimal_isa = isa.make(self);
        minimal_isa.select();
        minimal_isa
    }

    pub fn rewrites(&self) -> impl Iterator<Item = egg::Rewrite<HalideLang, ()>> + '_ {
        self.instructions().map(|(i, pat)| {
            let searcher: egg::Pattern<_> = pat.clone();

            let head = HalideExprOp::Instruction(i);
            let vars = pat.vars().into_iter().map(babble::PartialExpr::Hole);
            let applier_partial_expr: babble::PartialExpr<_, _> =
                babble::PartialExpr::Node(babble::AstNode::new(head, vars));
            let applier: egg::Pattern<_> = applier_partial_expr.into();

            let name = format!("instruction {i}");
            egg::rewrite!(name; searcher => applier)
        })
    }

    pub fn instructions(&self) -> impl Iterator<Item = (Instr, egg::Pattern<HalideLang>)> + '_ {
        self.state.instructions.iter().cloned()
    }

    pub fn serialize(&self, path: &Path) -> anyhow::Result<()> {
        let file = File::create(path)?;

        let value = self
            .instructions()
            .map(|(idx, pat)| (idx.0, pat.ast))
            .collect_vec();
        serde_json::to_writer_pretty(file, &value)?;

        Ok(())
    }
}
