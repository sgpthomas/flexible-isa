use std::sync::Arc;

pub trait RecExprRewriter<L>
where
    L: egg::Language,
{
    /// Apply a set of `egg::Rewrite` rules destructively.
    fn destructively_rewrite<N>(
        &self,
        rewrites: impl IntoIterator<Item = egg::Rewrite<L, N>>,
    ) -> Self
    where
        N: egg::Analysis<L> + Default + 'static;
}

/// An `egg::Applier` that wraps another `egg::Applier`. Before calling the
/// underlying applier, we remove everything from the matched `eclass`. This makes
/// `egraph` rewriting behave like traditional destructive rewriting. You should
/// only use this on an e-graph that has a single node per `eclass`. Otherwise you
/// might get weird results.
///
/// This also doesn't work for patterns that don't match any variables. Not totally
/// sure why that's the case. For now, I just check if there are variables, and
/// disable the destructive behavior.
struct DestructiveApplier<L, N>
where
    L: egg::Language,
    N: egg::Analysis<L>,
{
    applier: Arc<dyn egg::Applier<L, N> + Sync + Send>,
}

impl<L, N> egg::Applier<L, N> for DestructiveApplier<L, N>
where
    L: egg::Language,
    N: egg::Analysis<L>,
{
    fn apply_one(
        &self,
        egraph: &mut egg::EGraph<L, N>,
        eclass: egg::Id,
        subst: &egg::Subst,
        searcher_ast: Option<&egg::PatternAst<L>>,
        rule_name: egg::Symbol,
    ) -> Vec<egg::Id> {
        // HACK: Don't remove nodes if we aren't matching any variables.
        if !self.applier.vars().is_empty() {
            egraph[eclass].nodes = vec![];
        }

        // call the underlying applier. now that we have removed the root of the pattern
        // match, this behaves like a destructive applier
        self.applier
            .apply_one(egraph, eclass, subst, searcher_ast, rule_name)
    }
}

impl<L> RecExprRewriter<L> for egg::RecExpr<L>
where
    L: egg::Language + 'static + std::fmt::Display,
{
    fn destructively_rewrite<N>(
        &self,
        rewrites: impl IntoIterator<Item = egg::Rewrite<L, N>>,
    ) -> Self
    where
        N: egg::Analysis<L> + Default + 'static,
    {
        let mut egraph: egg::EGraph<L, N> = egg::EGraph::default();

        let root = egraph.add_expr(self);
        egraph.rebuild();

        for mut rw in rewrites {
            rw.applier = Arc::new(DestructiveApplier {
                applier: rw.applier,
            });

            let matches = rw.search(&egraph);
            rw.apply(&mut egraph, &matches);
            egraph.rebuild();
        }

        // egraph.dot().to_pdf("simple.pdf").unwrap();

        let extractor = egg::Extractor::new(&egraph, egg::AstSize);
        let (_, best) = extractor.find_best(root);
        best
    }
}
