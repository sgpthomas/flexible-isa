use std::{
    ffi::OsStr,
    fmt::{self, Formatter},
    io::Write,
    path::Path,
    time::Duration,
};

use closure::closure;
use indicatif::{ProgressBar, ProgressStyle};
use itertools::Itertools;

pub trait PrefixLines {
    fn prefix_lines<D>(self, prefix: D) -> Self
    where
        D: std::fmt::Display;
}

impl PrefixLines for String {
    fn prefix_lines<D>(self, prefix: D) -> Self
    where
        D: std::fmt::Display,
    {
        self.lines().join(&format!("\n{}", prefix))
    }
}

#[allow(unused)]
pub trait IntoNamedDot<'a, L: egg::Language, N: egg::Analysis<L>> {
    fn named_dot(&'a self) -> NamedDot<'a, L, N>;
}

impl<'a, L: egg::Language, N: egg::Analysis<L>> IntoNamedDot<'a, L, N> for egg::EGraph<L, N> {
    fn named_dot(&'a self) -> NamedDot<'a, L, N> {
        NamedDot {
            egraph: self,
            config: vec![],
            use_anchors: true,
        }
    }
}

pub struct NamedDot<'a, L: egg::Language, N: egg::Analysis<L>> {
    pub(crate) egraph: &'a egg::EGraph<L, N>,
    /// A list of strings to be output top part of the dot file.
    pub config: Vec<String>,
    /// Whether or not to anchor the edges in the output.
    /// True by default.
    pub use_anchors: bool,
}

#[allow(unused)]
impl<'a, L, N> NamedDot<'a, L, N>
where
    L: egg::Language + std::fmt::Display,
    N: egg::Analysis<L>,
{
    /// Writes the `Dot` to a .dot file with the given filename.
    /// Does _not_ require a `dot` binary.
    pub fn to_dot(&self, filename: impl AsRef<Path>) -> std::io::Result<()> {
        let mut file = std::fs::File::create(filename)?;
        write!(file, "{}", self)
    }

    /// Adds a line to the dot output.
    /// Indentation and a newline will be added automatically.
    pub fn with_config_line(mut self, line: impl Into<String>) -> Self {
        self.config.push(line.into());
        self
    }

    /// Set whether or not to anchor the edges in the output.
    pub fn with_anchors(mut self, use_anchors: bool) -> Self {
        self.use_anchors = use_anchors;
        self
    }

    /// Renders the `Dot` to a .png file with the given filename.
    /// Requires a `dot` binary to be on your `$PATH`.
    pub fn to_png(&self, filename: impl AsRef<Path>) -> std::io::Result<()> {
        self.run_dot(["-Tpng".as_ref(), "-o".as_ref(), filename.as_ref()])
    }

    /// Renders the `Dot` to a .svg file with the given filename.
    /// Requires a `dot` binary to be on your `$PATH`.
    pub fn to_svg(&self, filename: impl AsRef<Path>) -> std::io::Result<()> {
        self.run_dot(["-Tsvg".as_ref(), "-o".as_ref(), filename.as_ref()])
    }

    /// Renders the `Dot` to a .pdf file with the given filename.
    /// Requires a `dot` binary to be on your `$PATH`.
    pub fn to_pdf(&self, filename: impl AsRef<Path>) -> std::io::Result<()> {
        self.run_dot(["-Tpdf".as_ref(), "-o".as_ref(), filename.as_ref()])
    }

    /// Invokes `dot` with the given arguments, piping this formatted
    /// `Dot` into stdin.
    pub fn run_dot<S, I>(&self, args: I) -> std::io::Result<()>
    where
        S: AsRef<OsStr>,
        I: IntoIterator<Item = S>,
    {
        self.run("dot", args)
    }

    /// Invokes some program with the given arguments, piping this
    /// formatted `Dot` into stdin.
    ///
    /// Can be used to run a different binary than `dot`:
    /// ```no_run
    /// # use egg::*;
    /// # let mut egraph: EGraph<SymbolLang, ()> = Default::default();
    /// egraph.dot().run(
    ///     "/path/to/my/dot",
    ///     &["arg1", "-o", "outfile"]
    /// ).unwrap();
    /// ```
    pub fn run<S1, S2, I>(&self, program: S1, args: I) -> std::io::Result<()>
    where
        S1: AsRef<OsStr>,
        S2: AsRef<OsStr>,
        I: IntoIterator<Item = S2>,
    {
        use std::process::{Command, Stdio};
        let mut child = Command::new(program)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .spawn()?;
        let stdin = child.stdin.as_mut().expect("Failed to open stdin");
        write!(stdin, "{}", self)?;
        match child.wait()?.code() {
            Some(0) => Ok(()),
            Some(e) => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("dot program returned error code {}", e),
            )),
            None => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "dot program was killed by a signal",
            )),
        }
    }

    // gives back the appropriate label and anchor
    fn edge(&self, i: usize, len: usize) -> (String, String) {
        assert!(i < len);
        let s = |s: &str| s.to_string();
        if !self.use_anchors {
            return (s(""), format!("label={}", i));
        }
        match (len, i) {
            (1, 0) => (s(""), s("")),
            (2, 0) => (s(":sw"), s("")),
            (2, 1) => (s(":se"), s("")),
            (3, 0) => (s(":sw"), s("")),
            (3, 1) => (s(":s"), s("")),
            (3, 2) => (s(":se"), s("")),
            (_, _) => (s(""), format!("label={}", i)),
        }
    }
}

impl<'a, L, N> std::fmt::Display for NamedDot<'a, L, N>
where
    L: egg::Language + std::fmt::Display,
    N: egg::Analysis<L>,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "digraph egraph {{")?;

        // set compound=true to enable edges to clusters
        writeln!(f, "  compound=true")?;
        writeln!(f, "  clusterrank=local")?;

        for line in &self.config {
            writeln!(f, "  {}", line)?;
        }

        // define all the nodes, clustered by eclass
        for class in self.egraph.classes() {
            writeln!(f, "  subgraph cluster_{} {{", class.id)?;
            writeln!(f, "    style=dotted")?;
            writeln!(f, "    label=\"{}\"", class.id)?;
            writeln!(f, "    fontsize=9")?;
            for (i, node) in class.iter().enumerate() {
                writeln!(f, "    {}.{}[label = \"{}\"]", class.id, i, node)?;
            }
            writeln!(f, "  }}")?;
        }

        for class in self.egraph.classes() {
            for (i_in_class, node) in class.iter().enumerate() {
                let mut arg_i = 0;
                node.try_for_each(|child| {
                    // write the edge to the child, but clip it to the eclass with lhead
                    let (anchor, label) = self.edge(arg_i, node.len());
                    let child_leader = self.egraph.find(child);

                    if child_leader == class.id {
                        writeln!(
                            f,
                            // {}.0 to pick an arbitrary node in the cluster
                            "  {}.{}{} -> {}.{}:n [lhead = cluster_{}, {}]",
                            class.id, i_in_class, anchor, class.id, i_in_class, class.id, label
                        )?;
                    } else {
                        writeln!(
                            f,
                            // {}.0 to pick an arbitrary node in the cluster
                            "  {}.{}{} -> {}.0 [lhead = cluster_{}, {}]",
                            class.id, i_in_class, anchor, child, child_leader, label
                        )?;
                    }
                    arg_i += 1;
                    Ok(())
                })?;
            }
        }

        write!(f, "}}")
    }
}

#[allow(unused)]
pub fn count_combinations<N: TryInto<u64>>(n: N, r: u64) -> u64
where
    <N as TryInto<u64>>::Error: std::fmt::Debug,
{
    let n = n.try_into().unwrap();
    if r > n {
        0
    } else {
        (1..=r).fold(1, |acc, val| acc * (n - val + 1) / val)
    }
}

pub fn progress_style() -> ProgressStyle {
    ProgressStyle::with_template("[eta:{eta}] {bar:40.cyan/blue} ({percent}%) {msg}")
        .unwrap()
        .progress_chars("#*-")
}

pub fn progress_bar<N: TryInto<u64>>(len: N) -> ProgressBar
where
    <N as TryInto<u64>>::Error: std::fmt::Debug,
{
    ProgressBar::new(len.try_into().unwrap()).with_style(progress_style())
}

pub fn pb_runner<'a, L, N, D, I, S>(
    runner: egg::Runner<L, N, D>,
    msg: S,
    rewrites: I,
) -> egg::Runner<L, N, D>
where
    L: egg::Language + 'a,
    N: egg::Analysis<L> + 'a,
    D: egg::IterationData<L, N>,
    I: IntoIterator<Item = &'a egg::Rewrite<L, N>>,
    S: std::fmt::Display + Clone + 'static,
{
    let spinner = std::rc::Rc::new(
        ProgressBar::new_spinner()
            .with_message(format!("{}", msg))
            .with_style(
                ProgressStyle::with_template("{spinner:.cyan/green} {msg}")
                    .unwrap()
                    .tick_chars(".oO@*✓"),
            ),
    );
    spinner.enable_steady_tick(Duration::from_millis(100));
    let runner = runner
        .with_hook(closure!(clone msg, clone spinner, |runner| {
            spinner.set_message(
                format!(
                    "{msg}: {}",
                    runner_stats_msg(runner)
                ));
            Ok(())
        }))
        .run(rewrites);
    spinner.finish_with_message(format!(
        "{}: {} ({})",
        msg,
        runner
            .stop_reason
            .as_ref()
            .map(|reason| format!("{:?}", reason))
            .unwrap_or("Unknown".to_string()),
        runner_stats_msg(&runner)
    ));
    println!();
    runner
}

fn runner_stats_msg<L, N, D>(runner: &egg::Runner<L, N, D>) -> String
where
    L: egg::Language,
    N: egg::Analysis<L>,
    D: egg::IterationData<L, N>,
{
    format!(
        "iter: {}, eclasses: {}, enodes: {}",
        runner.iterations.len(),
        runner.egraph.number_of_classes(),
        runner.egraph.total_number_of_nodes(),
    )
}
