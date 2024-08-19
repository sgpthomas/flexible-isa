use std::{fs, path::PathBuf};

use argh::FromArgs;

/// manage bisa config
#[derive(argh::FromArgs)]
#[argh(subcommand, name = "config")]
pub struct ConfigCommand {
    #[argh(subcommand)]
    sub: ConfigAction,
}

#[derive(argh::FromArgs)]
#[argh(subcommand)]
pub enum ConfigAction {
    Init(InitCommand),
}

/// initialize the bisa config
#[derive(argh::FromArgs)]
#[argh(subcommand, name = "init")]
pub struct InitCommand {
    /// relative path to halide installation
    #[argh(option, long = "halide")]
    halide_path: PathBuf,

    /// flexible isa root location
    #[argh(option)]
    root: PathBuf,
}

#[derive(serde::Serialize, Debug)]
struct InitConfig {
    root: PathBuf,
    halide: HalideConfig,
}

#[derive(serde::Serialize, Debug)]
struct HalideConfig {
    root: PathBuf,
}

impl InitConfig {
    fn new(root: PathBuf, halide_root: PathBuf) -> Self {
        Self {
            root,
            halide: HalideConfig { root: halide_root },
        }
    }
}

impl ConfigCommand {
    fn init(&self, driver: &fud_core::Driver, cmd: &InitCommand) -> anyhow::Result<()> {
        let config_path = fud_core::config::config_path(&driver.name);

        let halide_abs_path = cmd.halide_path.canonicalize()?;
        let root_path = cmd.root.canonicalize()?;

        let init_config = InitConfig::new(root_path, halide_abs_path);
        let toml_string = toml::to_string_pretty(&init_config)?;

        fs::write(&config_path, toml_string)?;

        println!("Wrote {config_path:?}");
        Ok(())
    }

    fn run(&self, driver: &fud_core::Driver) -> anyhow::Result<()> {
        match &self.sub {
            ConfigAction::Init(cmd) => self.init(driver, cmd),
        }
    }
}

pub enum BisaCli {
    Config(ConfigCommand),
}

impl fud_core::cli::CliExt for BisaCli {
    fn run(&self, driver: &fud_core::Driver) -> anyhow::Result<()> {
        match &self {
            BisaCli::Config(cmd) => cmd.run(driver),
        }
    }

    fn inner_command_info() -> Vec<argh::CommandInfo> {
        vec![argh::CommandInfo {
            name: "config",
            description: "manage bisa config",
        }]
    }

    fn inner_redact_arg_values() -> Vec<(&'static str, fud_core::cli::RedactArgFn)> {
        vec![("config", ConfigCommand::redact_arg_values)]
    }

    fn inner_from_args() -> Vec<(&'static str, fud_core::cli::FromArgFn<Self>)> {
        vec![("config", |command_name, args| {
            ConfigCommand::from_args(command_name, args).map(Self::Config)
        })]
    }
}
