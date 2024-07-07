use fud_core::{cli, DriverBuilder};

fn main() -> anyhow::Result<()> {
    let mut bld = DriverBuilder::new("bisa");

    #[cfg(debug_assertions)]
    {
        bld.scripts_dir(manifest_dir_macros::directory_path!("scripts"));
    }

    #[cfg(not(debug_assertions))]
    {
        bld.script_files({
            const DIR: include_dir::Dir = include_dir::include_dir!("$CARGO_MANIFEST_DIR/scripts");
            DIR.files()
                .map(|file| (file.path().to_str().unwrap(), file.contents()))
                .collect()
        });
    }

    bld = bld.load_plugins();

    let driver = bld.build();
    cli::cli(&driver)
}
