#![warn(clippy::all, clippy::pedantic)]

mod dump_animation;

use dump_animation::{dump_animation, DumpAnimation};

use plumber_core::{fs::FileSystem, steam::Libraries};

use clap::Parser;

#[derive(Parser)]
#[clap(version = "0.1.0")]
struct Opts {
    #[clap(short, long)]
    app_id: u32,
    #[clap(subcommand)]
    subcommand: SubCommand,
}

#[derive(Parser)]
enum SubCommand {
    DumpAnimation(DumpAnimation),
}

fn main() {
    let opts = Opts::parse();

    let libraries = Libraries::discover().unwrap();

    let app = libraries
        .apps()
        .map(Result::unwrap)
        .find(|app| app.app_id == opts.app_id)
        .unwrap();
    let file_system = FileSystem::from_app(&app).unwrap();

    match opts.subcommand {
        SubCommand::DumpAnimation(opts) => dump_animation(opts, &file_system),
    }
}
