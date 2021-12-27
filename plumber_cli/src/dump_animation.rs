use clap::Parser;

use plumber_core::{
    fs::{FileSystem, GamePathBuf},
    model::Model,
};

#[derive(Parser)]
pub struct DumpAnimation {
    #[clap(short, long)]
    mdl_path: String,
    #[clap(short, long)]
    anim_name: Option<String>,
    #[clap(short, long)]
    bones: bool,
}

pub fn dump_animation(opts: DumpAnimation, file_system: &FileSystem) {
    let file_system = file_system.open().unwrap();

    let model = Model::read(&GamePathBuf::from(opts.mdl_path), &file_system).unwrap();
    let verified = model.verify().unwrap();

    if opts.bones {
        for bone in verified.bones().unwrap() {
            eprintln!("{:#?}", bone);
        }
    }

    for res in verified.animations().unwrap() {
        let animation = res.unwrap();

        if let Some(filter) = &opts.anim_name {
            if animation.name != filter {
                continue;
            }
        }

        eprintln!("{:#?}", animation);
    }
}
