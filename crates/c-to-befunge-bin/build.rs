use c_to_befunge::compile_stdlib;
use include_dir::{Dir, include_dir};
use std::{
    env::{current_dir, set_current_dir},
    path::PathBuf,
};

// TODO: dont use include_dir here there's no point, just make it a normal dir
static BEFUNGE_LIBC: Dir = include_dir!("./befunge_libc");

fn main() {
    let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    let manifest_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    println!(
        "cargo:rerun-if-changed={}",
        manifest_dir
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("befunge_libc")
            .display()
    );

    let dir = current_dir().unwrap();
    // FIXME: dont change cwd
    set_current_dir("../../").unwrap();
    let (a, b) = compile_stdlib(&BEFUNGE_LIBC, true);
    set_current_dir(dir).unwrap();

    let out_path = out_dir.join("stdlib.bin");
    std::fs::write(&out_path, postcard::to_allocvec(&(a, b)).unwrap()).unwrap();
}
