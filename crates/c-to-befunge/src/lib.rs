#![expect(incomplete_features)]
#![feature(deref_patterns)] // incomplete feature

pub mod args;
mod builder;
pub mod c_compiler;
pub mod codegen;
mod errors;
pub mod ir;
mod number_generation;
pub mod passes;
mod softfloat_files;

use include_dir::Dir;
use std::{collections::HashMap, process};

use crate::{
    args::ARGS, c_compiler::FileBuilder, ir::IRTopLevel, softfloat_files::SOFTFLOAT_FILES,
};

pub fn compile_stdlib(
    dir: &Dir,
    enable_softfloat: bool,
) -> (
    Vec<(HashMap<Box<str>, IRTopLevel>, Vec<IRTopLevel>)>,
    Vec<String>,
) {
    let mut files = vec![];
    let mut roots = vec![];

    let iquoted = vec!["./../../befunge_libc/stdlib/"];

    for entry in dir.get_dir("stdlib").expect("stdlib").files() {
        if !enable_softfloat
            && let Some(filename) = entry.path().to_str()
            && filename.contains("_bf_float")
        {
            continue;
        }
        if let Some(ext) = entry.path().extension()
            && ext == "c"
        {
            files.push(
                match FileBuilder::parse_c(
                    entry.contents(),
                    entry.path().to_str().unwrap(),
                    &[
                        "befunge_libc/stdlib",
                        "befunge_libc/softfloat/source/include",
                    ],
                    &[],
                ) {
                    Err(err) => {
                        if !ARGS.silent {
                            err.print();
                        }
                        process::exit(1);
                    }
                    Ok(x) => x,
                },
            );
        }
    }

    for entry in dir.get_dir("internal").expect("internal").files() {
        if !enable_softfloat
            && let Some(filename) = entry.path().to_str()
            && filename.contains("_bf_float")
        {
            continue;
        }
        if let Some(ext) = entry.path().extension()
            && ext == "c"
        {
            let file = match FileBuilder::parse_c(
                entry.contents(),
                entry.path().to_str().unwrap(),
                &[
                    "befunge_libc/stdlib",
                    "befunge_libc/softfloat/source/include",
                ],
                &[],
            ) {
                Err(err) => {
                    if !ARGS.silent {
                        err.print();
                    }
                    process::exit(1);
                }
                Ok(x) => x,
            };
            // TODO: this currently means we include all floating point operations
            // and all bitwise operations even if they are not used
            // need to get some machinery to sensibly keep track of this
            for func_name in file.0.keys() {
                roots.push(func_name.to_string());
            }
            files.push(file);
        }
    }

    if enable_softfloat {
        for entry in dir.get_dir("softfloat/source").expect("softfloat").files() {
            let Some(path) = entry.path().to_str() else {
                continue;
            };
            let Some(index) = path.rfind('/') else {
                continue;
            };
            let filename = &path[index + 1..];
            if SOFTFLOAT_FILES.contains(&filename) {
                files.push(
                    match FileBuilder::parse_c(
                        entry.contents(),
                        entry.path().to_str().unwrap(),
                        &[
                            "befunge_libc/stdlib",
                            "befunge_libc/softfloat_config",
                            "befunge_libc/softfloat/source/include",
                            "befunge_libc/softfloat/source/8086-SSE",
                        ],
                        &[],
                    ) {
                        Err(err) => {
                            if !ARGS.silent {
                                err.print();
                            }
                            process::exit(1);
                        }
                        Ok(x) => x,
                    },
                );
            }
        }

        for entry in dir
            .get_dir("softfloat/source/8086-SSE")
            .expect("softfloat")
            .files()
        {
            let Some(path) = entry.path().to_str() else {
                continue;
            };
            let Some(index) = path.rfind('/') else {
                continue;
            };
            let filename = &path[index + 1..];
            if SOFTFLOAT_FILES.contains(&filename) {
                files.push(
                    match FileBuilder::parse_c(
                        entry.contents(),
                        entry.path().to_str().unwrap(),
                        &[
                            "befunge_libc/stdlib",
                            "befunge_libc/softfloat_config",
                            "befunge_libc/softfloat/source/include",
                            "befunge_libc/softfloat/source/8086-SSE",
                        ],
                        &[],
                    ) {
                        Err(err) => {
                            if !ARGS.silent {
                                err.print();
                            }
                            process::exit(1);
                        }
                        Ok(x) => x,
                    },
                );
            }
        }
    }

    (files, roots)
}
