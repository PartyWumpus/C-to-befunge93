#![expect(incomplete_features)]
#![feature(deref_patterns)] // incomplete feature

use c_to_befunge::ir::IRTopLevel;
use c_to_befunge::{args::ARGS, c_compiler::FileBuilder, codegen::CodeGen, ir::print_ir, passes};
use std::collections::HashMap;
use std::{fs, process};

// made in build.rs
const STDLIB: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/stdlib.bin"));

fn main() {
    let mut files = vec![];
    // TODO: once function pointers actually work this won't work
    // probably just add all function pointers to the list of roots?
    // maybe something smarter is possible
    let mut roots = vec!["main".to_owned()];

    if ARGS.filenames.is_empty() {
        eprintln!("No input files provided, try --help");
        process::exit(1);
    }

    let mut included = vec!["befunge_libc/stdlib"];
    let mut x: Vec<&str> = ARGS.include_dirs.iter().map(AsRef::as_ref).collect();
    included.append(&mut x);

    for filename in &ARGS.filenames {
        let c_source = match fs::read_to_string(filename) {
            Err(err) => {
                eprintln!("File '{filename}' failed to open: {err}");
                process::exit(1);
            }
            Ok(a) => a,
        };
        if ARGS.verbose {
            println!("-- C SOURCE (pre preprocessor)");
            println!("{c_source}\n");
        }

        let iquoted = if let Some(path) = Path::new(filename).parent() {
            vec![path.to_str().unwrap()]
        } else {
            vec![]
        };

        let program = match FileBuilder::parse_c(c_source.as_bytes(), filename, &included, &iquoted)
        {
            Err(err) => {
                if !ARGS.silent {
                    err.print();
                }
                process::exit(1);
            }
            Ok(x) => x,
        };

        files.push(program);
    }

    // TODO: add an option to compile the stdlib now instead of using prebuilt copy
    //let (stdlib_files, stdlib_roots) = compile_stdlib(&BEFUNGE_LIBC);
    let (stdlib_files, stdlib_roots): (
        Vec<(HashMap<Box<str>, IRTopLevel>, Vec<IRTopLevel>)>,
        Vec<String>,
    ) = { postcard::from_bytes(STDLIB).unwrap() };

    files.extend(stdlib_files);
    roots.extend(stdlib_roots);

    let mut program = passes::the_linkening(files, roots);

    if ARGS.verbose {
        println!("\n-- IR, (post linking, pre optimizations)");
        print_ir(&program);
    }

    // Mandatory passes
    passes::remove_pseudos(&mut program);
    passes::stack_size_reducer(&mut program);
    let function_map = passes::function_id_mapping(&program);

    if ARGS.verbose {
        println!("\n-- IR, (post optimizations)");
        print_ir(&program);
        println!("function mappings: {function_map:?}");
    }

    if ARGS.verbose {
        println!("\n-- befunge begin");
    }

    let out = CodeGen::compile_program(program, function_map);

    if !ARGS.quiet && ARGS.outfile.is_none() {
        for line in out.clone() {
            println!("{line}");
        }
    }

    if let Some(filename) = &ARGS.outfile {
        write_each(filename, out).unwrap();
    }
}

use std::fs::File;
use std::io::Write;
use std::path::Path;

fn write_each(
    path: impl AsRef<Path>,
    items: impl IntoIterator<Item = impl AsRef<[u8]>>,
) -> std::io::Result<()> {
    let mut file = File::create(path)?;
    for i in items {
        file.write_all(i.as_ref())?;
        file.write_all(b"\n")?;
    }
    // Surface any I/O errors that could otherwise be swallowed when
    // the file is closed implicitly by being dropped.
    file.sync_all()
}

// FIXME: reorganise __asm__ so all [bstack]'s are loaded first
// TODO: use seperate global counter for global values
