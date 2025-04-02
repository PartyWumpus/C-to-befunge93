#![allow(
    clippy::needless_pass_by_ref_mut,
    clippy::needless_raw_string_hashes,
    clippy::needless_raw_strings,
    clippy::unused_self,
    clippy::struct_excessive_bools,
    clippy::too_many_lines
)]
#![warn(clippy::clone_on_ref_pointer)]

use c_compiler::FileBuilder;
use clap::Parser;
use codegen::CodeGen;
use ir::print_ir;
use passes::{
    function_id_mapping_pass, pseudo_removal_pass, sort_functions_pass, stack_size_reducer_pass,
    the_linkening,
};
use std::process;
use walkdir::WalkDir;

mod builder;
mod c_compiler;
mod codegen;
mod ir;
mod passes;

#[derive(Parser, Debug)]
#[command(about="A C compiler that outputs befunge93 instead of assembly.", long_about = None)]
struct Args {
    /// File to compile
    filename: String,

    /// Print extra info about compilation
    #[arg(short, long)]
    verbose: bool,

    /// Don't print the output program
    #[arg(short, long)]
    quiet: bool,

    /// Don't output any errors
    #[arg(short, long)]
    silent: bool,

    /// File to write program to
    #[arg(short, long)]
    outfile: Option<String>,

    /// Add preprocessor info to the bottom for `BefunExec`
    #[arg(short, long)]
    preprocessor_info: bool,
}

fn main() {
    let args = Args::parse();
    let c_source = std::fs::read_to_string(&args.filename).expect("Unable to read input file");
    if args.verbose {
        println!("-- C SOURCE (pre preprocessor)");
        println!("{c_source}\n");
    }

    // TODO: support multiple user files
    let program = match FileBuilder::parse_c(&args.filename, &args) {
        Err(err) => {
            if !args.silent {
                println!("{err}");
            }
            process::exit(1);
        }
        Ok(x) => x,
    };
    if args.verbose {
        println!("\n-- IR, (pre linking)");
        print_ir(&program);
    }

    let mut files = vec![program];

    // TODO: add caching so the entire lib isn't compiled every time
    for entry in WalkDir::new("befunge_libc") {
        // TODO: handle FS errors at least a little bit
        let entry = entry.unwrap();
        if entry.file_type().is_file() {
            if let Some(ext) = entry.path().extension() {
                if ext == "c" {
                    files.push(match FileBuilder::parse_c(entry.path(), &args) {
                        Err(err) => {
                            if !args.silent {
                                println!("{err}");
                            }
                            process::exit(1);
                        }
                        Ok(x) => x,
                    });
                }
            }
        }
    }

    // TODO: strip out unused functions
    let mut program = the_linkening(files);

    if args.verbose {
        println!("\n-- IR, (post linking, pre optimizations)");
        print_ir(&program);
    }

    // Mandatory passes
    program = sort_functions_pass(program); // put main function at the top
    pseudo_removal_pass(&mut program);
    stack_size_reducer_pass(&mut program);
    let function_map = function_id_mapping_pass(&program);

    if args.verbose {
        println!("\n-- IR, (post optimizations)");
        print_ir(&program);
        println!("function mappings: {function_map:?}");
    }

    if args.verbose {
        println!("\n-- befunge begin");
    }

    let out = CodeGen::compile_program(program, function_map, &args);

    if !args.quiet && args.outfile.is_none() {
        for line in out.clone() {
            println!("{line}");
        }
    }

    if let Some(filename) = args.outfile {
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

// FIXME: Fix first function having a stack size greater than x. Just increment by stack_frame_size
// at start :)
// FIXME: reorganise __asm__ so all [bstack]'s are loaded first
// TODO: use seperate global counter for global values
//
// NOTE: remember, linking can only use values without . in the name
//
// NOTE: Some custom printing thing is going to be needed for every value that isn't signed int(/long?)
// NOTE: When implementing unsigned ints, gonna need to have custom impls for some things
//        likely mult, divide and modulo (as befunge ops are signed).
