use c_compiler::FileBuilder;
use clap::Parser;
use codegen::CodeGen;
use include_dir::{include_dir, Dir};
use ir::print_ir;
use std::process;
use std::sync::LazyLock;

mod builder;
mod c_compiler;
mod codegen;
mod ir;
mod number_generation;
mod passes;

static ARGS: LazyLock<Args> = LazyLock::new(Args::parse);
static BEFUNGE_LIBC: Dir = include_dir!("./befunge_libc");

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

    /// Fills stack values with zero before they're first written to
    #[arg(long)]
    zero_stack_before_use: bool,

    /// Disables the use of bitwise operations to speed up startup times
    #[arg(long)]
    disable_bitwise_ops: bool,
}

fn main() {
    let c_source = std::fs::read_to_string(&ARGS.filename).expect("Unable to read input file");
    if ARGS.verbose {
        println!("-- C SOURCE (pre preprocessor)");
        println!("{c_source}\n");
    }

    // TODO: support multiple user files?
    let program = match FileBuilder::parse_c(c_source.as_bytes(), &ARGS.filename) {
        Err(err) => {
            if !ARGS.silent {
                err.print();
            }
            process::exit(1);
        }
        Ok(x) => x,
    };
    if ARGS.verbose {
        println!("\n-- IR, (pre linking)");
        print_ir(&program);
    }

    let mut files = vec![program];

    // TODO: add caching so the entire lib isn't compiled every time
    for entry in BEFUNGE_LIBC.files() {
        if let Some(ext) = entry.path().extension() {
            if ext == "c" {
                files.push(
                    match FileBuilder::parse_c(entry.contents(), entry.path().to_str().unwrap()) {
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

    // TODO: strip out unused functions
    let mut program = passes::the_linkening(files);

    if ARGS.verbose {
        println!("\n-- IR, (post linking, pre optimizations)");
        print_ir(&program);
    }

    // Mandatory passes
    passes::sort_functions(&mut program); // put main function at the top
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
//
// NOTE: remember, linking can only use values without . in the name
//
// NOTE: Some custom printing thing is going to be needed for every value that isn't signed int(/long?)
// NOTE: When implementing unsigned ints, gonna need to have custom impls for some things
//        likely mult, divide and modulo (as befunge ops are signed).
