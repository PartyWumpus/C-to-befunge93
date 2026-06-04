use clap::Parser;
use std::sync::LazyLock;

pub static ARGS: LazyLock<Args> = LazyLock::new(Args::parse);

#[derive(Parser, Debug)]
#[command(about="A C compiler that outputs befunge93 instead of assembly.", long_about = None)]
pub struct Args {
    /// File to compile
    pub filenames: Vec<String>,

    /// Directories to be searched by the preprocessor for header files
    #[arg(short = 'I')]
    pub include_dirs: Vec<String>,

    /// Print extra info about compilation
    #[arg(short, long)]
    pub verbose: bool,

    /// Don't print the output program
    #[arg(short, long)]
    pub quiet: bool,

    /// Don't output any errors
    #[arg(short, long)]
    pub silent: bool,

    /// File to write program to
    #[arg(short, long)]
    pub outfile: Option<String>,

    /// Add preprocessor info to the bottom for `BefunExec`
    #[arg(short, long)]
    pub preprocessor_info: bool,

    /// Fills stack values with zero before they're first written to
    #[arg(long)]
    pub zero_stack_before_use: bool,

    /// Disables the use of bitwise operations to speed up startup times
    #[arg(long)]
    pub disable_bitwise_ops: bool,

    /// Use Berkeley SoftFloat v3 to do floating point operations
    /// Currently barely functional, do not use
    #[arg(long)]
    pub enable_softfloat: bool,

    #[arg(short = 'O', default_value = "0")]
    pub optimization_level: u8,
}
