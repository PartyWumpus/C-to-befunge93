use std::fmt;

use anstyle::{Color, Style};

use crate::c_compiler::{CSize, CType, ScopeInfo};

const KEYWORD: Style = Style::new()
    .fg_color(Some(Color::Ansi(anstyle::AnsiColor::Green)))
    .bold();
const NUM: Style = Style::new().fg_color(Some(Color::Ansi(anstyle::AnsiColor::BrightGreen)));
const TYPE: Style = Style::new().fg_color(Some(Color::Ansi(anstyle::AnsiColor::Red)));
const IDENT: Style = Style::new().fg_color(Some(Color::Ansi(anstyle::AnsiColor::BrightMagenta)));
const LABEL: Style = Style::new().fg_color(Some(Color::Ansi(anstyle::AnsiColor::Yellow)));

#[derive(Debug, Clone)]
pub enum IRValue {
    /// An offset from the current bottom of the stack
    Stack(usize),
    /// A constant immediate value, cannnot be written to
    Immediate(usize),
    /// A register, ie a position in the "zero-page"
    Register(usize),
    /// An absolute position in static memory
    Data(usize),
    /// A location to be determined Later, will be on the stack
    Psuedo { name: String, size: CSize },
    /// A location to be determined Later, will be in static memory
    StaticPsuedo {
        name: String,
        linkable: bool,
        size: CSize,
    },
    /// The current value on the top of the bstack
    /// Must be careful when using
    BefungeStack,
}

impl fmt::Display for IRValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{IDENT}")?;
        match self {
            Self::Stack(n) => write!(f, "%{n}"),
            Self::Immediate(n) => write!(f, "#{n}"),
            Self::Register(n) => write!(f, "r{n}"),
            Self::Data(n) => write!(f, "@{n}"),
            Self::Psuedo { name, .. } => write!(f, "%{name}"),
            Self::StaticPsuedo { name, .. } => write!(f, "@{name}"),
            Self::BefungeStack => write!(f, "bf-stack"),
        }?;
        write!(f, "{IDENT:#}")
    }
}

impl IRValue {
    pub const fn int(int: usize) -> Self {
        Self::Immediate(int)
    }

    pub const fn float(float: f64) -> Self {
        Self::Immediate(float.to_bits() as usize)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
pub enum IRType {
    Signed(u8),
    Unsigned(u8),
    Double,
}

impl fmt::Display for IRType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{TYPE}")?;
        match self {
            Self::Signed(size) => write!(f, "i{size}"),
            Self::Unsigned(size) => write!(f, "u{size}"),
            Self::Double => write!(f, "double"),
        }?;
        write!(f, "{TYPE:#}")
    }
}

impl IRType {
    pub fn from_ctype(value: &CType, scope: &ScopeInfo) -> Self {
        match value {
            CType::Bool => Self::Unsigned(1),
            CType::UnsignedChar => Self::Unsigned(8),
            CType::Char | CType::SignedChar => Self::Signed(8),

            CType::UnsignedShort => Self::Unsigned(16),
            CType::SignedShort => Self::Signed(16),

            CType::UnsignedInt => Self::Unsigned(32),
            CType::SignedInt => Self::Signed(32),

            CType::UnsignedLong => Self::Unsigned(64),
            CType::SignedLong => Self::Signed(64),

            CType::Double => Self::Double,

            CType::Pointer(_) => Self::Signed(64),
            CType::Array(..) | CType::ImmediateArray(..) => panic!("Arrays cannot be irtypes"),
            CType::Struct(tag_id) => panic!("Structs cannot be irtypes"),

            CType::Function(..) => panic!("functions cannot be used as concrete types"),
            CType::Void => panic!("void cannot be used as a concrete type"),
        }
    }
}

impl CType {
    pub fn sizeof(&self, scope: &ScopeInfo) -> CSize {
        match self {
            Self::Pointer(..)
            | Self::Char
            | Self::SignedChar
            | Self::SignedShort
            | Self::SignedInt
            | Self::SignedLong
            | Self::UnsignedChar
            | Self::UnsignedShort
            | Self::UnsignedInt
            | Self::UnsignedLong
            | Self::Bool
            | Self::Double => CSize::new(1).unwrap(),
            Self::Array(inner_type, size) | Self::ImmediateArray(inner_type, size) => {
                CSize::new(inner_type.sizeof(scope).get() * size.get()).unwrap()
            }
            Self::Struct(tag_id) => scope.get_struct_by_id(*tag_id).expect("struct exists").size,
            Self::Void => panic!("void is not sized"),
            Self::Function(..) => panic!("functions cannot be used as concrete types"),
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            Self::Bool => panic!("are bools signed?"),

            Self::Char
            | Self::SignedChar
            | Self::SignedShort
            | Self::SignedInt
            | Self::SignedLong => true,

            Self::UnsignedChar | Self::UnsignedShort | Self::UnsignedInt | Self::UnsignedLong => {
                false
            }

            _ => unreachable!("type {self:?} cannot be signed"),
        }
    }

    pub fn numerical_bigness(&self) -> usize {
        match self {
            Self::Bool => 1,

            Self::Char | Self::UnsignedChar | Self::SignedChar => 2,

            Self::UnsignedShort | Self::SignedShort => 3,

            Self::UnsignedInt | Self::SignedInt => 4,

            Self::SignedLong | Self::UnsignedLong => 5,

            _ => unreachable!("type {self:?} cannot be bigness compared"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum IROp {
    Return(IRValue, CSize),
    GetReturnValue(IRValue, CSize),
    GetIdOfFunction(String, IRValue),
    Call(String, Vec<(IRValue, CSize)>),
    Label(String),
    InlineBefunge(Vec<String>),
    AlwaysBranch(String),
    CondBranch(BranchType, String, IRValue),
    AddressOf(IRValue, IRValue),
    Dereference(IRValue, IRValue, CSize),
    // do i need a source and destination offset?
    Copy(IRValue, IRValue, CSize),
    Store(IRValue, IRValue, CSize),
    One(UnaryOp, IRValue, IRValue, IRType),
    Two(BinOp, IRValue, IRValue, IRValue, IRType),
    Cmp(CmpOp, IRValue, IRValue, IRValue, IRType),
    Cast(IRType, (IRValue, IRType), IRValue),
    CopyWithOffset((IRValue, usize), (IRValue, usize)),
    AddPtr(IRValue, IRValue, IRValue, CSize),
}

impl fmt::Display for IROp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Return(val, size) => {
                write!(f, "{KEYWORD}ret{KEYWORD:#} {NUM}{size}{NUM:#} {val}")
            }
            Self::GetReturnValue(dst, size) => {
                write!(f, "{dst} = {KEYWORD}retval{KEYWORD:#} {NUM}{size}{NUM:#}")
            }
            Self::GetIdOfFunction(name, dst) => {
                write!(f, "{dst} = {KEYWORD}fnptr{KEYWORD:#} @{name}")
            }
            Self::Call(name, args) => {
                let args: Vec<_> = args.iter().map(|(v, s)| format!("{v}[{s}]")).collect();
                write!(
                    f,
                    "{KEYWORD}call{KEYWORD:#} {IDENT}@{name}{IDENT:#}({})",
                    args.join(", ")
                )
            }
            Self::Label(name) => write!(f, "{LABEL}{name}{LABEL:#}:"),
            Self::InlineBefunge(str) => {
                let str = if str.len() == 1 {
                    " ".to_string() + &str[0]
                } else {
                    let str = str
                        .iter()
                        .map(|str| format!("\n    {str}"))
                        .collect::<String>();
                    str + "\n "
                };
                write!(f, "{KEYWORD}asm{KEYWORD:#} {{{str} }}")
            }
            Self::AlwaysBranch(label) => {
                write!(f, "{KEYWORD}br{KEYWORD:#} {LABEL}{label}{LABEL:#}")
            }
            Self::CondBranch(branch_type, label, cond) => {
                write!(
                    f,
                    "{KEYWORD}br.{branch_type}{KEYWORD:#} {cond}, {LABEL}{label}{LABEL:#}"
                )
            }
            Self::AddressOf(src, dst) => write!(f, "{dst} = {KEYWORD}addrof{KEYWORD:#} {src}"),
            Self::Dereference(src, dst, size) => write!(
                f,
                "{dst} = {KEYWORD}deref{KEYWORD:#} {NUM}{size}{NUM:#} {src}"
            ),
            Self::Copy(src, dst, size) => write!(
                f,
                "{dst} = {KEYWORD}copy{KEYWORD:#} {NUM}{size}{NUM:#} {src}"
            ),
            Self::Store(src, dst, size) => {
                write!(f, "{KEYWORD}store{KEYWORD:#}[{size}] {src} -> {dst}")
            }
            Self::One(op, src, dst, ty) => write!(f, "{dst} = {KEYWORD}{op}{KEYWORD:#} {ty} {src}"),
            Self::Two(op, lhs, rhs, dst, ty) => {
                write!(f, "{dst} = {KEYWORD}{op}{KEYWORD:#} {ty} {lhs}, {rhs}")
            }
            Self::Cmp(op, lhs, rhs, dst, ty) => {
                write!(f, "{dst} = {KEYWORD}cmp.{op}{KEYWORD:#} {ty} {lhs}, {rhs}")
            }
            Self::Cast(to_ty, (src, from_ty), dst) => {
                write!(
                    f,
                    "{dst} = {KEYWORD}cast{KEYWORD:#} {src} : {from_ty} -> {to_ty}"
                )
            }
            Self::CopyWithOffset((src, src_off), (dst, dst_off)) => {
                if *src_off == 0 {
                    write!(
                        f,
                        "{dst}+{NUM}{dst_off}{NUM:#} = {KEYWORD}copy{KEYWORD:#} {src}"
                    )
                } else {
                    write!(
                        f,
                        "{dst}+{NUM}{dst_off}{NUM:#} = {KEYWORD}copy{KEYWORD:#} {src}+{NUM}{src_off}{NUM:#}"
                    )
                }
            }
            Self::AddPtr(ptr, offset, dst, scale) => {
                write!(
                    f,
                    "{dst} = {KEYWORD}addptr{KEYWORD:#} {ptr}, {offset} * {NUM}{scale}{NUM:}"
                )
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BranchType {
    NonZero,
    Zero,
}

impl fmt::Display for BranchType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NonZero => write!(f, "nz"),
            Self::Zero => write!(f, "z"),
        }
    }
}

#[derive(strum_macros::Display, Debug, Clone, Copy)]
#[strum(serialize_all = "snake_case")]
pub enum UnaryOp {
    Minus,
    Complement,
    BooleanNegate,
}

#[derive(strum_macros::Display, Debug, Clone, Copy)]
#[strum(serialize_all = "snake_case")]
pub enum BinOp {
    Add,
    Sub,
    Mult,
    Div,
    Mod,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
}

#[derive(Debug, Clone, Copy)]
pub enum CmpOp {
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

impl fmt::Display for CmpOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Equal => write!(f, "eq"),
            Self::NotEqual => write!(f, "ne"),
            Self::LessThan => write!(f, "lt"),
            Self::LessOrEqual => write!(f, "le"),
            Self::GreaterThan => write!(f, "gt"),
            Self::GreaterOrEqual => write!(f, "ge"),
        }
    }
}

#[derive(Default, Debug)]
pub struct IRTopLevel {
    pub name: String,
    pub stack_frame_size: usize,
    pub parameters_size: usize,
    pub ops: Vec<IROp>,
    pub is_initializer: bool,
    pub return_type: Option<CType>,
}

#[derive(Debug, Clone, Copy)]
pub struct FuncInfo {
    pub stack_frame_size: usize,
    pub id: usize,
}

pub fn print_ir(ir: &Vec<IRTopLevel>) {
    for func in ir {
        println!("\n// frame_size: {}", func.stack_frame_size);
        if func.is_initializer {
            println!("{KEYWORD}def{KEYWORD:#} {IDENT}{}{IDENT:#} {{", func.name);
        } else {
            let ty = func
                .return_type
                .as_ref()
                .map_or_else(|| "no type?".into(), CType::display_type_badly);
            println!(
                "{KEYWORD}fn{KEYWORD:#} {TYPE}{ty}{TYPE:#} {IDENT}@{}{IDENT:#}(..{}) {{",
                func.name, func.parameters_size
            );
        }
        for line in &func.ops {
            if matches!(line, IROp::Label(_)) {
                println!(" {line}");
            } else {
                println!("  {line}");
            }
        }
        println!("}}");
    }
}
