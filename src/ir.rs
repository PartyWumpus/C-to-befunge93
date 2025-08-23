use thiserror::Error;

use crate::c_compiler::CType;

#[derive(Debug, Clone)]
pub enum IRValue {
    Stack(usize),
    Immediate(usize),
    Register(usize), // limited to only like 70ish tho
    Data(usize),
    Psuedo {
        name: String,
        size: usize,
    },
    StaticPsuedo {
        name: String,
        linkable: bool,
        size: usize,
    },
    // Must be careful when using
    BefungeStack,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
pub enum IRType {
    Signed(u8),
    Unsigned(u8),
    Float,
    Sized(usize),
}

#[derive(Error, Debug)]
pub enum IRTypeConversionError {
    #[error("Void cannot be used in a concrete value")]
    VoidNotValidType,
    #[error("Function types cannot be used in a concrete value")]
    FunctionNotValidType,
}

impl From<CType> for IRType {
    fn from(value: CType) -> Self {
        Self::from(&value)
    }
}

impl From<&CType> for IRType {
    fn from(value: &CType) -> Self {
        match value {
            CType::SignedInt => Self::Signed(16),
            CType::SignedLong => Self::Signed(32),
            CType::UnsignedInt => Self::Unsigned(32),
            CType::UnsignedLong => Self::Unsigned(32),
            CType::Void => panic!("void cannot be used as concrete types"),
            CType::Pointer(_) => Self::Signed(64),
            CType::Array(..) => Self::Sized(value.sizeof()),
            CType::Function(..) => panic!("functions cannot be used as concrete types"),
        }
    }
}

impl CType {
    pub fn sizeof(&self) -> usize {
        match self {
            CType::SignedInt
            | CType::SignedLong
            | CType::UnsignedInt
            | CType::UnsignedLong
            | CType::Pointer(..) => 1,
            CType::Array(inner_type, size) => inner_type.sizeof() * size,
            CType::Void => panic!("void is not sized"),
            CType::Function(..) => panic!("functions cannot be used as concrete types"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum IROp {
    Return(IRValue),
    Call(String, Vec<IRValue>),
    Label(String),
    InlineBefunge(Vec<String>),
    AlwaysBranch(String),
    CondBranch(BranchType, String, IRValue),
    AddressOf(IRValue, IRValue),
    One(UnaryOp, IRValue, IRValue, IRType),
    Two(BinOp, IRValue, IRValue, IRValue, IRType),
    Cast(IRType, (IRValue, IRType), IRValue),
    CopyToOffset(IRValue, IRValue, usize),
    AddPtr(IRValue, IRValue, IRValue, usize),
}

#[derive(Debug, Clone, Copy)]
pub enum BranchType {
    NonZero,
    Zero,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Minus,
    Complement,
    Copy,
    Store,
    BooleanNegate,
    Dereference,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    // TODO: make these seperate because they always return int
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
}

#[derive(Debug)]
pub struct IRTopLevel {
    pub name: String,
    pub stack_frame_size: usize,
    pub parameters: usize,
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
        if func.is_initializer {
            println!("\nINIT REGION {}", func.name);
        } else {
            println!("\nFUNC {:?}", func.name);
        }
        println!("frame_size: {}", func.stack_frame_size);
        for line in &func.ops {
            println!("{line:?}");
        }
    }
}
