use thiserror::Error;

use crate::c_compiler::{CType, ScopeInfo};

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
    Psuedo { name: String, size: usize },
    /// A location to be determined Later, will be in static memory
    StaticPsuedo {
        name: String,
        linkable: bool,
        size: usize,
    },
    /// The current value on the top of the bstack
    /// Must be careful when using
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

impl IRType {
    pub fn from_ctype(value: &CType, scope: &ScopeInfo) -> Self {
        match value {
            CType::SignedInt => Self::Signed(16),
            CType::SignedLong => Self::Signed(32),
            CType::UnsignedInt => Self::Unsigned(32),
            CType::UnsignedLong => Self::Unsigned(32),
            CType::Void => panic!("void cannot be used as concrete types"),
            CType::Pointer(_) => Self::Signed(64),
            CType::Array(..) | CType::ImmediateArray(..) => Self::Sized(value.sizeof(scope)),
            CType::Struct(tag_id) => Self::Sized(scope.get_struct_by_id(*tag_id).size),
            CType::Function(..) => panic!("functions cannot be used as concrete types"),
        }
    }
}

impl CType {
    pub fn sizeof(&self, scope: &ScopeInfo) -> usize {
        match self {
            Self::SignedInt
            | Self::SignedLong
            | Self::UnsignedInt
            | Self::UnsignedLong
            | Self::Pointer(..) => 1,
            Self::Array(inner_type, size) | Self::ImmediateArray(inner_type, size) => {
                inner_type.sizeof(scope) * size
            }
            Self::Struct(tag_id) => scope.get_struct_by_id(*tag_id).size,
            Self::Void => panic!("void is not sized"),
            Self::Function(..) => panic!("functions cannot be used as concrete types"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum IROp {
    Return(IRValue),
    Call(String, Vec<(IRValue, usize)>),
    Label(String),
    InlineBefunge(Vec<String>),
    AlwaysBranch(String),
    CondBranch(BranchType, String, IRValue),
    AddressOf(IRValue, IRValue),
    One(UnaryOp, IRValue, IRValue, IRType),
    Two(BinOp, IRValue, IRValue, IRValue, IRType),
    Cast(IRType, (IRValue, IRType), IRValue),
    CopyToOffset(IRValue, IRValue, usize),
    CopyFromOffset(IRValue, IRValue, usize),
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
