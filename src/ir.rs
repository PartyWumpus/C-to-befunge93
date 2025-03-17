use crate::c_compiler::CType;

#[derive(Debug, Clone)]
pub enum IRValue {
    Stack(usize),
    Immediate(usize),
    Register(usize), // limited to only like 70ish tho
    Data(usize),
    Psuedo { name: String },
    StaticPsuedo { name: String, linkable: bool },
    // Must be careful when using
    BefungeStack,
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
enum IRType {
    Signed(u8),
    Unsigned(u8),
    Float,
}

impl From<CType> for IRType {
    fn from(value: CType) -> Self {
        match value {
            CType::SignedInt => IRType::Signed(16),
            CType::SignedLong => IRType::Signed(32),
            CType::Void => panic!("void cannot be converted to an ir type"),
            CType::Pointer(_) => IRType::Signed(64),
            CType::Array(..) => todo!("array types"),
            CType::Function(..) => panic!("function types cannot be converted to irtypes"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum IROp {
    FunctionLabel(String),
    Return(IRValue),
    Call(String, Vec<IRValue>),
    Label(String),
    InlineBefunge(Vec<String>),
    AlwaysBranch(String),
    CondBranch(BranchType, String, IRValue),
    One(UnaryOp, IRValue, IRValue),
    Two(BinOp, IRValue, IRValue, IRValue),
    Cast(CType, (IRValue, CType), IRValue),
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
    BooleanNegate,
    Dereference,
    AddressOf,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
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
