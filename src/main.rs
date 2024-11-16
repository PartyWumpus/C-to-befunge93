use std::mem;

use builder::OpBuilder;

mod builder;

#[derive(Debug)]
enum IRValue {
    Stack(usize),
    Immediate(usize),
    Register(usize), // limited to only like 70ish tho
}

#[derive(Debug)]
enum IROperation {
    Function(String),
    Return(IRValue),
    Label(String),
    UnconditionalBranch(String),
    Branch(BranchType, String, IRValue),
    One(UnaryOp, IRValue, IRValue),
    Two(BinOp, IRValue, IRValue, IRValue),
}

#[derive(Debug)]
enum BranchType {
    NonZero,
    Zero,
}

#[derive(Debug)]
enum UnaryOp {
    BinaryNot,
    Negate,
}

#[derive(Debug)]
enum BinOp {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    GreaterThan,
}

#[derive(Debug)]
struct IRFunction {
    name: String,
    ops: Vec<IROperation>,
}

struct CodeGen {
    builder: OpBuilder,
}

impl CodeGen {
    fn compile_function(&mut self, func: IRFunction) {
        for op in func.ops {
            self.build_op(&op);
            self.builder.append_char(' ');
        }

        let mut builder = OpBuilder::new();
        mem::swap(&mut builder, &mut self.builder);
        for line in builder.finalize_function() {
            println!("{}", line)
        }
    }

    fn build_op(&mut self, op: &IROperation) {
        match op {
            IROperation::Function(String) => (),
            IROperation::Return(val) => {
                self.get_val(val);
                self.builder.set_return_val();
            }
            IROperation::Label(label) => self.builder.label(label.to_owned()),
            IROperation::Branch(flavour, label, val) => {
                self.get_val(val);
                match flavour {
                    BranchType::Zero => self.builder.zero_branch(label.to_string()),
                    BranchType::NonZero => self.builder.not_zero_branch(label.to_string()),
                }
            }
            IROperation::UnconditionalBranch(label) => {
                self.builder.unconditional_branch(label.to_owned())
            }
            IROperation::One(op, a, out) => {
                self.get_val(a);
                match op {
                    UnaryOp::BinaryNot => (),
                    UnaryOp::Negate => (),
                }
                self.put_val(out);
            }
            IROperation::Two(op, a, b, out) => {
                self.get_val(a);
                self.get_val(b);
                match op {
                    BinOp::Add => self.builder.append_char('+'),
                    BinOp::Sub => self.builder.append_char('-'),
                    BinOp::Mult => self.builder.append_char('*'),
                    BinOp::Div => self.builder.append_char('/'),
                    BinOp::Mod => self.builder.append_char('%'),
                    BinOp::GreaterThan => self.builder.append_char('`'),
                }
                self.put_val(out);
            }
        }
    }

    fn get_val(&mut self, val: &IRValue) {
        match val {
            IRValue::Stack(offset) => self.builder.load_stack_val(*offset),
            IRValue::Register(id) => self.builder.load_register_val(*id),
            IRValue::Immediate(value) => self.builder.load_number(*value),
        }
    }

    fn put_val(&mut self, val: &IRValue) {
        match val {
            IRValue::Stack(offset) => self.builder.set_stack_val(*offset),
            IRValue::Register(id) => self.builder.set_register_val(*id),
            IRValue::Immediate(_) => panic!("Immediate value as output location"),
        }
    }
}

/// TODO:
fn parse(str: String) -> Vec<IROperation> {
    let mut out = vec![];
    for line in str.lines() {}
    out
}

fn main() {
    let x = r#"FUNC phonk
ADD s1, i5, r2
LABEL point
MULT i100, r2, s1
BRZ point, s5
RET s1"#;
    println!("{x}");
    let x = IRFunction {
        name: "phonk".to_string(),
        ops: vec![
            IROperation::Function("phonk".to_string()),
            IROperation::Two(
                BinOp::Add,
                IRValue::Stack(1),
                IRValue::Immediate(5),
                IRValue::Register(2),
            ),
            IROperation::Label("point".to_string()),
            IROperation::Two(
                BinOp::Mult,
                IRValue::Immediate(100),
                IRValue::Register(2),
                IRValue::Stack(1),
            ),
            IROperation::Branch(BranchType::Zero, "point".to_string(), IRValue::Stack(5)),
            IROperation::Return(IRValue::Stack(1)),
        ],
    };
    println!("");
    for line in &x.ops {
        println!("{line:?}");
    }
    println!("");
    let mut cg = CodeGen {
        builder: OpBuilder::new(),
    };
    cg.compile_function(x);
}
