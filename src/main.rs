use std::{collections::HashMap, mem};

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
    FunctionLabel(String),
    Return(IRValue),
    Call(String, Vec<IRValue>),
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
    Mov,
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
    stack_frame_size: usize,
    ops: Vec<IROperation>,
}

struct CodeGen {
    builder: OpBuilder,
    function_map: HashMap<String, usize>,
}

impl CodeGen {
    fn compile_function(&mut self, func: IRFunction) {
        for op in func.ops {
            match &op {
                IROperation::FunctionLabel(String) => (),
                IROperation::Call(call_func_name, vals) => {
                    for (i, val) in vals.iter().enumerate() {
                        self.get_val(&val);
                        self.put_val(&IRValue::Stack(func.stack_frame_size + i + 1));
                    }

                    self.builder.load_number(0);
                    self.builder.load_number(self.function_map[call_func_name]);

                    self.builder.increment_stack_ptr(func.stack_frame_size);
                    self.builder.load_number(self.function_map[&func.name]);
                    self.builder.call();
                }
                IROperation::Return(val) => {
                    self.get_val(val);
                    self.builder.return_(func.stack_frame_size);
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
                        UnaryOp::Mov => (),
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
            self.builder.append_char(' ');
        }

        let mut builder = OpBuilder::new();
        mem::swap(&mut builder, &mut self.builder);
        for line in builder.finalize_function() {
            println!("{}", line)
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
    /*
        let x = r#"FUNC phonk
    ADD s1, i5, r2
    LABEL point
    MULT i100, r2, s1
    BRZ point, s5
    RET s1"#;
        println!("{x}");
        let x = IRFunction {
            name: "phonk".to_string(),
            stack_frame_size: 1,
            ops: vec![
                IROperation::FunctionLabel("phonk".to_string()),
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
        */

    let x = r#"
    FUNC main
    LABEL labl
    ADD i50 i4 s1
    BRZ labl r1
    CALL phonk s1
    ADD r0 i1 r4
    RET r4

    FUNC phonk
    ADD s1 i2 r5
    RET r5
    "#;
    println!("{x}");
    let main = IRFunction {
        name: "main".to_string(),
        stack_frame_size: 2,
        ops: vec![
            IROperation::FunctionLabel("main".to_string()),
            IROperation::Label("labl".to_owned()),
            IROperation::Two(
                BinOp::Add,
                IRValue::Immediate(50),
                IRValue::Immediate(4),
                IRValue::Stack(1),
            ),
            IROperation::Branch(BranchType::Zero, "labl".to_owned(), IRValue::Stack(1)),
            IROperation::Call("phonk".to_owned(), vec![IRValue::Stack(1)]),
            IROperation::Two(
                BinOp::Add,
                IRValue::Register(0),
                IRValue::Immediate(1),
                IRValue::Register(4),
            ),
            IROperation::Return(IRValue::Register(4)),
        ],
    };

    let phonk = IRFunction {
        name: "phonk".to_string(),
        stack_frame_size: 2,
        ops: vec![
            IROperation::FunctionLabel("phonk".to_string()),
            IROperation::Two(
                BinOp::Add,
                IRValue::Stack(1),
                IRValue::Immediate(2),
                IRValue::Register(5),
            ),
            IROperation::Return(IRValue::Register(5)),
        ],
    };
    let mut function_map = HashMap::new();
    function_map.insert("main".to_owned(), 1);
    function_map.insert("phonk".to_owned(), 2);
    println!("");
    for line in &main.ops {
        println!("{line:?}");
    }
    println!("");
    for line in &phonk.ops {
        println!("{line:?}");
    }
    println!("");
    let mut cg = CodeGen {
        builder: OpBuilder::new(),
        function_map,
    };
    cg.compile_function(main);
    println!("");
    cg.compile_function(phonk);
}
