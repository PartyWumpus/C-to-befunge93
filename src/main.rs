use std::{collections::HashMap, mem, process::exit};

use builder::OpBuilder;
use c::parse_c;

mod builder;

static PRELUDE: &str = r##"v!R#######
v#########                       ###################
v#########                       ###################
v#########
v#########
v#########
v#########
v#########
v#########
v#########
>"!"00p 010g2-2pv
>v  10          <
 >:#v_@
 v  <
"##;

#[derive(Debug)]
enum IRValue {
    Stack(usize),
    Immediate(usize),
    Register(usize), // limited to only like 70ish tho
    Psuedo(String),
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
            IRValue::Psuedo(_) => {
                panic!("Psuedo registers should be removed by befunge generation time")
            }
        }
    }

    fn put_val(&mut self, val: &IRValue) {
        match val {
            IRValue::Stack(offset) => self.builder.set_stack_val(*offset),
            IRValue::Register(id) => self.builder.set_register_val(*id),
            IRValue::Immediate(_) => panic!("Immediate value as output location"),
            IRValue::Psuedo(_) => {
                panic!("Psuedo registers should be removed by befunge generation time")
            }
        }
    }
}

/// TODO:
fn parse_ir(str: String) -> Vec<IROperation> {
    let mut out = vec![];
    for line in str.lines() {}
    out
}

mod c {

    use std::path::Path;

    use lang_c::{
        ast::{
            BlockItem, Constant, Declaration, Declarator, DeclaratorKind, Expression,
            ExternalDeclaration, FunctionDefinition, IntegerBase, Statement,
        },
        driver::{parse, Config},
        span::Node,
    };

    use crate::{IRFunction, IROperation, IRValue};

    pub fn parse_c<P: AsRef<Path>>(file: P) -> Result<Vec<IRFunction>, lang_c::driver::Error> {
        let config = Config::default();
        let parsed = parse(&config, file)?;
        let out = parsed
            .unit
            .0
            .iter()
            .map(|obj| match &obj.node {
                ExternalDeclaration::Declaration(_) => todo!("add declarations"),
                ExternalDeclaration::StaticAssert(_) => todo!("add static asserts"),
                ExternalDeclaration::FunctionDefinition(obj) => parse_function(&obj.node),
            })
            .collect::<Vec<_>>();
        Ok(out)
    }

    fn parse_function(func: &FunctionDefinition) -> IRFunction {
        // TODO: deal with specifiers
        // TODO: deal with K&R declarations, although i only really care about c99
        let name = parse_declarator(&func.declarator.node);
        let vec = parse_statement(&func.statement.node);

        // TODO: do pseudo -> stack pass

        IRFunction {
            name,
            ops: vec,
            stack_frame_size: 5, // FIXME: :3
        }
    }

    fn parse_declarator(decl: &Declarator) -> String {
        match &decl.kind.node {
            DeclaratorKind::Identifier(node) => node.node.name.clone(),
            _ => todo!("add non identifier functions"),
        }
    }

    fn parse_statement(stmt: &Statement) -> Vec<IROperation> {
        match stmt {
            Statement::Return(maybe_expr) => {
                if let Some(expr) = maybe_expr {
                    vec![IROperation::Return(parse_expression(&expr.node))]
                } else {
                    vec![IROperation::Return(IRValue::Immediate(0))]
                }
            }
            Statement::Compound(blocks) => {
                let mut ops = vec![];
                for block in blocks {
                    ops.extend(parse_block(&block.node));
                }
                ops
            }
            _ => todo!("STATEMENT:, {:?}", stmt),
        }
    }

    fn parse_block(block: &BlockItem) -> Vec<IROperation> {
        match block {
            BlockItem::Statement(stmt) => parse_statement(&stmt.node),
            _ => todo!("BLOCK: {:?}", block),
        }
    }

    fn parse_expression(expr: &Expression) -> IRValue {
        match expr {
            Expression::Constant(constant) => parse_constant(&constant.node),
            _ => todo!("EXPRESSION: {:?}", expr),
        }
    }

    fn parse_constant(val: &Constant) -> IRValue {
        match val {
            Constant::Integer(int) => {
                let x: usize = match int.base {
                    IntegerBase::Decimal => usize::from_str_radix(&int.number, 10).unwrap(),
                    _ => todo!("INT: {:?}", int),
                };
                IRValue::Immediate(x)
            }
            _ => todo!("CONSTANT: {:?}", val),
        }
    }
}

fn main() {
    let program = parse_c("examples/listing1-1.c").unwrap();

    // TODO: make proper function map pass
    let mut function_map = HashMap::new();
    function_map.insert("main".to_owned(), 1);
    function_map.insert("phonk".to_owned(), 2);

    println!("");
    for line in &program {
        println!("{line:?}");
    }
    println!("-- befunge begin");
    let mut cg = CodeGen {
        builder: OpBuilder::new(),
        function_map,
    };
    println!("{}", PRELUDE);
    for func in program {
        cg.compile_function(func);
    }
}
