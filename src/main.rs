use std::{collections::HashMap, mem};

use builder::OpBuilder;
use c::{function_id_mapping_pass, parse_c, pseudo_removal_pass, stack_size_reducer_pass};

mod builder;

static PRELUDE: &str = r#"v!R#######
v#########            STACK -> xx###################
v#########       CALL STACK -> xx###################
v#########
v#########
v#########
v#########
v#########
v#########
v#########
>"!"00p 010g2-2pv
>v  10          <
 >:#v_  $$ 20g . @
 v  <
"#;

#[derive(Debug, Clone)]
enum IRValue {
    Stack(usize),
    Immediate(usize),
    Register(usize), // limited to only like 70ish tho
    Psuedo(String),
}

#[derive(Debug, Clone)]
pub enum IROp {
    FunctionLabel(String),
    Return(IRValue),
    Call(String, Vec<IRValue>),
    Label(String),
    AlwaysBranch(String),
    CondBranch(BranchType, String, IRValue),
    One(UnaryOp, IRValue, IRValue),
    Two(BinOp, IRValue, IRValue, IRValue),
}

#[derive(Debug, Clone, Copy)]
enum BranchType {
    NonZero,
    Zero,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Minus,
    Complement,
    Copy,
}

#[derive(Debug, Clone, Copy)]
enum BinOp {
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
}

#[derive(Debug)]
struct IRFunction {
    name: String,
    stack_frame_size: usize,
    ops: Vec<IROp>,
}

struct CodeGen {
    builder: OpBuilder,
    function_map: HashMap<String, usize>,
}

impl CodeGen {
    fn compile_function(&mut self, func: IRFunction) -> Vec<String> {
        for op in func.ops {
            match &op {
                IROp::FunctionLabel(_) => (),
                IROp::Call(call_func_name, vals) => {
                    for (i, val) in vals.iter().enumerate() {
                        self.get_val(val);
                        self.put_val(&IRValue::Stack(func.stack_frame_size + i + 1));
                    }

                    self.builder.load_number(0);
                    self.builder.load_number(self.function_map[call_func_name]);

                    self.builder.increment_stack_ptr(func.stack_frame_size);
                    self.builder.load_number(self.function_map[&func.name]);
                    self.builder.call();
                }
                IROp::Return(val) => {
                    self.get_val(val);
                    self.builder.return_(func.stack_frame_size);
                }
                IROp::Label(label) => self.builder.label(label.to_owned()),
                IROp::CondBranch(flavour, label, val) => {
                    self.get_val(val);
                    match flavour {
                        BranchType::Zero => self.builder.zero_branch(label.to_string()),
                        BranchType::NonZero => self.builder.not_zero_branch(label.to_string()),
                    }
                }
                IROp::AlwaysBranch(label) => self.builder.unconditional_branch(label.to_owned()),
                IROp::One(op, a, out) => {
                    match op {
                        UnaryOp::Copy => self.get_val(a),
                        UnaryOp::Minus => {
                            self.builder.char('0');
                            self.get_val(a);
                            self.builder.char('-');
                        }
                        UnaryOp::Complement => {
                            self.builder.char('1');
                            self.get_val(a);
                            self.builder.char('-');
                        }
                    }
                    self.put_val(out);
                }
                IROp::Two(op, a, b, out) => {
                    self.get_val(a);
                    self.get_val(b);
                    match op {
                        BinOp::Add => self.builder.char('+'),
                        BinOp::Sub => self.builder.char('-'),
                        BinOp::Mult => self.builder.char('*'),
                        BinOp::Div => self.builder.char('/'),
                        BinOp::Mod => self.builder.char('%'),
                        BinOp::Equal => self.builder.str("-!"),
                        BinOp::NotEqual => self.builder.str("-!!"),
                        BinOp::LessThan => {
                            todo!("less than");
                            self.builder.str("");
                        }
                        BinOp::LessOrEqual => self.builder.str("`!"),
                        BinOp::GreaterThan => self.builder.char('`'),
                        BinOp::GreaterOrEqual => {
                            todo!("greater or equal");
                            self.builder.str("");
                        }
                    }
                    self.put_val(out);
                }
            }
            self.builder.char(' ');
        }

        let mut builder = OpBuilder::new();
        mem::swap(&mut builder, &mut self.builder);
        builder.finalize_function()
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

mod c {

    use std::{collections::HashMap, path::Path};

    use lang_c::{
        ast::{
            BinaryOperator, BinaryOperatorExpression, BlockItem, CallExpression, Constant,
            Declaration, Declarator, DeclaratorKind, Expression, ExternalDeclaration,
            FunctionDefinition, IfStatement, Initializer, IntegerBase, Statement, UnaryOperator,
            UnaryOperatorExpression,
        },
        driver::{parse, Config},
    };

    use crate::{BinOp, BranchType, IRFunction, IROp, IRValue, UnaryOp};

    struct CFunctionThingy {
        count: usize,
        ops: Vec<IROp>,
    }

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

    struct PsuedoMap {
        map: HashMap<String, usize>,
        count: usize,
    }

    fn pseudo_removal(func: &mut IRFunction) {
        let mut map = PsuedoMap {
            map: HashMap::new(),
            count: 0,
        };
        for i in 0..func.ops.len() {
            func.ops[i] = match &func.ops[i] {
                IROp::Return(o) => IROp::Return(map.get(o)),
                IROp::Call(str, ops) => {
                    IROp::Call(str.clone(), ops.iter().map(|o| map.get(o)).collect())
                }
                IROp::CondBranch(flavor, str, a) => {
                    IROp::CondBranch(*flavor, str.clone(), map.get(a))
                }
                IROp::One(flavour, a, out) => IROp::One(*flavour, map.get(a), map.get(out)),
                IROp::Two(flavour, a, b, o) => {
                    IROp::Two(*flavour, map.get(a), map.get(b), map.get(o))
                }
                x => x.clone(),
            }
        }
    }

    impl PsuedoMap {
        fn get(&mut self, val: &IRValue) -> IRValue {
            if let IRValue::Psuedo(str) = val {
                if let Some(id) = self.map.get(str) {
                    IRValue::Stack(*id)
                } else {
                    self.count += 1;
                    self.map.insert(str.to_owned(), self.count);
                    IRValue::Stack(self.count)
                }
            } else {
                val.clone()
            }
        }
    }

    pub fn pseudo_removal_pass(funcs: &mut Vec<IRFunction>) {
        for func in funcs {
            pseudo_removal(func);
        }
    }

    fn stack_size_recalculator(func: &IRFunction) -> usize {
        // find biggest Stack value
        let mut counter = 0;
        let mut check = |val: &IRValue| {
            if let IRValue::Stack(val) = val {
                if *val > counter {
                    counter = *val;
                }
            }
        };
        for op in &func.ops {
            match op {
                IROp::Return(o) => check(o),
                IROp::Call(_, ops) => ops.iter().for_each(&mut check),
                IROp::CondBranch(_, _, a) => check(a),
                IROp::One(_, a, out) => {
                    check(a);
                    check(out);
                }
                IROp::Two(_, a, b, out) => {
                    check(a);
                    check(b);
                    check(out);
                }
                _ => (),
            };
        }
        counter
    }

    pub fn stack_size_reducer_pass(funcs: &mut Vec<IRFunction>) {
        for func in funcs {
            func.stack_frame_size = stack_size_recalculator(func);
        }
    }

    pub fn function_id_mapping_pass(funcs: &Vec<IRFunction>) -> HashMap<String, usize> {
        let mut map = HashMap::new();
        let mut i = 2;
        for func in funcs {
            if func.name == "main" {
                map.insert(func.name.clone(), 1);
            } else {
                map.insert(func.name.clone(), i);
                i += 1;
            }
        }
        map
    }

    fn parse_function(func: &FunctionDefinition) -> IRFunction {
        let mut function_state = CFunctionThingy {
            ops: vec![],
            count: 0,
        };
        // TODO: deal with specifiers
        // TODO: deal with K&R declarations, although i only really care about c99
        let name = function_state.parse_func_declarator(&func.declarator.node);
        function_state.parse_statement(&func.statement.node);

        IRFunction {
            name,
            ops: function_state.ops,
            stack_frame_size: function_state.count, // NOTE: this is a 'worst case' value,
                                                    // and should be recalculated in a later pass
        }
    }

    impl CFunctionThingy {
        fn parse_func_declarator(&self, decl: &Declarator) -> String {
            match &decl.kind.node {
                DeclaratorKind::Identifier(node) => node.node.name.clone(),
                _ => todo!("add non identifier functions"),
            }
        }

        fn parse_declarator(&self, decl: &Declarator) -> IRValue {
            match &decl.kind.node {
                DeclaratorKind::Identifier(node) => IRValue::Psuedo(node.node.name.clone()),
                _ => todo!("add non trivial identifier declarations"),
            }
        }

        fn push(&mut self, op: IROp) {
            self.ops.push(op);
        }

        fn parse_statement(&mut self, stmt: &Statement) -> IRValue {
            match stmt {
                Statement::Return(maybe_expr) => {
                    if let Some(expr) = maybe_expr {
                        let value = self.parse_expression(&expr.node);
                        self.push(IROp::Return(value));
                    } else {
                        self.push(IROp::Return(IRValue::Immediate(0)));
                    };
                    IRValue::Immediate(0)
                }
                Statement::Compound(blocks) => {
                    for block in blocks {
                        self.parse_block(&block.node);
                    }
                    IRValue::Immediate(0)
                }
                Statement::Expression(Some(expr)) => self.parse_expression(&expr.node),
                Statement::Expression(None) => IRValue::Immediate(0),
                Statement::If(stmt) => {
                    self.parse_if(&stmt.node);
                    IRValue::Immediate(0)
                }
                _ => todo!("STATEMENT:, {:?}", stmt),
            }
        }

        fn parse_if(&mut self, if_stmt: &IfStatement) {
            let cond = self.parse_expression(&if_stmt.condition.node);
            let (else_str, else_label) = self.generate_label("else");
            let (end_str, end_label) = self.generate_label("else");
            self.push(IROp::CondBranch(BranchType::Zero, else_str, cond));
            self.parse_statement(&if_stmt.then_statement.node);
            if if_stmt.else_statement.is_some() {
                self.push(IROp::AlwaysBranch(end_str));
            };
            self.push(else_label);
            if let Some(else_stmt) = &if_stmt.else_statement {
                self.parse_statement(&else_stmt.node);
                self.push(end_label);
            }
        }

        fn parse_block(&mut self, block: &BlockItem) {
            match block {
                BlockItem::Statement(stmt) => {
                    self.parse_statement(&stmt.node);
                }
                BlockItem::Declaration(decls) => self.parse_declarations(&decls.node),
                BlockItem::StaticAssert(_) => todo!("STATIC ASSERT BLOCK: {:?}", block),
            };
        }

        fn parse_declarations(&mut self, decls: &Declaration) {
            // TODO: deal with specifiers
            // FIXME: CLONE :(
            for decl in decls.declarators.clone() {
                if let Some(init) = decl.node.initializer {
                    let loc = self.parse_declarator(&decl.node.declarator.node);
                    let init = self.parse_initializer(&init.node);
                    self.push(IROp::One(UnaryOp::Copy, init, loc));
                }
            }
        }

        fn parse_initializer(&mut self, init: &Initializer) -> IRValue {
            match init {
                Initializer::Expression(expr) => self.parse_expression(&expr.node),
                Initializer::List(_) => todo!("lists in initializers"),
            }
        }

        fn parse_expression(&mut self, expr: &Expression) -> IRValue {
            match expr {
                Expression::Constant(constant) => self.parse_constant(&constant.node),
                Expression::UnaryOperator(unary_expr) => {
                    self.parse_unary_expression(&unary_expr.node)
                }
                Expression::BinaryOperator(binary_expr) => {
                    self.parse_binary_expression(&binary_expr.node)
                }
                Expression::Identifier(ident) => IRValue::Psuedo(ident.node.name.clone()),
                Expression::Call(call_expr) => self.parse_call(&call_expr.node),
                _ => todo!("EXPRESSION: {:?}", expr),
            }
        }

        fn parse_call(&mut self, expr: &CallExpression) -> IRValue {
            let args = expr
                .arguments
                .iter()
                .map(|expr| self.parse_expression(&expr.node))
                .collect::<Vec<IRValue>>();
            match &expr.callee.node {
                Expression::Identifier(ident) => {
                    self.push(IROp::Call(ident.node.name.clone(), args));
                }
                _ => todo!("non ident calls"),
            };
            let out = self.generate_pseudo();
            self.push(IROp::One(UnaryOp::Copy, IRValue::Register(0), out.clone()));
            out
        }

        fn parse_unary_expression(&mut self, expr: &UnaryOperatorExpression) -> IRValue {
            let val = self.parse_expression(&expr.operand.node);
            let out = self.generate_pseudo();
            self.push(match expr.operator.node {
                UnaryOperator::Complement => IROp::One(UnaryOp::Complement, val, out.clone()),
                UnaryOperator::Minus => IROp::One(UnaryOp::Minus, val, out.clone()),
                _ => todo!("UNARY OP: {:?}", expr),
            });
            out
        }

        fn parse_binary_expression(&mut self, expr: &BinaryOperatorExpression) -> IRValue {
            let (skip_label_str, skip_label) = self.generate_label("skip");
            let (end_label_str, end_label) = self.generate_label("end");

            let lhs = self.parse_expression(&expr.lhs.node);

            if matches!(expr.operator.node, BinaryOperator::LogicalAnd) {
                self.push(IROp::CondBranch(
                    BranchType::Zero,
                    skip_label_str.clone(),
                    lhs.clone(),
                ));
            }
            if matches!(expr.operator.node, BinaryOperator::LogicalOr) {
                self.push(IROp::CondBranch(
                    BranchType::NonZero,
                    skip_label_str.clone(),
                    lhs.clone(),
                ));
            }

            let rhs = self.parse_expression(&expr.rhs.node);

            if matches!(expr.operator.node, BinaryOperator::LogicalAnd) {
                self.push(IROp::CondBranch(
                    BranchType::Zero,
                    skip_label_str.clone(),
                    rhs.clone(),
                ));
            }
            if matches!(expr.operator.node, BinaryOperator::LogicalOr) {
                self.push(IROp::CondBranch(
                    BranchType::NonZero,
                    skip_label_str,
                    rhs.clone(),
                ));
            }

            if matches!(expr.operator.node, BinaryOperator::LogicalAnd) {
                let out = self.generate_pseudo();
                self.ops
                    .push(IROp::One(UnaryOp::Copy, IRValue::Immediate(1), out.clone()));

                self.push(IROp::AlwaysBranch(end_label_str));
                self.push(skip_label);
                self.ops
                    .push(IROp::One(UnaryOp::Copy, IRValue::Immediate(0), out.clone()));
                self.push(end_label);
                return out;
            };

            if matches!(expr.operator.node, BinaryOperator::LogicalOr) {
                let out = self.generate_pseudo();
                self.ops
                    .push(IROp::One(UnaryOp::Copy, IRValue::Immediate(0), out.clone()));

                self.push(IROp::AlwaysBranch(end_label_str));
                self.push(skip_label);
                self.ops
                    .push(IROp::One(UnaryOp::Copy, IRValue::Immediate(1), out.clone()));
                self.push(end_label);
                return out;
            };

            let out = self.generate_pseudo();
            self.push(match expr.operator.node {
                BinaryOperator::Multiply => IROp::Two(BinOp::Mult, lhs, rhs, out.clone()),
                BinaryOperator::Divide => IROp::Two(BinOp::Div, lhs, rhs, out.clone()),
                BinaryOperator::Modulo => IROp::Two(BinOp::Mod, lhs, rhs, out.clone()),
                BinaryOperator::Plus => IROp::Two(BinOp::Add, lhs, rhs, out.clone()),
                BinaryOperator::Minus => IROp::Two(BinOp::Sub, lhs, rhs, out.clone()),

                BinaryOperator::Less => IROp::Two(BinOp::LessThan, lhs, rhs, out.clone()),
                BinaryOperator::Greater => IROp::Two(BinOp::GreaterThan, lhs, rhs, out.clone()),
                BinaryOperator::LessOrEqual => IROp::Two(BinOp::LessOrEqual, lhs, rhs, out.clone()),
                BinaryOperator::GreaterOrEqual => {
                    IROp::Two(BinOp::GreaterOrEqual, lhs, rhs, out.clone())
                }
                BinaryOperator::Equals => IROp::Two(BinOp::Equal, lhs, rhs, out.clone()),
                BinaryOperator::NotEquals => IROp::Two(BinOp::NotEqual, lhs, rhs, out.clone()),

                BinaryOperator::Index => todo!("index"),

                // bitwise ops
                BinaryOperator::ShiftLeft => todo!("ShiftLeft"),
                BinaryOperator::ShiftRight => todo!("ShiftRight"),
                BinaryOperator::BitwiseAnd => todo!("BitwiseAnd"),
                BinaryOperator::BitwiseXor => todo!("BitwiseXor"),
                BinaryOperator::BitwiseOr => todo!("BitwiseOr"),

                // assign
                BinaryOperator::Assign => IROp::One(UnaryOp::Copy, rhs, lhs),
                BinaryOperator::AssignMultiply => todo!("AssignMultiply"),
                BinaryOperator::AssignDivide => todo!("AssignDivide"),
                BinaryOperator::AssignModulo => todo!("AssignModulo"),
                BinaryOperator::AssignPlus => todo!("AssignPlus"),
                BinaryOperator::AssignMinus => todo!("AssignMinus"),
                BinaryOperator::AssignShiftLeft => todo!("AssignShiftLeft"),
                BinaryOperator::AssignShiftRight => todo!("AssignShiftRight"),
                BinaryOperator::AssignBitwiseAnd => todo!("AssignBitwiseAnd"),
                BinaryOperator::AssignBitwiseXor => todo!("AssignBitwiseXor"),
                BinaryOperator::AssignBitwiseOr => todo!("AssignBitwiseOr"),

                // FREAKY SHORT CIRCUITING OPS!
                BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => unreachable!(),
            });
            out
        }

        fn parse_constant(&self, val: &Constant) -> IRValue {
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

        fn generate_pseudo(&mut self) -> IRValue {
            self.count += 1;
            IRValue::Psuedo("tmp.".to_owned() + &self.count.to_string())
        }

        fn generate_label(&mut self, str: &str) -> (String, IROp) {
            self.count += 1;
            let str = str.to_owned() + "." + &self.count.to_string();
            (str.clone(), IROp::Label(str))
        }
    }
}

fn main() {
    let x = std::fs::read_to_string("source.c").expect("Unable to read file");
    println!("{x}");
    let mut program = parse_c("source.c").unwrap();
    println!("-- IR, PRE PASSES");
    for func in &program {
        println!("\nFUNC {:?}", func.name);
        for line in &func.ops {
            println!("{line:?}");
        }
    }

    pseudo_removal_pass(&mut program);
    stack_size_reducer_pass(&mut program);
    let function_map = function_id_mapping_pass(&program);
    println!("{function_map:?}");

    println!("-- IR, POST PASSES");
    for func in &program {
        println!("FUNC {:?}", func.name);
        for line in &func.ops {
            println!("{line:?}");
        }
    }
    println!("-- befunge begin");
    let mut cg = CodeGen {
        builder: OpBuilder::new(),
        function_map,
    };

    let mut out: Vec<String> = vec![];
    out.extend(PRELUDE.lines().map(std::borrow::ToOwned::to_owned));
    for func in program {
        let x = cg.compile_function(func);
        out.extend(x);
    }
    for line in out.clone() {
        println!("{line}");
    }
    write_each("out.b93", out).unwrap();
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

// FIXME: SCOPE VARIABLE NAMES PROPERLY! CURRENTLY SCOPING IS JUST WRONG!!! WHOOPSIE!!!!
// FIXME: SIMILARLY, LOOP 'LABELING' IS GOINT TO BE WEIRD. OH NO
// FIXME: might have to replace lang_c...
