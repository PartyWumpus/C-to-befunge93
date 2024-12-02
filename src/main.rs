use std::{collections::HashMap, mem};

use builder::OpBuilder;
use c::{function_id_mapping_pass, pseudo_removal_pass, stack_size_reducer_pass, FileBuilder};

mod builder;

static PRELUDE: &str = r#"v!R#######
v#########            STACK -> ################################################...
v#########       CALL STACK -> ################################################...
v#########    STATIC MEMORY -> ################################################...
v#########        // IN FUTURE, malloc() mem could be a 4th mem location here
v#########
v#########
v#########
v#########
v#########
>"!"00p 010g2-2pv
v               <
"#;

pub static PRELUDE2: &str = "0
1
>v
 >:#v_  $$ 20g . @
 v  <";

#[derive(Debug, Clone)]
enum IRValue {
    Stack(usize),
    Immediate(usize),
    Register(usize), // limited to only like 70ish tho
    Data(usize),
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
    BooleanNegate,
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
struct IRTopLevel {
    name: String,
    stack_frame_size: usize,
    parameters: usize,
    ops: Vec<IROp>,
    is_initializer: bool,
}

struct CodeGen {
    builder: OpBuilder,
    function_map: HashMap<String, usize>,
}

impl CodeGen {
    fn compile_top_level(&mut self, func: IRTopLevel) -> Vec<String> {
        self.builder = OpBuilder::new(!func.is_initializer);
        for op in func.ops {
            match &op {
                IROp::FunctionLabel(_) => (),
                IROp::Call(call_func_name, vals) => {
                    if func.is_initializer {
                        panic!("Non static call in static context")
                    };
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
                        UnaryOp::BooleanNegate => {
                            self.get_val(a);
                            self.builder.char('!');
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
                        BinOp::LessThan => self.builder.str("\\`"),
                        BinOp::LessOrEqual => self.builder.str("`!"),
                        BinOp::GreaterThan => self.builder.char('`'),
                        BinOp::GreaterOrEqual => self.builder.str("\\`!"),
                    }
                    self.put_val(out);
                }
            }
            self.builder.char(' ');
        }

        self.builder.finalize_function()
    }

    fn get_val(&mut self, val: &IRValue) {
        match val {
            IRValue::Stack(offset) => self.builder.load_stack_val(*offset),
            IRValue::Register(id) => self.builder.load_register_val(*id),
            IRValue::Immediate(value) => self.builder.load_number(*value),
            IRValue::Data(position) => self.builder.load_data_val(*position),
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
            IRValue::Data(position) => self.builder.set_data_val(*position),
            IRValue::Psuedo(_) => {
                panic!("Psuedo registers should be removed by befunge generation time")
            }
        }
    }
}

mod c {

    use std::{collections::HashMap, path::Path, process::exit};

    use lang_c::{
        ast::{
            BinaryOperator, BinaryOperatorExpression, BlockItem, CallExpression, Constant,
            Declaration, Declarator, DeclaratorKind, DerivedDeclarator, DoWhileStatement,
            Expression, ExternalDeclaration, ForInitializer, ForStatement, FunctionDefinition,
            IfStatement, Initializer, IntegerBase, Statement, UnaryOperator,
            UnaryOperatorExpression, WhileStatement,
        },
        driver::{parse, Config},
    };

    use crate::{BinOp, BranchType, IROp, IRTopLevel, IRValue, UnaryOp};

    struct TopLevelBuilder {
        count: usize,
        ops: Vec<IROp>,
        scope: ScopeInfo,
        loop_id: usize,
        /// If const, use Data instead of Stack
        is_const: bool,
    }

    pub struct FileBuilder {
        count: usize,
        scope: ScopeInfo,
    }

    // TODO: scope system will need a bit of a refactor to make unshadowing of globals possible
    #[derive(Debug, Clone, Default)]
    struct ScopeInfo {
        var_map: HashMap<String, IRValue>,
    }

    impl FileBuilder {
        pub fn parse_c<P: AsRef<Path>>(file: P) -> Result<Vec<IRTopLevel>, lang_c::driver::Error> {
            let mut builder = FileBuilder {
                count: 0,
                scope: ScopeInfo::default(),
            };
            let config = Config::default();
            let parsed = parse(&config, file)?;
            let mut out = vec![];
            for obj in parsed.unit.0.iter() {
                match &obj.node {
                    ExternalDeclaration::Declaration(decl) => {
                        out.push(builder.parse_top_level_declaration(&decl.node))
                    }
                    ExternalDeclaration::StaticAssert(_) => todo!("add static asserts"),
                    ExternalDeclaration::FunctionDefinition(func) => {
                        out.push(builder.parse_function(&func.node))
                    }
                }
            }
            Ok(out)
        }

        fn parse_function(&mut self, func: &FunctionDefinition) -> IRTopLevel {
            let mut builder = TopLevelBuilder {
                ops: vec![],
                count: self.count,
                scope: self.scope.clone(),
                loop_id: 0,
                is_const: false,
            };
            // TODO: deal with specifiers
            // TODO: deal with K&R declarations
            let name = builder.parse_declarator(&func.declarator.node);
            let param_count = builder.parse_func_declarator(&func.declarator.node);
            builder.parse_statement(&func.statement.node);

            // FIXME: bad bad bad, just have a seperate global counter
            self.count = builder.count;
            IRTopLevel {
                name,
                ops: builder.ops,
                parameters: param_count,
                is_initializer: false,
                stack_frame_size: builder.count, // NOTE: this is a 'worst case' value,
                                                 // and should be recalculated in a later pass
            }
        }

        fn parse_top_level_declaration(&mut self, decl: &Declaration) -> IRTopLevel {
            let mut builder = TopLevelBuilder {
                ops: vec![],
                count: self.count,
                scope: self.scope.clone(),
                loop_id: 0,
                is_const: true,
            };

            builder.parse_declarations(&decl);
            self.scope.var_map.extend(builder.scope.var_map);

            // FIXME: bad bad bad, just have a seperate global counter
            self.count = builder.count;
            IRTopLevel {
                name: "top level declarator dont need no name".to_owned(),
                ops: builder.ops,
                parameters: 0,
                is_initializer: true,
                stack_frame_size: builder.count, // NOTE: this is a 'worst case' value,
                                                 // and should be recalculated in a later pass
            }
        }
    }

    struct PsuedoMap {
        map: HashMap<String, usize>,
        count: usize,
    }

    fn pseudo_removal(func: &mut IRTopLevel) {
        let mut map = PsuedoMap {
            map: HashMap::new(),
            count: func.parameters - 1,
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

    pub fn pseudo_removal_pass(funcs: &mut Vec<IRTopLevel>) {
        for func in funcs {
            pseudo_removal(func);
        }
    }

    fn stack_size_recalculator(func: &IRTopLevel) -> usize {
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

    pub fn stack_size_reducer_pass(funcs: &mut Vec<IRTopLevel>) {
        for func in funcs {
            func.stack_frame_size = stack_size_recalculator(func);
        }
    }

    pub fn function_id_mapping_pass(funcs: &Vec<IRTopLevel>) -> HashMap<String, usize> {
        let mut map = HashMap::new();
        let mut i = 2;
        for func in funcs {
            if !func.is_initializer {
                if func.name == "main" {
                    map.insert(func.name.clone(), 1);
                } else {
                    map.insert(func.name.clone(), i);
                    i += 1;
                }
            }
        }
        map
    }

    impl TopLevelBuilder {
        fn parse_func_declarator(&mut self, decl: &Declarator) -> usize {
            let mut count = 1;
            for node in decl.derived.iter() {
                match &node.node {
                    DerivedDeclarator::Function(func_decl) => {
                        // we don't care about func_decl.node.ellipsis
                        for param in func_decl.node.parameters.iter() {
                            // TODO: deal with types in decls
                            if let Some(decl) = param.node.declarator.clone() {
                                let name = self.parse_declarator(&decl.node);
                                let loc = IRValue::Stack(count);
                                count += 1;
                                self.scope.var_map.insert(name, loc.clone());
                            }
                        }
                    }
                    DerivedDeclarator::KRFunction(_) => todo!("KR functions"),
                    _ => panic!("non func declarator in the func declarator parser??"),
                }
            }
            count
        }

        fn parse_declarator(&self, decl: &Declarator) -> String {
            match &decl.kind.node {
                DeclaratorKind::Identifier(node) => node.node.name.clone(),
                _ => todo!("non trivial identifier functions"),
            }
        }

        fn push(&mut self, op: IROp) {
            self.ops.push(op);
        }

        fn parse_statement(&mut self, stmt: &Statement) {
            match stmt {
                Statement::Return(maybe_expr) => {
                    if let Some(expr) = maybe_expr {
                        let value = self.parse_expression(&expr.node);
                        self.push(IROp::Return(value));
                    } else {
                        self.push(IROp::Return(IRValue::Immediate(0)));
                    };
                }
                Statement::Compound(blocks) => {
                    let old_scope = self.scope.clone();
                    for block_item in blocks {
                        self.parse_block_item(&block_item.node);
                    }
                    self.scope = old_scope;
                }
                Statement::Expression(Some(expr)) => {
                    self.parse_expression(&expr.node);
                }
                Statement::Expression(None) => (),
                Statement::If(stmt) => self.parse_if(&stmt.node),
                Statement::While(stmt) => self.parse_while_stmt(&stmt.node),
                Statement::DoWhile(stmt) => self.parse_do_while_stmt(&stmt.node),
                Statement::For(stmt) => self.parse_for_statement(&stmt.node),
                Statement::Break => self.parse_break(),
                Statement::Continue => self.parse_continue(),
                Statement::Labeled(_) | Statement::Goto(_) => todo!("goto"),
                Statement::Switch(_) => todo!("switch statements"),
                // If feeling freaky with it, asm statements could allow arbitrary befunge...
                // Probably just not gonna implement it tho lol
                Statement::Asm(_) => todo!("asm statements"),
            }
        }

        fn parse_break(&mut self) {
            let (loop_end_lbl_str, _) = self.generate_loop_end_label();
            self.push(IROp::AlwaysBranch(loop_end_lbl_str))
        }

        fn parse_continue(&mut self) {
            let (loop_cont_lbl_str, _) = self.generate_loop_continue_label();
            self.push(IROp::AlwaysBranch(loop_cont_lbl_str))
        }

        fn parse_while_stmt(&mut self, stmt: &WhileStatement) {
            let prev_id = self.loop_id;
            self.new_loop_id();
            let (loop_lbl_str, loop_lbl) = self.generate_loop_label();
            let (loop_end_lbl_str, loop_end_lbl) = self.generate_loop_end_label();

            self.push(loop_lbl);
            let cond = self.parse_expression(&stmt.expression.node);
            self.push(IROp::CondBranch(BranchType::Zero, loop_end_lbl_str, cond));
            self.parse_statement(&stmt.statement.node);
            self.push(IROp::AlwaysBranch(loop_lbl_str));
            self.push(loop_end_lbl);
            self.loop_id = prev_id;
        }

        fn parse_do_while_stmt(&mut self, stmt: &DoWhileStatement) {
            let prev_id = self.loop_id;
            self.new_loop_id();
            let (loop_lbl_str, loop_lbl) = self.generate_loop_label();

            self.push(loop_lbl);
            self.parse_statement(&stmt.statement.node);
            let cond = self.parse_expression(&stmt.expression.node);
            self.push(IROp::CondBranch(BranchType::NonZero, loop_lbl_str, cond));
            self.loop_id = prev_id;
        }

        fn parse_for_statement(&mut self, stmt: &ForStatement) {
            let (loop_lbl_str, loop_lbl) = self.generate_loop_label();
            let (loop_end_lbl_str, loop_end_lbl) = self.generate_loop_end_label();
            let (_, loop_cont_lbl) = self.generate_loop_continue_label();

            self.parse_for_initializer(&stmt.initializer.node);
            self.push(loop_lbl);
            if let Some(cond) = &stmt.condition {
                let cond = self.parse_expression(&cond.node);
                self.push(IROp::CondBranch(BranchType::Zero, loop_end_lbl_str, cond));
            }
            self.parse_statement(&stmt.statement.node);
            self.push(loop_cont_lbl);
            if let Some(step) = &stmt.step {
                self.parse_expression(&step.node);
            }
            self.push(IROp::AlwaysBranch(loop_lbl_str));
            self.push(loop_end_lbl);
        }

        fn parse_for_initializer(&mut self, init: &ForInitializer) {
            match init {
                ForInitializer::Empty => (),
                ForInitializer::Expression(expr) => {
                    self.parse_expression(&expr.node);
                }
                ForInitializer::Declaration(decls) => self.parse_declarations(&decls.node),
                ForInitializer::StaticAssert(_) => todo!("static assert in for init"),
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

        fn parse_block_item(&mut self, block: &BlockItem) {
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
            for decl in decls.declarators.clone() {
                let name = self.parse_declarator(&decl.node.declarator.node);
                let loc = if self.is_const {
                    self.count += 1;
                    IRValue::Data(self.count)
                } else {
                    self.generate_named_pseudo(name.clone())
                };
                self.scope.var_map.insert(name, loc.clone());

                if let Some(init) = decl.node.initializer {
                    let init = self.parse_initializer(&init.node);
                    self.push(IROp::One(UnaryOp::Copy, init, loc));
                } else if self.is_const {
                    // If we are in a declarator we must init un-inited values to zero.
                    // This impl may be overzealous, but as using the value of an
                    // un-inited value is UB, this is okay
                    self.push(IROp::One(UnaryOp::Copy, IRValue::Immediate(0), loc));
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
                Expression::Identifier(ident) => match self.scope.var_map.get(&ident.node.name) {
                    None => panic!("ident {ident:?} should exist. scope: {:?}", self.scope),
                    Some(val) => val.clone(),
                },
                Expression::Call(call_expr) => self.parse_call(&call_expr.node),

                // Type stuff
                Expression::SizeOfTy(_) => todo!("SizeOfTy {expr:?}"),
                Expression::SizeOfVal(_) => todo!("SizeOfVal {expr:?}"),
                Expression::AlignOf(_) => todo!("AlignOf {expr:?}"),
                Expression::Cast(_) => todo!("Cast {expr:?}"),

                // Struct stuff
                Expression::Member(_) => todo!("Member {expr:?}"),
                Expression::OffsetOf(_) => todo!("OffsetOf {expr:?}"),

                // Other stuff
                Expression::StringLiteral(_) => todo!("StringLiteral {expr:?}"),
                Expression::GenericSelection(_) => todo!("GenericSelection {expr:?}"),
                Expression::CompoundLiteral(_) => todo!("CompoundLiteral {expr:?}"),
                Expression::Conditional(_) => todo!("Conditional {expr:?}"),
                Expression::Comma(_) => todo!("Comma {expr:?}"),
                Expression::VaArg(_) => todo!("VaArg {expr:?}"),

                // Extension
                Expression::Statement(_) => todo!("Statement {expr:?}"),
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
                _ => todo!("non trival ident calls"),
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
                UnaryOperator::Negate => IROp::One(UnaryOp::BooleanNegate, val, out.clone()),
                UnaryOperator::Plus => IROp::One(UnaryOp::Copy, val, out.clone()), // silly

                // x++
                UnaryOperator::PostIncrement => todo!("PostIncrement {expr:?}"),
                UnaryOperator::PostDecrement => todo!("PostDecrement {expr:?}"),

                // ++x
                UnaryOperator::PreIncrement => todo!("PreIncrement {expr:?}"),
                UnaryOperator::PreDecrement => todo!("PreDecrement {expr:?}"),

                // Memory stuff
                UnaryOperator::Address => todo!("Address {expr:?}"),
                UnaryOperator::Indirection => todo!("Indirection {expr:?}"),
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
            let lhs2 = lhs.clone();
            self.push(match expr.operator.node {
                BinaryOperator::Plus | BinaryOperator::AssignPlus => {
                    IROp::Two(BinOp::Add, lhs, rhs, out.clone())
                }
                BinaryOperator::Minus | BinaryOperator::AssignMinus => {
                    IROp::Two(BinOp::Sub, lhs, rhs, out.clone())
                }
                BinaryOperator::Multiply | BinaryOperator::AssignMultiply => {
                    IROp::Two(BinOp::Mult, lhs, rhs, out.clone())
                }
                BinaryOperator::Divide | BinaryOperator::AssignDivide => {
                    IROp::Two(BinOp::Div, lhs, rhs, out.clone())
                }
                BinaryOperator::Modulo | BinaryOperator::AssignModulo => {
                    IROp::Two(BinOp::Mod, lhs, rhs, out.clone())
                }

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
                BinaryOperator::ShiftLeft | BinaryOperator::AssignShiftLeft => {
                    todo!("ShiftLeft {lhs:?} {rhs:?}")
                }
                BinaryOperator::ShiftRight | BinaryOperator::AssignShiftRight => {
                    todo!("ShiftRight {lhs:?} {rhs:?}")
                }
                BinaryOperator::BitwiseAnd | BinaryOperator::AssignBitwiseAnd => {
                    todo!("BitwiseAnd {lhs:?} {rhs:?}")
                }
                BinaryOperator::BitwiseXor | BinaryOperator::AssignBitwiseXor => {
                    todo!("BitwiseXor {lhs:?} {rhs:?}")
                }
                BinaryOperator::BitwiseOr | BinaryOperator::AssignBitwiseOr => {
                    todo!("BitwiseOr {lhs:?} {rhs:?}")
                }

                BinaryOperator::Assign => IROp::One(UnaryOp::Copy, rhs, lhs),

                // FREAKY SHORT CIRCUITING OPS!
                BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => unreachable!(),
            });

            if matches!(
                expr.operator.node,
                BinaryOperator::AssignPlus
                    | BinaryOperator::AssignMinus
                    | BinaryOperator::AssignMultiply
                    | BinaryOperator::AssignDivide
                    | BinaryOperator::AssignModulo
                    | BinaryOperator::AssignShiftLeft
                    | BinaryOperator::AssignShiftRight
                    | BinaryOperator::AssignBitwiseAnd
                    | BinaryOperator::AssignBitwiseXor
                    | BinaryOperator::AssignBitwiseOr
            ) {
                self.push(IROp::One(UnaryOp::Copy, out.clone(), lhs2));
            };
            out
        }

        fn parse_constant(&self, val: &Constant) -> IRValue {
            match val {
                Constant::Integer(int) => {
                    let x: usize = match int.base {
                        IntegerBase::Decimal => usize::from_str_radix(&int.number, 10).unwrap(),
                        IntegerBase::Octal => usize::from_str_radix(&int.number, 8).unwrap(),
                        IntegerBase::Hexadecimal => usize::from_str_radix(&int.number, 16).unwrap(),
                        IntegerBase::Binary => usize::from_str_radix(&int.number, 2).unwrap(),
                    };
                    IRValue::Immediate(x)
                }
                _ => todo!("Non integer constants {:?}", val),
            }
        }

        fn generate_pseudo(&mut self) -> IRValue {
            self.count += 1;
            IRValue::Psuedo("tmp.".to_owned() + &self.count.to_string())
        }

        fn generate_named_pseudo(&mut self, name: String) -> IRValue {
            self.count += 1;
            IRValue::Psuedo(name + "." + &self.count.to_string())
        }

        fn generate_label(&mut self, str: &str) -> (String, IROp) {
            self.count += 1;
            let str = str.to_owned() + "." + &self.count.to_string();
            (str.clone(), IROp::Label(str))
        }

        fn generate_loop_label(&mut self) -> (String, IROp) {
            let str = "loop".to_owned() + &self.loop_id.to_string();
            (str.clone(), IROp::Label(str))
        }

        fn generate_loop_end_label(&mut self) -> (String, IROp) {
            let str = "loop_end".to_owned() + &self.loop_id.to_string();
            (str.clone(), IROp::Label(str))
        }

        fn generate_loop_continue_label(&mut self) -> (String, IROp) {
            let str = "loop_continue".to_owned() + &self.loop_id.to_string();
            (str.clone(), IROp::Label(str))
        }

        fn new_loop_id(&mut self) -> usize {
            let x = self.count;
            self.loop_id = x;
            self.count += 1;
            x
        }
    }
}

fn print_ir(ir: &Vec<IRTopLevel>) {
    for func in ir {
        if !func.is_initializer {
            println!("\nFUNC {:?}", func.name);
        } else {
            println! {"\nINIT REGION"};
        }
        for line in &func.ops {
            println!("{line:?}");
        }
    }
}

fn main() {
    let x = std::fs::read_to_string("source.c").expect("Unable to read file");
    println!("{x}");
    let mut program = FileBuilder::parse_c("source.c").unwrap();
    println!("-- IR, PRE PASSES");
    print_ir(&program);

    pseudo_removal_pass(&mut program);
    stack_size_reducer_pass(&mut program);
    let function_map = function_id_mapping_pass(&program);
    println!("{function_map:?}");

    println!("-- IR, POST PASSES");
    print_ir(&program);

    println!("-- befunge begin");
    let mut cg = CodeGen {
        builder: OpBuilder::new(false),
        function_map,
    };

    let mut out: Vec<String> = vec![];
    out.extend(PRELUDE.lines().map(ToOwned::to_owned));
    let mut funcs: Vec<String> = vec![];
    let mut inits: Vec<String> = vec![];
    for func in program {
        let init = func.is_initializer;
        let x = cg.compile_top_level(func);
        if init {
            inits.extend(x);
        } else {
            funcs.extend(x);
        }
    }
    out.extend(inits);
    out.extend(PRELUDE2.lines().map(ToOwned::to_owned));
    out.extend(funcs);
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

// TODO: use seperate global counter for global values
// TODO: bitwise ops (going to be strange, as befunge has no bit level operators)
// TODO: increment and decrement
// TODO: for switch statements contine & break tracking must be done seperately (as switch can be breaked but not continued)
// TODO: goto
//
//
// NOTE: Some custom printing thing is going to be needed for every value that isn't signed int(/long?)
// NOTE: When implementing unsigned ints, gonna need to have custom impls for some things
//        likely mult, divide and modulo (as befunge ops are signed).
