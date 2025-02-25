#![allow(
    clippy::needless_pass_by_ref_mut,
    clippy::needless_raw_string_hashes,
    clippy::needless_raw_strings
)]

use std::collections::HashMap;

use builder::OpBuilder;
use c_compiler::{
    function_id_mapping_pass, pseudo_removal_pass, sort_functions_pass, stack_size_reducer_pass,
    the_linkening, FileBuilder,
};

mod builder;

static PRE_INIT_PRELUDE: &str = r##"v!R#######
v#########    main stack ->      !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ...etc
v#########    call stack ->      !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ...etc
v######### static memory ->      !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ...etc
v#########        // IN FUTURE, malloc() mem could be a 4th mem location here
v#########
v#########  below are the bit stacks, for bitshifts and bitwise operations
v#########  the last 64 bits are just zeros for cheaper bitshifts
v bit stack A
v bit stack B
>"!"00p 010g2-2pv
v               <
>0>:0\9p:0\8p1+v
  ^_v#-+"@A":  <
v $ <

"##;

pub static POST_INIT_PRELUDE: &str = "0
1
>v
 >:#v_  $$ 20g . @
 v  <";

use clap::Parser;

#[derive(Parser, Debug)]
#[command(about="A C compiler that outputs befunge93 instead of assembly.", long_about = None)]
struct Args {
    /// File to compile
    filename: String,

    /// Print extra info about compilation
    #[arg(short, long)]
    verbose: bool,

    /// Don't print the output program
    #[arg(short, long)]
    silent: bool,

    /// File to write program to
    #[arg(short, long)]
    outfile: Option<String>,

    /// Add preprocessor info to the bottom for `BefunExec`
    #[arg(short, long)]
    preprocessor_info: bool,
}

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

#[derive(Debug, Copy, Clone)]
enum CType {
    UnsignedInt,
    UnsignedLong,
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
struct IRTopLevel {
    name: String,
    stack_frame_size: usize,
    parameters: usize,
    ops: Vec<IROp>,
    is_initializer: bool,
}

struct CodeGen {
    builder: OpBuilder,
    function_map: HashMap<String, FuncInfo>,
}

#[derive(Debug, Clone, Copy)]
struct FuncInfo {
    stack_frame_size: usize,
    id: usize,
}

impl CodeGen {
    fn compile_top_level(&mut self, func: IRTopLevel) -> Vec<String> {
        self.builder = OpBuilder::new(!func.is_initializer);
        for op in func.ops {
            match &op {
                IROp::FunctionLabel(_) => (),
                IROp::Call(called_func_name, vals) => {
                    assert!(!func.is_initializer, "Non static call in static context");
                    // TODO: improve error on unknown func call
                    let called_func = self.function_map[called_func_name];

                    self.builder
                        .call(self.function_map[&func.name], called_func, vals);
                }
                IROp::Return(val) => {
                    self.builder.return_(val, func.stack_frame_size);
                }
                IROp::Label(label) => self.builder.label(label.to_owned()),
                IROp::InlineBefunge(lines) => self.builder.insert_inline_befunge(lines),
                IROp::CondBranch(flavour, label, val) => match flavour {
                    BranchType::Zero => self.builder.zero_branch(&val, label.to_string()),
                    BranchType::NonZero => self.builder.not_zero_branch(&val, label.to_string()),
                },
                IROp::AlwaysBranch(label) => self.builder.unconditional_branch(label.to_owned()),
                IROp::One(op, a, out) => {
                    match op {
                        UnaryOp::Copy => self.builder.copy(a, &IRValue::BefungeStack),
                        UnaryOp::Minus => self.builder.unary_minus(a),
                        UnaryOp::Complement => self.builder.bitwise_complement(a),
                        UnaryOp::BooleanNegate => self.builder.boolean_negate(a),
                    }
                    self.builder.copy(&IRValue::BefungeStack, out);
                }
                IROp::Two(op, a, b, out) => {
                    match op {
                        BinOp::Add => self.builder.add(a, b),
                        BinOp::Sub => self.builder.sub(a, b),
                        BinOp::Mult => self.builder.multiply(a, b),
                        BinOp::Div => self.builder.divide(a, b),
                        BinOp::Mod => self.builder.modulo(a, b),
                        BinOp::Equal => self.builder.is_equal(a, b),
                        BinOp::NotEqual => self.builder.is_not_equal(a, b),
                        BinOp::LessThan => self.builder.is_less_than(a, b),
                        BinOp::LessOrEqual => self.builder.is_less_or_equal(a, b),
                        BinOp::GreaterThan => self.builder.is_greater_than(a, b),
                        BinOp::GreaterOrEqual => self.builder.is_greater_or_equal(a, b),

                        BinOp::BitwiseAnd => self.builder.bit_and(a, b),
                        BinOp::BitwiseOr => self.builder.bit_or(a, b),
                        BinOp::BitwiseXor => self.builder.bit_xor(a, b),

                        BinOp::ShiftLeft => {
                            self.builder.bitshift_left(a, b);
                        }
                        BinOp::ShiftRight => {
                            self.builder.bitshift_right(a, b);
                        }
                    }
                    self.builder.copy(&IRValue::BefungeStack, out);
                }
            }
            self.builder.add_space();
        }

        self.builder.finalize_function()
    }
}

mod c_compiler {

    use std::{collections::HashMap, mem, path::Path};

    use lang_c::{
        ast::{
            AsmStatement, BinaryOperator, BinaryOperatorExpression, BlockItem, CallExpression,
            CastExpression, ConditionalExpression, Constant, Declaration, DeclarationSpecifier,
            Declarator, DeclaratorKind, DerivedDeclarator, DoWhileStatement, Expression,
            ExternalDeclaration, ForInitializer, ForStatement, FunctionDefinition, GnuAsmOperand,
            Identifier, IfStatement, Initializer, IntegerBase, Label, LabeledStatement, Statement,
            StorageClassSpecifier, SwitchStatement, TypeSpecifier, UnaryOperator,
            UnaryOperatorExpression, WhileStatement,
        },
        driver::{parse, Config},
        span::Node,
    };

    use crate::{BinOp, BranchType, CType, FuncInfo, IROp, IRTopLevel, IRValue, UnaryOp};

    struct TopLevelBuilder<'a> {
        count: usize,
        ops: Vec<IROp>,
        scope: ScopeInfo,
        break_last_seen: BreakTypes,
        loop_id: Option<usize>,
        switch_case_info: Option<SwitchCaseInfo>,
        /// If const (not inside a function), use Data instead of Stack
        is_const: bool,
        file_builder: &'a mut FileBuilder,
    }

    #[derive(Clone, Copy)]
    enum BreakTypes {
        SwitchCase,
        Loop,
        None,
    }

    #[derive(Clone)]
    struct SwitchCaseInfo {
        id: usize,
        // FIXME: will also need to express ranges :)
        cases: Vec<Box<Node<Expression>>>,
        has_default: bool,
    }

    impl SwitchCaseInfo {
        const fn new(id: usize) -> Self {
            Self {
                id,
                cases: vec![],
                has_default: false,
            }
        }
    }

    pub struct FileBuilder {
        count: usize,
        scope: ScopeInfo,
        funcs: Vec<IRTopLevel>,
    }

    // TODO: scope system will need a bit of a refactor to make unshadowing of globals possible
    #[derive(Debug, Clone, Default)]
    struct ScopeInfo {
        var_map: HashMap<String, IRValue>,
    }

    impl FileBuilder {
        pub fn parse_c<P: AsRef<Path>>(file: P) -> Result<Vec<IRTopLevel>, lang_c::driver::Error> {
            let mut builder = Self {
                count: 0,
                scope: ScopeInfo::default(),
                funcs: vec![],
            };
            let config = Config::default();
            let parsed = parse(&config, file)?;
            for obj in &parsed.unit.0 {
                match &obj.node {
                    ExternalDeclaration::Declaration(decl) => {
                        let x = builder.parse_top_level_declaration(&decl.node);
                        builder.funcs.push(x);
                    }
                    ExternalDeclaration::StaticAssert(_) => todo!("add static asserts"),
                    ExternalDeclaration::FunctionDefinition(func) => {
                        let x = builder.parse_function(&func.node);
                        builder.funcs.push(x);
                    }
                }
            }
            Ok(builder.funcs)
        }

        fn parse_function(&mut self, func: &FunctionDefinition) -> IRTopLevel {
            let mut builder = TopLevelBuilder {
                ops: vec![],
                count: self.count,
                scope: self.scope.clone(),
                loop_id: None,
                break_last_seen: BreakTypes::None,
                is_const: false,
                switch_case_info: None,
                file_builder: self,
            };
            // TODO: deal with specifiers
            // TODO: deal with K&R declarations
            let name = builder.parse_declarator(&func.declarator.node);

            let param_count = builder.parse_func_declarator(&func.declarator.node);
            builder.parse_statement(&func.statement.node);
            builder.push(IROp::Return(IRValue::Immediate(0)));

            // FIXME: bad bad bad, just have a seperate global counter
            builder.file_builder.count = builder.count;
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
                loop_id: None,
                break_last_seen: BreakTypes::None,
                is_const: true,
                switch_case_info: None,
                file_builder: self,
            };

            builder.parse_declarations(decl);
            builder
                .file_builder
                .scope
                .var_map
                .extend(builder.scope.var_map);

            // FIXME: bad bad bad, just have a seperate global counter
            builder.file_builder.count = builder.count;
            IRTopLevel {
                name: String::new(),
                ops: builder.ops,
                parameters: 0,
                is_initializer: true,
                stack_frame_size: builder.count, // NOTE: this is a 'worst case' value,
                                                 // and should be recalculated in a later pass
            }
        }
    }

    struct PsuedoMap<'a> {
        stack_map: HashMap<String, usize>,
        stack_count: usize,
        data_count: &'a mut usize,
        data_map: &'a mut HashMap<String, usize>,
    }

    fn apply_to_all_ir_values(ops: &mut IRTopLevel, func: &mut impl FnMut(&mut IRValue)) {
        for i in 0..ops.ops.len() {
            match &mut ops.ops[i] {
                IROp::Return(o) => func(o),
                IROp::Call(_, ops) => ops.into_iter().for_each(|x| func(x)),
                IROp::CondBranch(_, _, a) => func(a),
                IROp::One(_, a, out) => {
                    func(a);
                    func(out);
                }
                IROp::Two(_, a, b, out) => {
                    func(a);
                    func(b);
                    func(out);
                }
                _ => (),
            }
        }
    }

    fn pseudo_removal(
        func: &mut IRTopLevel,
        data_count: &mut usize,
        data_map: &mut HashMap<String, usize>,
    ) {
        let mut map = PsuedoMap {
            stack_map: HashMap::new(),
            stack_count: func.parameters,
            data_count,
            data_map,
        };
        let mut get = |val: &mut IRValue| *val = map.get(val);
        apply_to_all_ir_values(func, &mut get);
    }

    impl PsuedoMap<'_> {
        fn get(&mut self, val: &IRValue) -> IRValue {
            if let IRValue::Psuedo { name } = val {
                if let Some(id) = self.stack_map.get(name) {
                    return IRValue::Stack(*id);
                } else {
                    self.stack_count += 1;
                    self.stack_map.insert(name.to_owned(), self.stack_count);
                    return IRValue::Stack(self.stack_count);
                }
            }
            if let IRValue::StaticPsuedo { name, .. } = val {
                if let Some(id) = self.data_map.get(name) {
                    return IRValue::Data(*id);
                } else {
                    *self.data_count += 1;
                    self.data_map.insert(name.to_owned(), *self.data_count);
                    return IRValue::Data(*self.data_count);
                }
            }
            val.clone()
        }
    }

    pub fn pseudo_removal_pass(funcs: &mut Vec<IRTopLevel>) {
        let mut data_count = 0;
        let mut data_map = HashMap::new();
        for func in funcs {
            pseudo_removal(func, &mut data_count, &mut data_map);
        }
    }

    pub fn the_linkening(files: Vec<Vec<IRTopLevel>>) -> Vec<IRTopLevel> {
        let mut out = vec![];
        for (i, file) in files.into_iter().enumerate() {
            for mut func in file {
                append_to_all_psuedos(&mut func, &(".".to_owned() + &i.to_string()));
                out.push(func);
            }
        }
        out
    }

    fn append_to_all_psuedos(func: &mut IRTopLevel, new_suffix: &str) {
        apply_to_all_ir_values(func, &mut |value: &mut IRValue| {
            append_to_ir_value(value, new_suffix)
        });
    }

    fn append_to_ir_value(value: &mut IRValue, new_suffix: &str) {
        match value {
            IRValue::StaticPsuedo {
                name,
                linkable: false,
            }
            | IRValue::Psuedo { name } => {
                *name = name.clone() + new_suffix;
            }
            _ => (),
        }
    }

    fn stack_size_recalculator(func: &mut IRTopLevel) -> usize {
        // find biggest Stack value
        let mut counter = 0;
        let mut check = |val: &mut IRValue| {
            if let IRValue::Stack(val) = val {
                if *val > counter {
                    counter = *val;
                }
            }
        };
        apply_to_all_ir_values(func, &mut check);
        std::cmp::max(counter, func.parameters)
    }

    pub fn stack_size_reducer_pass(funcs: &mut Vec<IRTopLevel>) {
        for func in funcs {
            func.stack_frame_size = stack_size_recalculator(func);
        }
    }

    pub fn function_id_mapping_pass(funcs: &[IRTopLevel]) -> HashMap<String, FuncInfo> {
        let mut map = HashMap::new();
        let mut i = 1;
        for func in funcs {
            if !func.is_initializer {
                map.insert(
                    func.name.clone(),
                    FuncInfo {
                        id: i,
                        stack_frame_size: func.stack_frame_size,
                    },
                );
                i += 1;
            }
        }
        map
    }

    pub fn sort_functions_pass(funcs: Vec<IRTopLevel>) -> Vec<IRTopLevel> {
        let mut out = vec![None];
        for func in funcs {
            if func.name == "main" {
                assert!(out[0].is_none(), "Only one main function exists");
                out[0] = Some(func);
            } else {
                out.push(Some(func));
            }
        }
        out.into_iter()
            .map(|x| x.expect("At least one main function should exist"))
            .collect()
    }

    impl CType {
        fn from(types: &[&Node<TypeSpecifier>]) -> Self {
            let types = types.iter().map(|x| x.node.clone()).collect::<Vec<_>>();
            match types[..] {
                [] => panic!("No type specifiers?"),
                [TypeSpecifier::Int | TypeSpecifier::Signed]
                | [TypeSpecifier::Signed, TypeSpecifier::Int] => Self::UnsignedInt,
                [TypeSpecifier::Long]
                | [TypeSpecifier::Long, TypeSpecifier::Int]
                | [TypeSpecifier::Signed, TypeSpecifier::Long, TypeSpecifier::Int] => {
                    Self::UnsignedLong
                }
                _ => panic!("Unknown type: {types:?}"),
            }
        }
    }

    #[derive(Debug)]
    enum StorageDuration {
        Default,
        Extern,
        Static,
    }

    #[derive(Debug)]
    struct DeclarationInfo {
        c_type: CType,
        duration: StorageDuration,
    }

    impl DeclarationInfo {
        fn from(specifiers: &[Node<DeclarationSpecifier>]) -> Self {
            let mut c_types = vec![];
            let mut duration: StorageDuration = StorageDuration::Default;
            // this allows for loads of invalid code, like
            // `signed extern int x = 5;` because we don't validate that all types appear
            // sequentially without breaks
            // And extern static x will be accepted as static x
            for specifier in specifiers {
                match &specifier.node {
                    DeclarationSpecifier::StorageClass(spec) => match spec.node {
                        StorageClassSpecifier::Extern => duration = StorageDuration::Extern,
                        StorageClassSpecifier::Static => duration = StorageDuration::Static,
                        _ => println!(
                            "WARNING: Some storage class specifiers are ignored {specifier:?}"
                        ),
                    },
                    DeclarationSpecifier::TypeSpecifier(spec) => c_types.push(spec),
                    // Consider implementing just volatile
                    DeclarationSpecifier::TypeQualifier(_) => {
                        println!("WARNING: All type qualifiers are ignored {specifier:?}");
                    }
                    DeclarationSpecifier::Function(_) => {
                        panic!("function specifier on variable declaration")
                    }
                    DeclarationSpecifier::Alignment(_) => println!(
                        "WARNING: All declaration alignment specifiers are ignored {specifier:?}"
                    ),
                    DeclarationSpecifier::Extension(_) => println!(
                        "WARNING: GNU declaration specifier extensions are ignored {specifier:?}"
                    ),
                };
            }

            Self {
                duration,
                c_type: CType::from(&c_types),
            }
        }
    }

    impl TopLevelBuilder<'_> {
        fn parse_func_declarator(&mut self, decl: &Declarator) -> usize {
            let mut count = 1;
            for node in &decl.derived {
                match &node.node {
                    DerivedDeclarator::Function(func_decl) => {
                        // we don't care about func_decl.node.ellipsis
                        for param in &func_decl.node.parameters {
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
            count - 1
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
                Statement::Labeled(stmt) => self.parse_label_stmt(&stmt.node),
                Statement::Goto(stmt) => self.parse_goto_stmt(&stmt.node),
                Statement::Switch(stmt) => self.parse_switch(&stmt.node),
                Statement::Asm(stmt) => self.parse_asm(&stmt.node),
            }
        }

        fn parse_switch(&mut self, switch: &SwitchStatement) {
            let condition = self.parse_expression(&switch.expression.node);

            let prev_seen = self.break_last_seen;
            self.break_last_seen = BreakTypes::SwitchCase;

            let old_scope = self.scope.clone();
            let mut old_switch_case_info = self.switch_case_info.clone();

            self.new_switch_case_id();

            // NOTE: This is a sort of dirty hack, not super happy with it.
            // We do all the parsing of the switch statement in a new array of ops,
            // which we'll stick onto the end after the branching logic
            let mut ops = vec![];
            mem::swap(&mut self.ops, &mut ops);
            self.parse_statement(&switch.statement.node);
            mem::swap(&mut self.ops, &mut ops);

            mem::swap(&mut self.switch_case_info, &mut old_switch_case_info);

            let info = old_switch_case_info.unwrap();
            let id = info.id;

            let tmp = self.generate_pseudo();
            for (i, expr) in info.cases.iter().enumerate() {
                let case_value = self.parse_expression(&expr.node);
                self.push(IROp::Two(
                    BinOp::Equal,
                    case_value,
                    condition.clone(),
                    tmp.clone(),
                ));
                let lbl = self.generate_switch_case_label(id, i).0;
                self.push(IROp::CondBranch(BranchType::NonZero, lbl, tmp.clone()));
            }

            if info.has_default {
                let lbl = self.generate_switch_case_default_label(id).0;
                self.push(IROp::AlwaysBranch(lbl));
            } else {
                let lbl = self.generate_switch_case_end_label(id).0;
                self.push(IROp::AlwaysBranch(lbl));
            }

            self.ops.extend(ops);
            let lbl = self.generate_switch_case_end_label(id).1;
            self.push(lbl);

            self.scope = old_scope;
            self.break_last_seen = prev_seen;
        }

        fn parse_goto_stmt(&mut self, ident: &Identifier) {
            self.push(IROp::AlwaysBranch(ident.name.clone() + ".goto"));
        }

        fn parse_label_stmt(&mut self, stmt: &LabeledStatement) {
            match &stmt.label.node {
                Label::Identifier(lbl) => self.push(IROp::Label(lbl.node.name.clone() + ".goto")),
                Label::Case(expr) => {
                    let id = self.switch_case_info.as_ref().unwrap().id;
                    let count = self.switch_case_info.as_ref().unwrap().cases.len();
                    let lbl = self.generate_switch_case_label(id, count).1;
                    self.push(lbl);
                    if let Some(ref mut info) = &mut self.switch_case_info {
                        info.cases.push(expr.clone());
                    } else {
                        panic!("Switch case outside of switch case D:")
                    }
                }
                Label::Default => {
                    let id = self.switch_case_info.as_ref().unwrap().id;
                    let lbl = self.generate_switch_case_default_label(id).1;
                    self.push(lbl);
                    self.switch_case_info.as_mut().unwrap().has_default = true;
                }
                Label::CaseRange(..) => todo!("GNU extension: case range"),
            }
            self.parse_statement(&stmt.statement.node);
        }

        fn parse_asm(&mut self, asm: &AsmStatement) {
            // TODO: ban newline char? it won't really work right
            match asm {
                AsmStatement::GnuBasic(asm) => {
                    let lines = cleanup_parsed_asm(&asm.node);
                    self.push(IROp::InlineBefunge(lines));
                }
                AsmStatement::GnuExtended(asm) => {
                    for input in &asm.inputs {
                        self.parse_asm_operand(&input.node, true);
                    }
                    // note it's supposed to be a "template", but it is
                    // difficult to do sensible befunge templating that still supports
                    // multiple lines, so we won't, instead the inputs/outputs
                    // will define what values are loaded where
                    let lines = cleanup_parsed_asm(&asm.template.node);
                    self.push(IROp::InlineBefunge(lines));
                    for output in &asm.outputs {
                        self.parse_asm_operand(&output.node, false);
                    }

                    for clobbers in &asm.clobbers {
                        println!("WARNING: asm clobbers are ignored {clobbers:?}");
                    }
                }
            }
        }

        fn parse_asm_operand(&mut self, op: &GnuAsmOperand, input: bool) {
            if let Some(output_name) = &op.symbolic_name {
                let c_value = self.parse_expression(&op.variable_name.node);
                let asm_value = Self::parse_asm_symbolic(&output_name.node.name);
                if input {
                    self.push(IROp::One(UnaryOp::Copy, c_value, asm_value));
                } else {
                    self.push(IROp::One(UnaryOp::Copy, asm_value, c_value));
                }
            }
        }

        fn parse_asm_symbolic(str: &str) -> IRValue {
            if let Some(rest) = str.strip_prefix('r') {
                IRValue::Register(rest.parse().unwrap())
            } else if str == "bstack" {
                IRValue::BefungeStack
            } else {
                panic!("Invalid symbolic value");
            }
        }

        fn parse_break(&mut self) {
            match self.break_last_seen {
                BreakTypes::None => panic!("nothing to break!"),
                BreakTypes::Loop => {
                    let lbl = self.generate_loop_break_label().0;
                    self.push(IROp::AlwaysBranch(lbl));
                }
                BreakTypes::SwitchCase => {
                    let id = self.switch_case_info.as_ref().unwrap().id;
                    let lbl = self.generate_switch_case_end_label(id).0;
                    self.push(IROp::AlwaysBranch(lbl));
                }
            }
        }

        fn parse_continue(&mut self) {
            let (loop_cont_lbl_str, _) = self.generate_loop_continue_label();
            self.push(IROp::AlwaysBranch(loop_cont_lbl_str));
        }

        fn parse_while_stmt(&mut self, stmt: &WhileStatement) {
            let prev_scope = self.scope.clone();
            let prev_seen = self.break_last_seen;
            self.break_last_seen = BreakTypes::Loop;
            let prev_id = self.loop_id;
            self.new_loop_id();
            let (loop_lbl_str, loop_lbl) = self.generate_loop_continue_label();
            let (loop_end_lbl_str, loop_end_lbl) = self.generate_loop_break_label();

            self.push(loop_lbl);
            let cond = self.parse_expression(&stmt.expression.node);
            self.push(IROp::CondBranch(BranchType::Zero, loop_end_lbl_str, cond));
            self.parse_statement(&stmt.statement.node);
            self.push(IROp::AlwaysBranch(loop_lbl_str));
            self.push(loop_end_lbl);
            self.loop_id = prev_id;
            self.break_last_seen = prev_seen;
            self.scope = prev_scope;
        }

        fn parse_do_while_stmt(&mut self, stmt: &DoWhileStatement) {
            let prev_scope = self.scope.clone();
            let prev_seen = self.break_last_seen;
            self.break_last_seen = BreakTypes::Loop;
            let prev_id = self.loop_id;
            self.new_loop_id();
            let (loop_lbl_str, loop_lbl) = self.generate_loop_continue_label();
            let (_, loop_end_lbl) = self.generate_loop_break_label();

            self.push(loop_lbl);
            self.parse_statement(&stmt.statement.node);
            let cond = self.parse_expression(&stmt.expression.node);
            self.push(IROp::CondBranch(BranchType::NonZero, loop_lbl_str, cond));
            self.push(loop_end_lbl);
            self.loop_id = prev_id;
            self.break_last_seen = prev_seen;
            self.scope = prev_scope;
        }

        fn parse_for_statement(&mut self, stmt: &ForStatement) {
            let old_scope = self.scope.clone();
            let prev_seen = self.break_last_seen;
            self.break_last_seen = BreakTypes::Loop;
            let prev_id = self.loop_id;
            self.new_loop_id();
            let (start_lbl_str, start_lbl) = self.generate_loop_label();
            let (break_lbl_str, break_lbl) = self.generate_loop_break_label();
            let (_, cont_lbl) = self.generate_loop_continue_label();

            self.parse_for_initializer(&stmt.initializer.node);
            self.push(start_lbl);
            if let Some(cond) = &stmt.condition {
                let cond = self.parse_expression(&cond.node);
                self.push(IROp::CondBranch(BranchType::Zero, break_lbl_str, cond));
            }
            self.parse_statement(&stmt.statement.node);
            self.push(cont_lbl);
            if let Some(step) = &stmt.step {
                self.parse_expression(&step.node);
            }
            self.push(IROp::AlwaysBranch(start_lbl_str));
            self.push(break_lbl);
            self.loop_id = prev_id;
            self.break_last_seen = prev_seen;
            self.scope = old_scope;
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
            let info = DeclarationInfo::from(&decls.specifiers);
            // FIXME: this is horrific.
            // TODO: use type info
            for decl in decls.declarators.clone() {
                let name = self.parse_declarator(&decl.node.declarator.node);
                let loc = if self.is_const {
                    match info.duration {
                        StorageDuration::Static => IRValue::StaticPsuedo {
                            name: name.clone(),
                            linkable: false,
                        },
                        StorageDuration::Default | StorageDuration::Extern => {
                            IRValue::StaticPsuedo {
                                name: name.clone(),
                                linkable: true,
                            }
                        }
                    }
                } else {
                    match info.duration {
                        // Grab from scope outside function (note init is not allowed here)
                        StorageDuration::Extern => {
                            if self.file_builder.scope.var_map.contains_key(&name) {
                                self.file_builder.scope.var_map.get(&name).unwrap().clone()
                            } else {
                                // NOTE: this relies on a neat trick: if you reference a variable
                                // via `extern` before it has been declared, then it cannot be
                                // `static`, it must be either (top level) `extern` or have no
                                // keyword. Both of those are linkable, so we can just generate a
                                // linkable pseudo here instead of having to figure it out later.
                                let j = IRValue::StaticPsuedo {
                                    name: name.clone(),
                                    linkable: true,
                                };
                                self.file_builder
                                    .scope
                                    .var_map
                                    .insert(name.clone(), j.clone());
                                j
                            }
                        }
                        // Initialize
                        StorageDuration::Static => self.generate_unique_static_pseudo(name.clone()),
                        StorageDuration::Default => self.generate_named_pseudo(name.clone()),
                    }
                };
                self.scope.var_map.insert(name, loc.clone());

                if !self.is_const && matches!(info.duration, StorageDuration::Static) {
                    let mut builder = TopLevelBuilder {
                        ops: vec![],
                        count: self.count,
                        scope: self.file_builder.scope.clone(),
                        loop_id: None,
                        break_last_seen: BreakTypes::None,
                        is_const: true,
                        switch_case_info: None,
                        file_builder: self.file_builder,
                    };
                    let init = if let Some(init) = decl.node.initializer {
                        builder.parse_initializer(&init.node)
                    } else {
                        IRValue::Immediate(0)
                    };

                    builder.push(IROp::One(UnaryOp::Copy, init, loc));
                    // FIXME: bad bad bad, just have a seperate global counter
                    self.count = builder.count;
                    let init = IRTopLevel {
                        name: String::new(),
                        ops: builder.ops,
                        parameters: 0,
                        is_initializer: true,
                        stack_frame_size: builder.count,
                    };
                    self.file_builder.funcs.push(init);
                } else {
                    let init = if let Some(init) = decl.node.initializer {
                        self.parse_initializer(&init.node)
                    } else if self.is_const {
                        IRValue::Immediate(0)
                    } else {
                        return;
                    };

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
                Expression::Identifier(ident) => match self.scope.var_map.get(&ident.node.name) {
                    None => panic!("ident {ident:?} should exist. scope: {:?}", self.scope),
                    Some(val) => val.clone(),
                },
                Expression::Call(call_expr) => self.parse_call(&call_expr.node),

                // Type stuff
                Expression::Cast(cast_expr) => self.parse_cast(&cast_expr.node),
                Expression::SizeOfTy(_) => todo!("SizeOfTy {expr:?}"),
                Expression::SizeOfVal(_) => todo!("SizeOfVal {expr:?}"),
                Expression::AlignOf(_) => todo!("AlignOf {expr:?}"),

                // Struct stuff
                Expression::Member(_) => todo!("Member {expr:?}"),
                Expression::OffsetOf(_) => todo!("OffsetOf {expr:?}"),

                // Other stuff
                Expression::StringLiteral(_) => todo!("StringLiteral {expr:?}"),
                Expression::GenericSelection(_) => todo!("GenericSelection {expr:?}"),
                Expression::CompoundLiteral(_) => todo!("CompoundLiteral {expr:?}"),
                Expression::Conditional(cond) => self.parse_ternary(&cond.node),
                Expression::Comma(_) => todo!("Comma {expr:?}"),
                Expression::VaArg(_) => todo!("VaArg {expr:?}"),

                // Extension
                Expression::Statement(_) => todo!("Statement {expr:?}"),
            }
        }

        fn parse_cast(&mut self, node: &CastExpression) -> IRValue {
            todo!("Cast {node:?}")
        }

        fn parse_ternary(&mut self, node: &ConditionalExpression) -> IRValue {
            let out = self.generate_pseudo();
            let (else_str, else_lbl) = self.generate_label("else");
            let (end_str, end_lbl) = self.generate_label("end");

            let cond = self.parse_expression(&node.condition.node);
            self.push(IROp::CondBranch(BranchType::Zero, else_str, cond));
            let temp = self.parse_expression(&node.then_expression.node);
            self.push(IROp::One(UnaryOp::Copy, temp, out.clone()));
            self.push(IROp::AlwaysBranch(end_str));
            self.push(else_lbl);
            let temp = self.parse_expression(&node.else_expression.node);
            self.push(IROp::One(UnaryOp::Copy, temp, out.clone()));
            self.push(end_lbl);
            out
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
            match expr.operator.node {
                UnaryOperator::Complement => {
                    self.push(IROp::One(UnaryOp::Complement, val, out.clone()));
                }
                UnaryOperator::Minus => self.push(IROp::One(UnaryOp::Minus, val, out.clone())),
                UnaryOperator::Negate => {
                    self.push(IROp::One(UnaryOp::BooleanNegate, val, out.clone()));
                }
                UnaryOperator::Plus => self.push(IROp::One(UnaryOp::Copy, val, out.clone())), // silly

                // ++x, increment and evaluate to x+1
                UnaryOperator::PreIncrement => {
                    self.push(IROp::Two(
                        BinOp::Add,
                        val.clone(),
                        IRValue::Immediate(1),
                        val.clone(),
                    ));
                    return val;
                }
                // --x, decrement and evaluate to x-1
                UnaryOperator::PreDecrement => {
                    self.push(IROp::Two(
                        BinOp::Sub,
                        val.clone(),
                        IRValue::Immediate(1),
                        val.clone(),
                    ));
                    return val;
                }

                // x++, increment and evaluate to x
                UnaryOperator::PostIncrement => {
                    self.push(IROp::One(UnaryOp::Copy, val.clone(), out.clone()));
                    self.push(IROp::Two(
                        BinOp::Add,
                        val.clone(),
                        IRValue::Immediate(1),
                        val,
                    ));
                }
                // x--
                UnaryOperator::PostDecrement => {
                    self.push(IROp::One(UnaryOp::Copy, val.clone(), out.clone()));
                    self.push(IROp::Two(
                        BinOp::Sub,
                        val.clone(),
                        IRValue::Immediate(1),
                        val,
                    ));
                }

                // Memory stuff
                UnaryOperator::Address => todo!("Address {expr:?}"),
                UnaryOperator::Indirection => todo!("Indirection {expr:?}"),
            };
            out
        }

        fn parse_binary_expression(&mut self, expr: &BinaryOperatorExpression) -> IRValue {
            let (skip_label_str, skip_label) = self.generate_label("logical_skip");
            let (end_label_str, end_label) = self.generate_label("logical_end");

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
                    IROp::Two(BinOp::ShiftLeft, lhs, rhs, out.clone())
                }
                BinaryOperator::ShiftRight | BinaryOperator::AssignShiftRight => {
                    IROp::Two(BinOp::ShiftRight, lhs, rhs, out.clone())
                }
                BinaryOperator::BitwiseAnd | BinaryOperator::AssignBitwiseAnd => {
                    IROp::Two(BinOp::BitwiseAnd, lhs, rhs, out.clone())
                }
                BinaryOperator::BitwiseXor | BinaryOperator::AssignBitwiseXor => {
                    IROp::Two(BinOp::BitwiseXor, lhs, rhs, out.clone())
                }
                BinaryOperator::BitwiseOr | BinaryOperator::AssignBitwiseOr => {
                    IROp::Two(BinOp::BitwiseOr, lhs, rhs, out.clone())
                }

                BinaryOperator::Assign => {
                    self.push(IROp::One(UnaryOp::Copy, rhs.clone(), lhs));
                    return rhs;
                }

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

        #[allow(clippy::from_str_radix_10)]
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
            IRValue::Psuedo {
                name: "tmp.".to_owned() + &self.count.to_string(),
            }
        }

        fn generate_named_pseudo(&mut self, name: String) -> IRValue {
            self.count += 1;
            IRValue::Psuedo {
                name: name + "." + &self.count.to_string(),
            }
        }

        fn generate_unique_static_pseudo(&mut self, name: String) -> IRValue {
            self.count += 1;
            IRValue::StaticPsuedo {
                name: name + "." + &self.count.to_string(),
                linkable: false,
            }
        }

        fn generate_label(&mut self, str: &str) -> (String, IROp) {
            self.count += 1;
            let str = str.to_owned() + "." + &self.count.to_string();
            (str.clone(), IROp::Label(str))
        }

        fn generate_loop_label(&mut self) -> (String, IROp) {
            let loop_id = self
                .loop_id
                .expect("Loop labels should only occur inside a loop");
            let str = "loop".to_owned() + &loop_id.to_string();
            (str.clone(), IROp::Label(str))
        }

        fn generate_loop_break_label(&mut self) -> (String, IROp) {
            let loop_id = self
                .loop_id
                .expect("Break labels should only occur inside a loop");
            let str = "loop_break".to_owned() + &loop_id.to_string();
            (str.clone(), IROp::Label(str))
        }

        fn generate_loop_continue_label(&mut self) -> (String, IROp) {
            let loop_id = self
                .loop_id
                .expect("Continue labels should only occur inside a loop");
            let str = "loop_continue".to_owned() + &loop_id.to_string();
            (str.clone(), IROp::Label(str))
        }

        fn new_loop_id(&mut self) {
            let x = self.count;
            self.loop_id = Some(x);
            self.count += 1;
        }

        fn new_switch_case_id(&mut self) {
            let x = self.count;
            self.switch_case_info = Some(SwitchCaseInfo::new(x));
            self.count += 1;
        }

        fn generate_switch_case_label(&mut self, id: usize, case: usize) -> (String, IROp) {
            let str = "switch.".to_owned() + &id.to_string() + ".case." + &case.to_string();
            (str.clone(), IROp::Label(str))
        }

        fn generate_switch_case_default_label(&mut self, id: usize) -> (String, IROp) {
            let str = "switch.".to_owned() + &id.to_string() + ".case.default";
            (str.clone(), IROp::Label(str))
        }

        fn generate_switch_case_end_label(&mut self, id: usize) -> (String, IROp) {
            let str = "switch.".to_owned() + &id.to_string() + ".end";
            (str.clone(), IROp::Label(str))
        }
    }

    fn cleanup_parsed_asm(lines: &[String]) -> Vec<String> {
        lines
            .iter()
            .map(|line| line.trim_matches(|x| x == '"').to_owned())
            .collect()
    }
}

fn print_ir(ir: &Vec<IRTopLevel>) {
    for func in ir {
        if func.is_initializer {
            println!("\nINIT REGION");
        } else {
            println!("\nFUNC {:?}", func.name);
        }
        println!("frame_size: {}", func.stack_frame_size);
        for line in &func.ops {
            println!("{line:?}");
        }
    }
}

fn main() {
    let args = Args::parse();
    let c_source = std::fs::read_to_string(&args.filename).expect("Unable to read input file");
    if args.verbose {
        println!("-- C SOURCE");
        println!("{c_source}\n");
    }
    let program = match FileBuilder::parse_c(&args.filename) {
        Err(err) => panic!("Error occurred during parsing: {err:?}"),
        Ok(x) => x,
    };
    if args.verbose {
        println!("\n-- IR, PRE LINKING");
        print_ir(&program);
    }

    let mut program = the_linkening(vec![
        program,
        /*vec![IRTopLevel {
            ops: vec![IROp::One(
                UnaryOp::Copy,
                IRValue::Immediate(5),
                IRValue::StaticPsuedo {
                    name: "j".to_string(),
                    linkable: true,
                },
            )],
            name: "".to_string(),
            stack_frame_size: 1,
            is_initializer: true,
            parameters: 0,
        }],*/
    ]);

    if args.verbose {
        println!("\n-- IR, PRE PASSES");
        print_ir(&program);
    }

    // Mandatory passes
    program = sort_functions_pass(program); // put main function at the top
    pseudo_removal_pass(&mut program);
    stack_size_reducer_pass(&mut program);
    let function_map = function_id_mapping_pass(&program);

    if args.verbose {
        println!("\n-- IR, POST PASSES");
        print_ir(&program);
        println!("function mappings: {function_map:?}");
    }

    if args.verbose {
        println!("\n-- befunge begin");
    }
    let mut cg = CodeGen {
        builder: OpBuilder::new(false),
        function_map,
    };

    let mut out: Vec<String> = vec![];
    out.extend(PRE_INIT_PRELUDE.lines().map(ToOwned::to_owned));
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
    out.extend(POST_INIT_PRELUDE.lines().map(ToOwned::to_owned));
    let func_finder_pos = (3, out.len() - 2);
    out.extend(funcs);

    // Sneaky stick filename at top
    out[0] += &(" file: ".to_owned() + &args.filename);

    // Stick preproccesor info at the bottom
    if args.preprocessor_info {
        out.extend(vec![
            "#$watch[0,0]:int = stack".to_owned(),
            "#$watch[1,0]:int = call stack".to_owned(),
            "#$watch[2,0]:int = return".to_owned(),
            format!("#$break[{},{}]", func_finder_pos.0, func_finder_pos.1),
        ]);
    }

    if !args.silent {
        for line in out.clone() {
            println!("{line}");
        }
    }

    if let Some(filename) = args.outfile {
        write_each(filename, out).unwrap();
    }
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

// FIXME: Fix first function having a stack size greater than x. Just increment by stack_frame_size
// at start :)
// FIXME: reorganise __asm__ so all [bstack]'s are loaded first
// TODO: use seperate global counter for global values
//
// NOTE: remember, linking can only use values without . in the name
//
// NOTE: Some custom printing thing is going to be needed for every value that isn't signed int(/long?)
// NOTE: When implementing unsigned ints, gonna need to have custom impls for some things
//        likely mult, divide and modulo (as befunge ops are signed).
