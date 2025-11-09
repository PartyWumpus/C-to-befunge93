use codespan_reporting::{
    diagnostic::{self, Diagnostic},
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use indexmap::IndexMap;
use parking_lot::Mutex;

use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    io::{self, Write},
    mem,
    process::{Command, Stdio},
    sync::Arc,
};

use lang_c::{
    ast::{
        ArrayDeclarator, ArraySize, AsmStatement, BinaryOperator, BinaryOperatorExpression,
        BlockItem, CallExpression, CastExpression, ConditionalExpression, Constant, Declaration,
        DeclarationSpecifier, Declarator, DeclaratorKind, DerivedDeclarator, DoWhileStatement,
        Ellipsis, Expression, ExternalDeclaration, ForInitializer, ForStatement,
        FunctionDefinition, GnuAsmOperand, Identifier, IfStatement, Initializer, Integer,
        IntegerBase, IntegerSize, IntegerSuffix, Label, LabeledStatement, MemberExpression,
        MemberOperator, SizeOfTy, SizeOfVal, SpecifierQualifier, Statement, StorageClassSpecifier,
        StructDeclaration, StructKind, SwitchStatement, TypeName, TypeSpecifier, UnaryOperator,
        UnaryOperatorExpression, WhileStatement,
    },
    driver::{Flavor, parse_preprocessed},
    span::{Node, Span},
};
use thiserror::Error;

use crate::{
    ARGS,
    ir::{BinOp, BranchType, IROp, IRTopLevel, IRType, IRTypeConversionError, IRValue, UnaryOp},
};

#[derive(Error, Debug)]
pub enum IRGenerationErrorType {
    #[error(transparent)]
    IRTypeConversionError(#[from] IRTypeConversionError),
    #[error(transparent)]
    InvalidCoercion(#[from] InvalidCoercionError),
    #[error("Unknown type: {0:?}")]
    InvalidType(Box<[TypeSpecifier]>),
    #[error("Unknown struct: {0:?}")]
    InvalidStructType(String),
    #[error("Cannot dereference non-pointers")]
    DereferenceNonPointer,
    #[error("Cannot index pointers with pointers")]
    IndexPointerWithPointer,
    #[error("Cannot index non-pointers")]
    IndexWithoutPointer,
    #[error("Cannot add two pointers")]
    PointerAddition,
    #[error("Cannot subtract pointer from integer")]
    PointerSubtraction,
    #[error("Unknown identifier")]
    UnknownIdentifier,
    #[error("Non-integer array length")]
    NonIntegerArrayLength,
    #[error("Arrays must be initialized with an initializer list or string literal")]
    InvalidArrayInit,
    #[error(
        "(TODO) Non trivial array length. VLAs are not supported and neither are constant expressions."
    )]
    TODONonConstantArrayLength,
    #[error("(TODO) Array length must be manually specified for now.")]
    TODOUnknownArrayLength,
    #[error("K&R function definitions are not supported.")]
    KRFunctionDefinition,
    #[error("Function expected {expected} args, but recieved {recieved}")]
    IncorrectNumberOfArguments { expected: usize, recieved: usize },
    #[error("Attempted to call non function")]
    CallNonFunction,
    #[error("Attempted to use function as a variable")]
    FunctionUsedAsVariable,
    #[error("Non static expression in static block")]
    NonStaticInStaticBlock,
    #[error("Invalid ASM symbol '{0}'")]
    InvalidASMSymbol(String),
    #[error("Excess elements in array initializer")]
    ExcessElementsInInitializer,
    #[error("Char literals must be one character (or an escape sequence)")]
    LongCharLiteral,

    #[error("INTERNAL: A non func declarator in the func declarator parser? type: {0:?}")]
    INTERNALNonFuncDeclaratorInFuncDeclaratorParser(DerivedDeclarator),

    // todos
    #[error("GNU block types are not supported")]
    TODOBlockTypes,
    #[error("Imaginary numbers are not supported")]
    TODOImaginary,
    #[error("Given type suffix is not yet implemented")]
    TODOTypeSuffix,
    #[error("Floats are not yet supported")]
    TODOFloats,
    #[error("Ellipsis are not yet supported.")]
    TODOEllipsis,
    #[error("Case ranges are not yet supported")]
    TODOCaseRange,
    #[error("Static asserts are not yet supported")]
    TODOStaticAssert,
    #[error("Designated initializers are not yet supported")]
    TODODesignatedInit,
    #[error("Abstract declarators are not yet supported")]
    TODOAbstractDeclarator,
    #[error("Non-trivial initializers are not yet supported")]
    TODOComplexInitializers,
    #[error("Scalar initializers are not yet supported")]
    TODOScalarInitializers,

    // switch case errors
    #[error("Breaks can only appear inside loops or switch case statements")]
    InvalidBreak,
    #[error("Continues can only appear inside loops")]
    InvalidContinue,
    #[error("Case statement not in switch statement")]
    CaseNotInSwitch,
    #[error("'default' not in switch statement")]
    DefaultNotInSwitch,

    // struct errors
    #[error("Type {0:?} is a struct, use '.' instead of '->'")]
    AttemptToAccessStructWithArrow(CType),
    #[error("Type {0:?} is a pointer to a struct, use '->' instead of '.'")]
    AttemptToAccessPointerToStructWithDot(CType),
    #[error("Attempt to access non struct type: {0:?}")]
    AttemptToAccessNonStruct(CType),
    #[error("'{0}' is not a member of struct {1:?}")]
    InvalidStructMember(String, CType),

    // TODO: store the span of the location of the first declaration!
    #[error("Type {0:?} does not match earlier defined {1:?}")]
    NonMatchingDeclarations(CType, CType),
    #[error("Type {0:?} is not compatible with {1:?}")]
    IncompatibleTypes(CType, CType),
}

#[derive(Error, Debug)]
pub struct IRGenerationError {
    #[source]
    pub err: IRGenerationErrorType,
    pub span: lang_c::span::Span,
}

impl Display for IRGenerationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at span {:?} ", self.err, self.span)
    }
}

pub enum CompilerError {
    IRGenerationError {
        err: IRGenerationErrorType,
        span: lang_c::span::Span,
        source: String,
        filename: String,
    },
    ParseError {
        err: lang_c::driver::Error,
        filename: String,
    },
}

impl CompilerError {
    pub fn print(&self) {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        let (filename, source) = match self {
            Self::IRGenerationError {
                filename, source, ..
            } => (filename, source),
            Self::ParseError {
                filename,
                err: lang_c::driver::Error::SyntaxError(err),
            } => (filename, &err.source),
            Self::ParseError {
                filename,
                err: lang_c::driver::Error::PreprocessorError(_),
            } => (filename, &String::new()),
        };

        let file = SimpleFile::new(filename, source);
        let diag = self.report();

        term::emit(&mut writer.lock(), &config, &file, &diag)
            .expect("if io fails i cant do anything anyways");
    }

    fn report(&self) -> Diagnostic<()> {
        match self {
            Self::ParseError {
                err: lang_c::driver::Error::SyntaxError(err),
                ..
            } => {
                // check if missing char is (probably) a missing semicolon
                let mut end_of_line = false;
                let mut i = 0;
                if err.expected.contains(";") {
                    loop {
                        i += 1;
                        if i == err.offset {
                            break;
                        }
                        let char = err.source.as_bytes()[err.offset - i];
                        if char == b'\n' {
                            end_of_line = true;
                            break;
                        } else if char != b' ' {
                            end_of_line = false;
                            break;
                        }
                    }
                }

                if end_of_line {
                    Diagnostic::error()
                        .with_message("Missing semicolon at end of line")
                        .with_label(
                            diagnostic::Label::primary((), err.offset - i..err.offset - i)
                                .with_message("insert `;` here"),
                        )
                        .with_label(
                            diagnostic::Label::secondary((), err.offset..err.offset)
                                .with_message("Unexpected token"),
                        )
                } else {
                    Diagnostic::error()
                        .with_message("Unexpected token")
                        .with_label(diagnostic::Label::primary((), err.offset..err.offset))
                        .with_note(format!("Expected one of {:?}", err.expected))
                }
            }
            Self::ParseError {
                err: lang_c::driver::Error::PreprocessorError(err),
                ..
            } => Diagnostic::error()
                .with_message("Preprocessor error")
                .with_note(err),
            Self::IRGenerationError { err, span, .. } => Diagnostic::error()
                .with_message(err)
                .with_label(diagnostic::Label::primary((), span.start..span.end)),
        }
    }
}

#[derive(Debug)]
enum ExpressionOutput {
    Plain((IRValue, CType)),
    Dereferenced((IRValue, CType)),
    SubObject {
        base: IRValue,
        subtype: CType,
        offset: usize,
    },
}

#[derive(Debug)]
enum AssignmentStatus {
    NoAssignment,
    AssigningToValue(IRValue),
    AssigningToPointer(IRValue),
    AssigningToSubObject(IRValue, usize),
}

#[derive(Debug, Clone, Default, Eq, PartialEq)]
pub struct StructData {
    pub fields: IndexMap<String, (CType, usize)>,
    pub size: usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CType {
    UnsignedChar,
    SignedChar,
    Char,
    SignedInt,
    SignedLong,
    UnsignedInt,
    UnsignedLong,
    Void,
    Pointer(Box<CType>),
    // A fancy pointer
    Array(Box<CType>, usize),
    // The immediate location of an array
    ImmediateArray(Box<CType>, usize),
    Function(Box<[CType]>, Box<CType>),
    Struct(TagID),
}

impl CType {
    pub const fn is_integer(&self) -> bool {
        matches!(
            self,
            Self::SignedInt | Self::SignedLong | Self::UnsignedInt | Self::UnsignedLong
        )
    }

    pub const fn is_pointer(&self) -> bool {
        matches!(self, Self::Pointer(..) | Self::Array(..))
    }

    pub fn zero_init(&self) -> Vec<(IRValue, usize)> {
        match self {
            Self::Pointer(..)
            | Self::Char
            | Self::SignedChar
            | Self::SignedInt
            | Self::SignedLong
            | Self::UnsignedChar
            | Self::UnsignedInt
            | Self::UnsignedLong => {
                vec![(IRValue::Immediate(0), 1)]
            }
            Self::Array(inner_type, length) | Self::ImmediateArray(inner_type, length) => {
                let mut out = vec![];
                for _ in 0..*length {
                    out.extend(inner_type.zero_init());
                }
                out
            }
            Self::Struct(..) => todo!("zero init of structs"),
            Self::Void => unreachable!("void in zero init"),
            Self::Function(..) => unreachable!("function type in zero init"),
        }
    }

    pub fn get_common(type1: &Self, type2: &Self) -> Result<Self, InvalidCoercionError> {
        // If the same, no conversions are needed
        if type1 == type2 {
            return Ok(type1.clone());
        }

        // TODO: array types!

        // If one is a pointer, they must be the same (already checked) or one must be an integer
        if matches!(type1, Self::Pointer(_)) {
            return if type2.is_integer() {
                Ok(type1.clone())
            } else {
                Err(InvalidCoercionError(type1.clone(), type2.clone()))
            };
        }

        if matches!(type2, Self::Pointer(_)) {
            return if type1.is_integer() {
                Ok(type2.clone())
            } else {
                Err(InvalidCoercionError(type1.clone(), type2.clone()))
            };
        }

        Ok(Self::SignedLong)
    }
}

#[derive(Error, Debug)]
pub struct InvalidCoercionError(CType, CType);
impl Display for InvalidCoercionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid coersion between {:?} and {:?}", self.0, self.1)
    }
}

struct TopLevelBuilder<'a> {
    count: usize,
    ops: Vec<IROp>,
    scope: ScopeInfo,
    break_last_seen: BreakTypes,
    loop_id: Option<usize>,
    return_type: CType,
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
    cases: Vec<Node<Expression>>,
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
    // TODO: make hashmap so can be searched through easier
    funcs: Vec<IRTopLevel>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TagType {
    Struct,
}

type TagID = usize;

#[derive(Debug, Clone)]
struct TagData {
    source_depth: usize,
    tag_id: TagID,
    tag_type: TagType,
}

// TODO: scope system will need a bit of a refactor to make unshadowing of globals possible
#[derive(Debug, Clone, Default)]
pub struct ScopeInfo {
    // IRValue is only None when CType is Function
    var_map: HashMap<String, (Option<IRValue>, CType)>,
    tag_map: HashMap<String, TagData>,
    structs: Arc<Mutex<Vec<Option<StructData>>>>,
    depth: usize,
}

impl ScopeInfo {
    fn new_scope(&self) -> Self {
        let mut scope = self.clone();
        scope.depth += 1;
        scope
    }

    fn generate_struct_id(&mut self, name: &str) -> (TagID, bool) {
        match self.tag_map.get(name) {
            // if it has not been declared yet then just insert
            None => (self.structs.lock().len(), true),
            Some(TagData {
                source_depth,
                tag_type,
                tag_id,
            }) => {
                if *source_depth == self.depth {
                    assert_eq!(*tag_type, TagType::Struct);
                    (*tag_id, false)
                } else {
                    // it has been declared earlier, but in a higher scope, so overwrite
                    (self.structs.lock().len(), true)
                }
            }
        }
    }

    fn insert_incomplete_struct(&mut self, name: String) -> TagID {
        let (id, new) = self.generate_struct_id(&name);
        if !new {
            return id;
        }
        let mut structs = self.structs.lock();
        self.tag_map.insert(
            name,
            TagData {
                source_depth: self.depth,
                tag_id: id,
                tag_type: TagType::Struct,
            },
        );
        structs.push(None);
        id
    }

    fn insert_struct(&mut self, name: String, struct_data: StructData) -> TagID {
        let (id, new) = self.generate_struct_id(&name);
        if new {
            let mut structs = self.structs.lock();
            self.tag_map.insert(
                name,
                TagData {
                    source_depth: self.depth,
                    tag_id: id,
                    tag_type: TagType::Struct,
                },
            );
            structs.push(Some(struct_data));
            id
        } else {
            assert!(self.structs.lock()[id].is_none(), "cannot redefine structs");
            self.tag_map.insert(
                name,
                TagData {
                    source_depth: self.depth,
                    tag_id: id,
                    tag_type: TagType::Struct,
                },
            );
            self.structs.lock()[id] = Some(struct_data);
            id
        }
    }

    pub fn get_struct_by_id(&self, id: TagID) -> StructData {
        let structs = self.structs.lock();
        // TODO: don't clone here
        structs[id].as_ref().unwrap().clone()
    }
}

#[derive(Clone, Debug)]
enum InitializerInfo {
    Single((IRValue, CType), Span),
    Compound(Vec<InitializerInfo>, Span),
}

fn preprocess(config: &lang_c::driver::Config, source: &[u8]) -> io::Result<String> {
    let mut cmd = Command::new(&config.cpp_command);
    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());

    for item in &config.cpp_options {
        cmd.arg(item);
    }

    let mut cmd = cmd.spawn().expect("Failed to spawn child process");

    let mut stdin = cmd.stdin.take().expect("Failed to open stdin");
    let source = source.to_vec(); // pointless clone :(
    std::thread::spawn(move || {
        stdin.write_all(&source).expect("Failed to write to stdin");
    });

    let output = cmd.wait_with_output().expect("Failed to read stdout");

    if output.status.success() {
        match String::from_utf8(output.stdout) {
            Ok(s) => Ok(s),
            Err(e) => Err(io::Error::other(e)),
        }
    } else {
        String::from_utf8(output.stderr).map_or_else(
            |_| Err(io::Error::other("cpp error: contains invalid utf-8")),
            |s| Err(io::Error::other(s)),
        )
    }
}

impl FileBuilder {
    pub fn parse_c(source: &[u8], filename: &str) -> Result<Vec<IRTopLevel>, Box<CompilerError>> {
        let mut builder = Self {
            count: 0,
            scope: ScopeInfo::default(),
            funcs: vec![],
        };
        let config = lang_c::driver::Config {
            flavor: Flavor::GnuC11,
            cpp_command: "gcc".into(),
            cpp_options: vec![
                // don't include normal libc
                "-nostdinc".into(),
                // include custom libc
                "-I".into(),
                "befunge_libc".into(),
                // only preprocess
                "-E".into(),
                // read c file from stdin
                "-xc".into(),
                "-".into(),
            ],
        };
        let preproccessed = preprocess(&config, source).unwrap();

        if ARGS.verbose {
            println!("-- C SOURCE (post preprocessor)");
            println!("{preproccessed}\n");
        }

        let parsed = parse_preprocessed(&config, preproccessed).map_err(|err| {
            CompilerError::ParseError {
                err: lang_c::driver::Error::SyntaxError(err),
                filename: filename.to_string(),
            }
        })?;

        for obj in &parsed.unit.0 {
            match &obj.node {
                ExternalDeclaration::Declaration(decl) => {
                    let x = builder.parse_top_level_declaration(decl);
                    builder
                        .funcs
                        .push(x.map_err(|err| CompilerError::IRGenerationError {
                            err: err.err,
                            span: err.span,
                            source: parsed.source.clone(),
                            filename: filename.into(),
                        })?);
                }
                ExternalDeclaration::StaticAssert(ass) => Err(CompilerError::IRGenerationError {
                    err: IRGenerationErrorType::TODOStaticAssert,
                    span: ass.span,
                    source: parsed.source.clone(),
                    filename: filename.into(),
                })?,
                ExternalDeclaration::FunctionDefinition(func) => {
                    let x = builder.parse_function(func).map_err(|err| {
                        CompilerError::IRGenerationError {
                            err: err.err,
                            span: err.span,
                            source: parsed.source.clone(),
                            filename: filename.into(),
                        }
                    })?;
                    builder.funcs.push(x);
                }
            }
        }
        Ok(builder.funcs)
    }

    fn parse_function(
        &mut self,
        func: &Node<FunctionDefinition>,
    ) -> Result<IRTopLevel, IRGenerationError> {
        let info = DeclarationInfo::from_decl(&func.node.specifiers, &mut self.scope, false)?;
        let mut builder = TopLevelBuilder {
            ops: vec![],
            count: self.count,
            scope: self.scope.new_scope(),
            loop_id: None,
            break_last_seen: BreakTypes::None,
            is_const: false,
            switch_case_info: None,
            file_builder: self,
            return_type: info.c_type,
        };

        let name = parse_declarator_name(&func.node.declarator)?;
        let param_count = builder.parse_func_declarator(&func.node.declarator)?.len();
        builder.parse_statement(&func.node.statement)?;
        builder.push(IROp::Return(IRValue::Immediate(0)));

        // FIXME: bad bad bad, just have a seperate global counter
        builder.file_builder.count = builder.count;
        Ok(IRTopLevel {
            name,
            ops: builder.ops,
            parameters: param_count,
            is_initializer: false,
            return_type: Some(builder.return_type),
            stack_frame_size: builder.count, // NOTE: this is a 'worst case' value,
                                             // and should be recalculated in a later pass
        })
    }

    fn parse_top_level_declaration(
        &mut self,
        decl: &Node<Declaration>,
    ) -> Result<IRTopLevel, IRGenerationError> {
        let mut builder = TopLevelBuilder {
            ops: vec![],
            count: self.count,
            scope: self.scope.clone(),
            loop_id: None,
            break_last_seen: BreakTypes::None,
            is_const: true,
            switch_case_info: None,
            file_builder: self,
            return_type: CType::Void,
        };

        builder.parse_declarations(decl)?;
        let name = &decl
            .node
            .declarators
            .iter()
            .map(|x| Ok(format!("'{}'", parse_declarator_name(&x.node.declarator)?)))
            .collect::<Result<Vec<String>, _>>()?
            .join(", ");
        let name = if name.is_empty() {
            "empty region"
        } else {
            name
        };

        builder.file_builder.scope = builder.scope;

        // FIXME: bad bad bad, just have a seperate global counter
        builder.file_builder.count = builder.count;
        Ok(IRTopLevel {
            name: name.to_owned(),
            ops: builder.ops,
            parameters: 0,
            is_initializer: true,
            return_type: Some(builder.return_type),
            stack_frame_size: builder.count, // NOTE: this is a 'worst case' value,
                                             // and should be recalculated in a later pass
        })
    }
}

fn parse_declarator_name(decl: &Node<Declarator>) -> Result<String, IRGenerationError> {
    match &decl.node.kind.node {
        DeclaratorKind::Identifier(node) => Ok(node.node.name.clone()),
        DeclaratorKind::Declarator(node) => parse_declarator_name(node),
        DeclaratorKind::Abstract => Err(IRGenerationError {
            err: IRGenerationErrorType::TODOAbstractDeclarator,
            span: decl.span,
        }),
    }
}

impl CType {
    fn from_specifiers(
        types: &[&Node<TypeSpecifier>],
        scope: &mut ScopeInfo,
        is_statement: bool,
    ) -> Result<Self, IRGenerationError> {
        let ctypes = types.iter().map(|x| x.node.clone()).collect::<Vec<_>>();
        Ok(match &ctypes[..] {
            [] => unreachable!("No type specifiers?"),
            [TypeSpecifier::Signed, TypeSpecifier::Char] => Self::SignedChar,
            [TypeSpecifier::Unsigned, TypeSpecifier::Char] => Self::UnsignedChar,
            [TypeSpecifier::Char] => Self::Char,

            [TypeSpecifier::Int | TypeSpecifier::Signed]
            | [TypeSpecifier::Signed, TypeSpecifier::Int] => Self::SignedInt,
            [TypeSpecifier::Long]
            | [TypeSpecifier::Long, TypeSpecifier::Int]
            | [
                TypeSpecifier::Int | TypeSpecifier::Signed,
                TypeSpecifier::Long,
            ]
            | [
                TypeSpecifier::Long,
                TypeSpecifier::Int,
                TypeSpecifier::Signed,
            ]
            | [
                TypeSpecifier::Long,
                TypeSpecifier::Signed,
                TypeSpecifier::Int,
            ]
            | [
                TypeSpecifier::Signed,
                TypeSpecifier::Long,
                TypeSpecifier::Int,
            ] => Self::SignedLong,
            [TypeSpecifier::Unsigned] | [TypeSpecifier::Unsigned, TypeSpecifier::Int] => {
                Self::UnsignedInt
            }
            [TypeSpecifier::Unsigned, TypeSpecifier::Long]
            | [
                TypeSpecifier::Long,
                TypeSpecifier::Int,
                TypeSpecifier::Unsigned,
            ]
            | [
                TypeSpecifier::Unsigned,
                TypeSpecifier::Long,
                TypeSpecifier::Int,
            ] => Self::UnsignedLong,

            [TypeSpecifier::Void] => Self::Void,
            [TypeSpecifier::Struct(struct_data)] => {
                assert_eq!(struct_data.node.kind.node, StructKind::Struct);

                let name = struct_data
                    .node
                    .identifier
                    .clone()
                    .expect("no anon structs for now")
                    .node
                    .name;

                Self::Struct(match &struct_data.node.declarations {
                    // means it's just a definition not a declaration
                    None => match scope.tag_map.get(&name) {
                        // if it has not been declared yet then just insert
                        None => {
                            if is_statement {
                                scope.insert_incomplete_struct(name)
                            } else {
                                return Err(IRGenerationError {
                                    err: IRGenerationErrorType::InvalidStructType(name.clone()),
                                    span: struct_data.span,
                                });
                            }
                        }
                        Some(TagData {
                            source_depth,
                            tag_type,
                            tag_id,
                        }) => {
                            // it has been declared earlier, but in a higher scope, so overwrite
                            if is_statement && *source_depth != scope.depth {
                                scope.insert_incomplete_struct(name)
                            } else {
                                assert_eq!(*tag_type, TagType::Struct);
                                *tag_id
                            }
                        }
                    },
                    // it's a definition
                    Some(decls) => {
                        assert_ne!(decls.len(), 0);
                        let mut struct_data = StructData::default();
                        for decl in decls {
                            match &decl.node {
                                StructDeclaration::StaticAssert(..) => {
                                    return Err(IRGenerationError {
                                        err: IRGenerationErrorType::TODOAbstractDeclarator,
                                        span: decl.span,
                                    });
                                }
                                StructDeclaration::Field(field) => {
                                    let ctype =
                                        Self::from_qualifiers(&field.node.specifiers, scope)?;
                                    assert_eq!(field.node.declarators.len(), 1);
                                    assert!(field.node.declarators[0].node.bit_width.is_none());
                                    let declarator =
                                        &field.node.declarators[0].node.declarator.clone().unwrap();
                                    let mut ctype =
                                        Self::from_declarator(declarator, &ctype, scope)?;
                                    if let Self::Array(inner, length) = ctype {
                                        ctype = Self::ImmediateArray(inner, length);
                                    }
                                    let sizeof = ctype.sizeof(scope);
                                    struct_data.fields.insert(
                                        parse_declarator_name(declarator)?,
                                        (ctype, struct_data.size),
                                    );
                                    struct_data.size += sizeof;
                                }
                            }
                        }
                        scope.insert_struct(name, struct_data)
                    }
                })
            }
            _ => {
                if types.is_empty() {
                    unreachable!()
                } else {
                    let (start, end) = (types.first().unwrap().span, types.last().unwrap().span);
                    let span = lang_c::span::Span {
                        start: start.start,
                        end: end.end,
                    };
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::InvalidType(ctypes.into()),
                        span,
                    });
                }
            }
        })
    }

    fn from_qualifiers(
        specifiers: &[Node<SpecifierQualifier>],
        scope: &mut ScopeInfo,
    ) -> Result<Self, IRGenerationError> {
        let mut c_types = vec![];
        for specifier in specifiers {
            match &specifier.node {
                SpecifierQualifier::TypeSpecifier(spec) => c_types.push(spec),
                // Consider implementing just volatile
                SpecifierQualifier::TypeQualifier(_) => {
                    println!("WARNING: All type qualifiers are ignored {specifier:?}");
                }
                SpecifierQualifier::Extension(_) => println!(
                    "WARNING: GNU declaration specifier extensions are ignored {specifier:?}"
                ),
            }
        }

        Self::from_specifiers(&c_types, scope, false)
    }

    fn from_declarator(
        declarator: &Node<Declarator>,
        base_type: &Self,
        scope: &mut ScopeInfo,
    ) -> Result<Self, IRGenerationError> {
        let mut out = base_type.clone();
        let mut param_list = None;

        match &declarator.node.kind.node {
            DeclaratorKind::Identifier(_) | DeclaratorKind::Abstract => (),
            DeclaratorKind::Declarator(nested_decl) => {
                out = Self::from_declarator(nested_decl, &out, scope)?;
            }
        }

        for modifier in declarator.node.derived.iter().rev() {
            match &modifier.node {
                DerivedDeclarator::Pointer(qualifiers) => {
                    assert_eq!(qualifiers.len(), 0, "Pointer qualifiers not yet supported");
                    out = Self::Pointer(Box::new(out));
                }
                DerivedDeclarator::Array(array_decl) => {
                    let size = parse_array_length(array_decl)?;
                    out = Self::Array(Box::new(out), size);
                }
                DerivedDeclarator::Function(func_decl) => {
                    if matches!(func_decl.node.ellipsis, Ellipsis::Some) {
                        return Err(IRGenerationError {
                            err: IRGenerationErrorType::TODOEllipsis,
                            span: func_decl.span,
                        });
                    }
                    assert!(param_list.is_none());
                    param_list = Some(vec![]);
                    for param in &func_decl.node.parameters {
                        // FIXME:
                        let info =
                            DeclarationInfo::from_decl(&param.node.specifiers, scope, false)?;
                        let ctype = if let Some(decl) = &param.node.declarator {
                            Self::from_declarator(decl, &info.c_type, scope)?
                        } else {
                            info.c_type
                        };
                        if let Some(ref mut list) = param_list {
                            list.push(ctype);
                        }
                    }
                }
                DerivedDeclarator::KRFunction(_) => {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::KRFunctionDefinition,
                        span: modifier.span,
                    });
                }
                DerivedDeclarator::Block(_) => {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::TODOBlockTypes,
                        span: modifier.span,
                    });
                }
            }
        }

        if let Some(params) = param_list {
            let params = if matches!(params, [Self::Void]) {
                vec![]
            } else {
                params
            };
            Ok(Self::Function(params.into(), Box::new(out)))
        } else {
            Ok(out)
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

#[derive(Debug)]
struct FunctionDeclarationInfo {
    return_type: CType,
    parameters: Box<[CType]>,
}

impl DeclarationInfo {
    fn from_decl(
        specifiers: &[Node<DeclarationSpecifier>],
        scope: &mut ScopeInfo,
        is_statement: bool,
    ) -> Result<Self, IRGenerationError> {
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
                    _ => {
                        println!(
                            "WARNING: Some storage class specifiers are ignored {specifier:?}"
                        );
                    }
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
            }
        }

        Ok(Self {
            duration,
            c_type: CType::from_specifiers(&c_types, scope, is_statement)?,
        })
    }
}

#[expect(clippy::needless_pass_by_ref_mut, clippy::unused_self)]
impl TopLevelBuilder<'_> {
    fn parse_func_declarator(
        &mut self,
        decl: &Node<Declarator>,
    ) -> Result<Vec<(String, CType)>, IRGenerationError> {
        // FIXME: this name is found twice (also in parse_function). that's dumb.
        let name = parse_declarator_name(decl)?;

        let mut params = vec![];
        for node in &decl.node.derived {
            match &node.node {
                DerivedDeclarator::Function(func_decl) => {
                    if matches!(func_decl.node.ellipsis, Ellipsis::Some) {
                        return Err(IRGenerationError {
                            err: IRGenerationErrorType::TODOEllipsis,
                            span: node.span,
                        });
                    }
                    for param in &func_decl.node.parameters {
                        let info = DeclarationInfo::from_decl(
                            &param.node.specifiers,
                            &mut self.scope,
                            false,
                        )?;

                        if let Some(decl) = param.node.declarator.clone() {
                            let name = parse_declarator_name(&decl)?;
                            let ctype =
                                CType::from_declarator(&decl, &info.c_type, &mut self.scope)?;
                            params.push((name, ctype));
                        }
                    }
                }
                DerivedDeclarator::KRFunction(_) => {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::KRFunctionDefinition,
                        span: node.span,
                    });
                }
                DerivedDeclarator::Pointer(qualifiers) => {
                    assert_eq!(qualifiers.len(), 0, "Pointer qualifiers not yet supported");
                    self.return_type = CType::Pointer(Box::new(self.return_type.clone()));
                }
                DerivedDeclarator::Array(array_decl) => {
                    let size = parse_array_length(array_decl)?;
                    self.return_type = CType::Array(Box::new(self.return_type.clone()), size);
                }
                DerivedDeclarator::Block(..) => {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::INTERNALNonFuncDeclaratorInFuncDeclaratorParser(
                            node.node.clone(),
                        ),
                        span: node.span,
                    });
                }
            }
        }

        let params = match &decl.node.kind.node {
            DeclaratorKind::Identifier(_) | DeclaratorKind::Abstract => params,
            DeclaratorKind::Declarator(nested_decl) => {
                assert!(params.is_empty());
                self.parse_func_declarator(nested_decl)?
            }
        };

        self.scope.var_map.insert(
            name.clone(),
            (
                None,
                CType::Function(
                    params.iter().cloned().map(|(_, ctype)| ctype).collect(),
                    Box::new(self.return_type.clone()),
                ),
            ),
        );
        self.file_builder.scope.var_map.insert(
            name,
            (
                None,
                CType::Function(
                    params.iter().cloned().map(|(_, ctype)| ctype).collect(),
                    Box::new(self.return_type.clone()),
                ),
            ),
        );

        let mut i = 1;
        for (name, ctype) in params.iter().cloned() {
            let size = ctype.sizeof(&self.scope);
            self.scope
                .var_map
                .insert(name, (Some(IRValue::Stack(i)), ctype));
            i += size;
        }
        Ok(params)
    }

    fn push(&mut self, op: IROp) {
        self.ops.push(op);
    }

    fn parse_statement(&mut self, stmt: &Node<Statement>) -> Result<(), IRGenerationError> {
        match &stmt.node {
            Statement::Return(maybe_expr) => {
                if let Some(expr) = maybe_expr {
                    let (value, ctype) = self.parse_expression(expr)?;
                    let final_type = self.return_type.clone();
                    let out = self
                        .convert_to((value, ctype), &final_type)
                        .map_err(|err| IRGenerationError {
                            err,
                            span: stmt.span,
                        })?;
                    self.push(IROp::Return(out));
                } else {
                    // return type check not needed, as this is only reachable via UB
                    self.push(IROp::Return(IRValue::Immediate(0)));
                }
            }
            Statement::Compound(blocks) => {
                let old_scope = self.scope.new_scope();
                for block_item in blocks {
                    self.parse_block_item(block_item)?;
                }
                self.scope = old_scope;
            }
            Statement::Expression(Some(expr)) => {
                self.parse_expression(expr)?;
            }
            Statement::Expression(None) => (),
            Statement::If(stmt) => self.parse_if(stmt)?,
            Statement::While(stmt) => self.parse_while_stmt(stmt)?,
            Statement::DoWhile(stmt) => self.parse_do_while_stmt(stmt)?,
            Statement::For(stmt) => self.parse_for_statement(stmt)?,
            Statement::Break => self.parse_break().map_err(|err| IRGenerationError {
                err,
                span: stmt.span,
            })?,
            Statement::Continue => self.parse_continue().map_err(|err| IRGenerationError {
                err,
                span: stmt.span,
            })?,
            Statement::Labeled(stmt) => self.parse_label_stmt(stmt)?,
            Statement::Goto(stmt) => self.parse_goto_stmt(stmt),
            Statement::Switch(stmt) => self.parse_switch(stmt)?,
            Statement::Asm(stmt) => self.parse_asm(stmt)?,
        }
        Ok(())
    }

    fn parse_switch(&mut self, switch: &Node<SwitchStatement>) -> Result<(), IRGenerationError> {
        let condition = self.parse_expression(&switch.node.expression)?;

        let prev_seen = self.break_last_seen;
        self.break_last_seen = BreakTypes::SwitchCase;

        let old_scope = self.scope.new_scope();
        let mut old_switch_case_info = self.switch_case_info.clone();

        self.new_switch_case_id();

        // NOTE: This is a sort of dirty hack, not super happy with it.
        // We do all the parsing of the switch statement in a new array of ops,
        // which we'll stick onto the end after the branching logic
        let mut ops = vec![];
        mem::swap(&mut self.ops, &mut ops);
        self.parse_statement(&switch.node.statement)?;
        mem::swap(&mut self.ops, &mut ops);

        mem::swap(&mut self.switch_case_info, &mut old_switch_case_info);

        let info = old_switch_case_info.unwrap();
        let id = info.id;

        let tmp = self.generate_pseudo(CType::UnsignedInt.sizeof(&self.scope));
        for (i, expr) in info.cases.iter().enumerate() {
            let case_value = self.parse_expression(expr)?;
            self.push(IROp::Two(
                BinOp::Equal,
                case_value.0,
                condition.0.clone(),
                tmp.clone(),
                IRType::from_ctype(&CType::UnsignedInt, &self.scope),
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
        Ok(())
    }

    fn parse_goto_stmt(&mut self, ident: &Node<Identifier>) {
        self.push(IROp::AlwaysBranch(ident.node.name.clone() + ".goto"));
    }

    fn parse_label_stmt(&mut self, stmt: &Node<LabeledStatement>) -> Result<(), IRGenerationError> {
        match &stmt.node.label.node {
            Label::Identifier(lbl) => self.push(IROp::Label(lbl.node.name.clone() + ".goto")),
            Label::Case(expr) => match &mut self.switch_case_info {
                None => {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::CaseNotInSwitch,
                        span: expr.span,
                    });
                }
                Some(info) => {
                    let id = info.id;
                    let count = info.cases.len();
                    info.cases.push(*expr.clone());
                    let lbl = self.generate_switch_case_label(id, count).1;
                    self.push(lbl);
                }
            },
            Label::Default => {
                if self.switch_case_info.is_none() {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::DefaultNotInSwitch,
                        span: stmt.node.label.span,
                    });
                }
                let id = self.switch_case_info.as_ref().unwrap().id;
                let lbl = self.generate_switch_case_default_label(id).1;
                self.push(lbl);
                self.switch_case_info.as_mut().unwrap().has_default = true;
            }
            Label::CaseRange(expr) => {
                return Err(IRGenerationError {
                    err: IRGenerationErrorType::TODOCaseRange,
                    span: expr.span,
                });
            }
        }
        self.parse_statement(&stmt.node.statement)?;
        Ok(())
    }

    fn parse_asm(&mut self, asm: &Node<AsmStatement>) -> Result<(), IRGenerationError> {
        // TODO: ban newline char? it won't really work right
        match &asm.node {
            AsmStatement::GnuBasic(asm) => {
                let lines = cleanup_parsed_asm(&asm.node);
                self.push(IROp::InlineBefunge(lines));
            }
            AsmStatement::GnuExtended(asm) => {
                for input in &asm.inputs {
                    self.parse_asm_operand(input, true)?;
                }
                // note it's supposed to be a "template", but it is
                // difficult to do sensible befunge templating that still supports
                // multiple lines, so we won't, instead the inputs/outputs
                // will define what values are loaded where
                let lines = cleanup_parsed_asm(&asm.template.node);
                self.push(IROp::InlineBefunge(lines));
                for output in &asm.outputs {
                    self.parse_asm_operand(output, false)?;
                }

                for clobbers in &asm.clobbers {
                    println!("WARNING: asm clobbers are ignored {clobbers:?}");
                }
            }
        }
        Ok(())
    }

    fn parse_asm_operand(
        &mut self,
        op: &Node<GnuAsmOperand>,
        input: bool,
    ) -> Result<(), IRGenerationError> {
        if let Some(output_name) = &op.node.symbolic_name {
            let (c_value, ctype) = self.parse_expression(&op.node.variable_name)?;
            let asm_value = Self::parse_asm_symbolic(&output_name.node.name)
                .map_err(|err| IRGenerationError { err, span: op.span })?;
            if input {
                self.push(IROp::Copy(c_value, asm_value, ctype.sizeof(&self.scope)));
            } else {
                self.push(IROp::Copy(asm_value, c_value, ctype.sizeof(&self.scope)));
            }
        }
        Ok(())
    }

    fn parse_asm_symbolic(str: &str) -> Result<IRValue, IRGenerationErrorType> {
        if let Some(rest) = str.strip_prefix('r') {
            Ok(IRValue::Register(rest.parse().unwrap()))
        } else if str == "bstack" {
            Ok(IRValue::BefungeStack)
        } else {
            Err(IRGenerationErrorType::InvalidASMSymbol(str.to_string()))
        }
    }

    fn parse_break(&mut self) -> Result<(), IRGenerationErrorType> {
        match self.break_last_seen {
            BreakTypes::None => Err(IRGenerationErrorType::InvalidBreak)?,
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
        Ok(())
    }

    fn parse_continue(&mut self) -> Result<(), IRGenerationErrorType> {
        if self.loop_id.is_none() {
            Err(IRGenerationErrorType::InvalidContinue)?;
        }
        let (loop_cont_lbl_str, _) = self.generate_loop_continue_label();
        self.push(IROp::AlwaysBranch(loop_cont_lbl_str));
        Ok(())
    }

    fn parse_while_stmt(&mut self, stmt: &Node<WhileStatement>) -> Result<(), IRGenerationError> {
        let prev_scope = self.scope.new_scope();
        let prev_seen = self.break_last_seen;
        self.break_last_seen = BreakTypes::Loop;
        let prev_id = self.loop_id;
        self.new_loop_id();
        let (loop_lbl_str, loop_lbl) = self.generate_loop_continue_label();
        let (loop_end_lbl_str, loop_end_lbl) = self.generate_loop_break_label();

        self.push(loop_lbl);
        let (cond, _cond_type) = self.parse_expression(&stmt.node.expression)?;
        self.push(IROp::CondBranch(BranchType::Zero, loop_end_lbl_str, cond));
        self.parse_statement(&stmt.node.statement)?;
        self.push(IROp::AlwaysBranch(loop_lbl_str));
        self.push(loop_end_lbl);
        self.loop_id = prev_id;
        self.break_last_seen = prev_seen;
        self.scope = prev_scope;
        Ok(())
    }

    fn parse_do_while_stmt(
        &mut self,
        stmt: &Node<DoWhileStatement>,
    ) -> Result<(), IRGenerationError> {
        let prev_scope = self.scope.new_scope();
        let prev_seen = self.break_last_seen;
        self.break_last_seen = BreakTypes::Loop;
        let prev_id = self.loop_id;
        self.new_loop_id();
        let (loop_lbl_str, loop_lbl) = self.generate_loop_continue_label();
        let (_, loop_end_lbl) = self.generate_loop_break_label();

        self.push(loop_lbl);
        self.parse_statement(&stmt.node.statement)?;
        let (cond, _cond_type) = self.parse_expression(&stmt.node.expression)?;
        self.push(IROp::CondBranch(BranchType::NonZero, loop_lbl_str, cond));
        self.push(loop_end_lbl);
        self.loop_id = prev_id;
        self.break_last_seen = prev_seen;
        self.scope = prev_scope;
        Ok(())
    }

    fn parse_for_statement(&mut self, stmt: &Node<ForStatement>) -> Result<(), IRGenerationError> {
        let old_scope = self.scope.new_scope();
        let prev_seen = self.break_last_seen;
        self.break_last_seen = BreakTypes::Loop;
        let prev_id = self.loop_id;
        self.new_loop_id();
        let (start_lbl_str, start_lbl) = self.generate_loop_label();
        let (break_lbl_str, break_lbl) = self.generate_loop_break_label();
        let (_, cont_lbl) = self.generate_loop_continue_label();

        self.parse_for_initializer(&stmt.node.initializer)?;
        self.push(start_lbl);
        if let Some(cond) = &stmt.node.condition {
            let (cond, _cond_type) = self.parse_expression(cond)?;
            self.push(IROp::CondBranch(BranchType::Zero, break_lbl_str, cond));
        }
        self.parse_statement(&stmt.node.statement)?;
        self.push(cont_lbl);
        if let Some(step) = &stmt.node.step {
            self.parse_expression(step)?;
        }
        self.push(IROp::AlwaysBranch(start_lbl_str));
        self.push(break_lbl);
        self.loop_id = prev_id;
        self.break_last_seen = prev_seen;
        self.scope = old_scope;
        Ok(())
    }

    fn parse_for_initializer(
        &mut self,
        init: &Node<ForInitializer>,
    ) -> Result<(), IRGenerationError> {
        match &init.node {
            ForInitializer::Empty => (),
            ForInitializer::Expression(expr) => {
                self.parse_expression(expr)?;
            }
            ForInitializer::Declaration(decls) => {
                self.parse_declarations(decls)?;
            }
            ForInitializer::StaticAssert(expr) => Err(IRGenerationError {
                err: IRGenerationErrorType::TODOStaticAssert,
                span: expr.span,
            })?,
        }
        Ok(())
    }

    fn parse_if(&mut self, if_stmt: &Node<IfStatement>) -> Result<(), IRGenerationError> {
        let (cond, _cond_type) = self.parse_expression(&if_stmt.node.condition)?;
        let (else_str, else_label) = self.generate_label("else");
        let (end_str, end_label) = self.generate_label("else");
        self.push(IROp::CondBranch(BranchType::Zero, else_str, cond));
        self.parse_statement(&if_stmt.node.then_statement)?;
        if if_stmt.node.else_statement.is_some() {
            self.push(IROp::AlwaysBranch(end_str));
        }
        self.push(else_label);
        if let Some(else_stmt) = &if_stmt.node.else_statement {
            self.parse_statement(else_stmt)?;
            self.push(end_label);
        }
        Ok(())
    }

    fn parse_block_item(&mut self, block: &Node<BlockItem>) -> Result<(), IRGenerationError> {
        match &block.node {
            BlockItem::Statement(stmt) => self.parse_statement(stmt),
            BlockItem::Declaration(decls) => self.parse_declarations(decls),
            BlockItem::StaticAssert(ass) => Err(IRGenerationError {
                err: IRGenerationErrorType::TODOStaticAssert,
                span: ass.span,
            }),
        }
    }

    fn flatten_and_type_check_initializer_info(
        &mut self,
        init_info: InitializerInfo,
        target_type: &CType,
    ) -> Result<Vec<(IRValue, usize)>, IRGenerationError> {
        Ok(match (target_type, init_info) {
            (CType::Struct(..), InitializerInfo::Single((_rhs, rhs_type), span)) => {
                if *target_type != rhs_type {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::IncompatibleTypes(
                            target_type.clone(),
                            rhs_type,
                        ),
                        span,
                    });
                }

                return Err(IRGenerationError {
                    err: IRGenerationErrorType::TODOComplexInitializers,
                    span,
                });
            }
            (
                CType::Array(_inner_type, _size) | CType::ImmediateArray(_inner_type, _size),
                InitializerInfo::Single((_rhs, _rhs_type), span),
            ) => {
                return Err(IRGenerationError {
                    err: IRGenerationErrorType::InvalidArrayInit,
                    span,
                });
            }

            (_, InitializerInfo::Single((rhs, rhs_type), span)) => {
                vec![(
                    self.convert_to((rhs, rhs_type), target_type)
                        .map_err(|err| IRGenerationError { err, span })?,
                    target_type.sizeof(&self.scope),
                )]
            }

            (
                CType::Array(inner_type, size) | CType::ImmediateArray(inner_type, size),
                InitializerInfo::Compound(init_list, span),
            ) => {
                if init_list.len() > *size {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::ExcessElementsInInitializer,
                        span: Span {
                            start: match init_list[*size] {
                                InitializerInfo::Compound(_, span)
                                | InitializerInfo::Single(_, span) => span.start,
                            },
                            end: span.end,
                        },
                    });
                }
                let mut out = vec![];
                for init_info in init_list {
                    let x = self.flatten_and_type_check_initializer_info(init_info, inner_type)?;
                    out.extend(x);
                }
                while out.len() < size * inner_type.sizeof(&self.scope) {
                    out.extend(inner_type.zero_init());
                }
                out
            }
            (CType::Struct(tag_id), InitializerInfo::Compound(init_list, span)) => {
                let struct_data = self.scope.get_struct_by_id(*tag_id);
                if init_list.len() > struct_data.size {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::ExcessElementsInInitializer,
                        span: Span {
                            start: match init_list[struct_data.size] {
                                InitializerInfo::Compound(_, span)
                                | InitializerInfo::Single(_, span) => span.start,
                            },
                            end: span.end,
                        },
                    });
                }
                let mut out = vec![];
                let mut struct_fields = struct_data.fields.iter();
                for (init_info, (_field_name, (ctype, offset))) in
                    init_list.iter().zip(&mut struct_fields)
                {
                    let x =
                        self.flatten_and_type_check_initializer_info(init_info.clone(), ctype)?;
                    out.extend(x);
                }
                for (_field_name, (ctype, offset)) in struct_fields {
                    out.extend(ctype.zero_init());
                }
                out
            }
            (_, InitializerInfo::Compound(_init_list, span)) => {
                return Err(IRGenerationError {
                    err: IRGenerationErrorType::TODOScalarInitializers,
                    span,
                });
            }
        })
    }

    fn parse_declarations(&mut self, decls: &Node<Declaration>) -> Result<(), IRGenerationError> {
        let info = DeclarationInfo::from_decl(
            &decls.node.specifiers,
            &mut self.scope,
            decls.node.declarators.is_empty(),
        )?;
        // FIXME: this is horrific.
        // TODO: use type info
        for decl in &decls.node.declarators {
            let name = parse_declarator_name(&decl.node.declarator)?;
            let mut ctype =
                CType::from_declarator(&decl.node.declarator, &info.c_type, &mut self.scope)?;

            if let CType::Function(..) = ctype {
                if let Some((_, prev_ctype @ CType::Function(..))) = self.scope.var_map.get(&name) {
                    if *prev_ctype != ctype {
                        return Err(IRGenerationError {
                            err: IRGenerationErrorType::NonMatchingDeclarations(
                                ctype,
                                prev_ctype.clone(),
                            ),
                            span: decl.span,
                        });
                    }
                } else {
                    self.scope.var_map.insert(name, (None, ctype));
                }
                continue;
            }

            if let CType::Array(inner, length) = ctype {
                ctype = CType::ImmediateArray(inner, length);
            }

            let loc = if self.is_const {
                match info.duration {
                    StorageDuration::Static => IRValue::StaticPsuedo {
                        name: name.clone(),
                        linkable: false,
                        size: ctype.sizeof(&self.scope),
                    },
                    StorageDuration::Default | StorageDuration::Extern => IRValue::StaticPsuedo {
                        name: name.clone(),
                        linkable: true,
                        size: ctype.sizeof(&self.scope),
                    },
                }
            } else {
                match info.duration {
                    // Grab from scope outside function (note init is not allowed here)
                    StorageDuration::Extern => {
                        if self.file_builder.scope.var_map.contains_key(&name) {
                            let var = self.file_builder.scope.var_map.get(&name).unwrap();
                            match var {
                                (None, CType::Function(..)) => Err(IRGenerationError {
                                    err: IRGenerationErrorType::FunctionUsedAsVariable,
                                    span: decl.node.declarator.span,
                                })?,
                                (None, _) => unreachable!(),
                                (Some(loc), stored_ctype) => {
                                    if *stored_ctype != ctype {
                                        return Err(IRGenerationError {
                                            err: IRGenerationErrorType::NonMatchingDeclarations(
                                                ctype,
                                                stored_ctype.clone(),
                                            ),
                                            span: decl.node.declarator.span,
                                        });
                                    }

                                    loc.clone()
                                }
                            }
                        } else {
                            // NOTE: this relies on a c quirk: if you reference a variable
                            // via `extern` before it has been declared, then it cannot be
                            // `static`, it must be either (top level) `extern` or have no
                            // keyword. Both of those are linkable, so we can just generate a
                            // linkable pseudo here instead of having to figure it out later.
                            let j = IRValue::StaticPsuedo {
                                name: name.clone(),
                                linkable: true,
                                size: ctype.sizeof(&self.scope),
                            };
                            self.file_builder
                                .scope
                                .var_map
                                .insert(name.clone(), (Some(j.clone()), ctype.clone()));
                            j
                        }
                    }
                    // Initialize
                    StorageDuration::Static => {
                        self.generate_unique_static_pseudo(name.clone(), ctype.sizeof(&self.scope))
                    }
                    StorageDuration::Default => {
                        self.generate_named_pseudo(name.clone(), ctype.sizeof(&self.scope))
                    }
                }
            };

            self.scope
                .var_map
                .insert(name, (Some(loc.clone()), ctype.clone()));

            if !self.is_const && matches!(info.duration, StorageDuration::Static) {
                let mut builder = TopLevelBuilder {
                    ops: vec![],
                    count: self.count,
                    scope: self.file_builder.scope.new_scope(),
                    loop_id: None,
                    break_last_seen: BreakTypes::None,
                    is_const: true,
                    switch_case_info: None,
                    file_builder: self.file_builder,
                    return_type: ctype.clone(),
                };
                let inits = if let Some(init) = &decl.node.initializer {
                    let init_info = builder.parse_initializer(init)?;
                    builder.flatten_and_type_check_initializer_info(init_info, &ctype)?
                } else {
                    vec![(IRValue::Immediate(0), 1)]
                };

                for (i, init) in inits.into_iter().enumerate() {
                    assert!(init.1 == 1);
                    builder.push(IROp::CopyToOffset(init.0, loc.clone(), i));
                }

                // FIXME: bad bad bad, just have a seperate global counter
                self.count = builder.count;
                let init = IRTopLevel {
                    name: String::new(),
                    ops: builder.ops,
                    parameters: 0,
                    is_initializer: true,
                    stack_frame_size: builder.count,
                    return_type: Some(builder.return_type),
                };
                self.file_builder.funcs.push(init);
            } else {
                let inits = if let Some(init) = &decl.node.initializer {
                    let init_info = self.parse_initializer(init)?;
                    self.flatten_and_type_check_initializer_info(init_info, &ctype)?
                } else if self.is_const {
                    vec![(IRValue::Immediate(0), 1)]
                } else {
                    return Ok(());
                };

                for (i, init) in inits.into_iter().enumerate() {
                    assert!(init.1 == 1);
                    self.push(IROp::CopyToOffset(init.0, loc.clone(), i));
                }
            }
        }
        Ok(())
    }

    fn parse_type_name(&mut self, type_name: &Node<TypeName>) -> Result<CType, IRGenerationError> {
        // NOTE: type_name.declarator.kind is always Abstract
        let ctype = CType::from_qualifiers(&type_name.node.specifiers, &mut self.scope)?;
        if let Some(declarator) = &type_name.node.declarator {
            return CType::from_declarator(declarator, &ctype, &mut self.scope);
        }
        Ok(ctype)
    }

    fn parse_initializer(
        &mut self,
        init: &Node<Initializer>,
    ) -> Result<InitializerInfo, IRGenerationError> {
        match &init.node {
            Initializer::Expression(expr) => Ok(InitializerInfo::Single(
                self.parse_expression(expr)?,
                expr.span,
            )),
            Initializer::List(expr_list) => {
                let mut res = vec![];
                for expr in expr_list {
                    if !expr.node.designation.is_empty() {
                        return Err(IRGenerationError {
                            err: IRGenerationErrorType::TODODesignatedInit,
                            span: expr.node.designation[0].span,
                        });
                    }
                    res.push(self.parse_initializer(&expr.node.initializer)?);
                }
                Ok(InitializerInfo::Compound(res, init.span))
            }
        }
    }

    fn parse_expression(
        &mut self,
        expr: &Node<Expression>,
    ) -> Result<(IRValue, CType), IRGenerationError> {
        match self.parse_expression_inner(expr)? {
            ExpressionOutput::Plain((val, ctype)) => Ok((val, ctype)),
            ExpressionOutput::Dereferenced((ptr, ctype)) => {
                let out = self.generate_pseudo(ctype.sizeof(&self.scope));
                self.push(IROp::One(
                    UnaryOp::Dereference,
                    ptr,
                    out.clone(),
                    IRType::from_ctype(&ctype, &self.scope),
                ));
                Ok((out, ctype))
            }
            ExpressionOutput::SubObject {
                base,
                subtype,
                offset,
            } => {
                let out = self.generate_pseudo(subtype.sizeof(&self.scope));
                self.push(IROp::CopyFromOffset(base, out.clone(), offset));
                Ok((out, subtype))
            }
        }
    }

    fn parse_expression_inner(
        &mut self,
        expr: &Node<Expression>,
    ) -> Result<ExpressionOutput, IRGenerationError> {
        use ExpressionOutput as Out;
        Ok(match &expr.node {
            Expression::Constant(constant) => Out::Plain(self.parse_constant(constant)?),
            Expression::UnaryOperator(unary_expr) => self.parse_unary_expression(unary_expr)?,
            Expression::BinaryOperator(binary_expr) => self.parse_binary_expression(binary_expr)?,
            Expression::Identifier(ident) => match self.scope.var_map.get(&ident.node.name) {
                None => {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::UnknownIdentifier,
                        span: ident.span,
                    });
                }
                Some((Some(loc), ctype)) => Out::Plain((loc.clone(), ctype.clone())),
                Some((None, CType::Function(_, _))) => {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::FunctionUsedAsVariable,
                        span: ident.span,
                    });
                }
                Some((None, _)) => unreachable!(),
            },
            Expression::Call(call_expr) => Out::Plain(self.parse_call(call_expr)?),

            // Type stuff
            Expression::Cast(cast_expr) => Out::Plain(self.parse_cast(cast_expr)?),
            Expression::SizeOfTy(sizeof_expr) => Out::Plain(self.parse_sizeof_type(sizeof_expr)?),
            Expression::SizeOfVal(sizeof_expr) => Out::Plain(self.parse_sizeof_value(sizeof_expr)?),
            Expression::AlignOf(_) => todo!("AlignOf {expr:?}"),

            // Struct stuff
            Expression::Member(expr) => self.parse_member_access(expr)?,
            Expression::OffsetOf(_) => todo!("OffsetOf {expr:?}"),

            // Other stuff
            Expression::StringLiteral(_) => todo!("StringLiteral {expr:?}"),
            Expression::GenericSelection(_) => todo!("GenericSelection {expr:?}"),
            Expression::CompoundLiteral(_) => todo!("CompoundLiteral {expr:?}"),
            Expression::Conditional(cond) => Out::Plain(self.parse_ternary(cond)?),
            Expression::Comma(_) => todo!("Comma {expr:?}"),
            Expression::VaArg(_) => todo!("VaArg {expr:?}"),

            // Extension
            Expression::Statement(_) => todo!("Statement {expr:?}"),
        })
    }

    fn parse_member_access(
        &mut self,
        expr: &Node<MemberExpression>,
    ) -> Result<ExpressionOutput, IRGenerationError> {
        use ExpressionOutput as Out;
        let inner = self.parse_expression_inner(&expr.node.expression)?;
        let base_type = match &inner {
            Out::Plain((_, ctype))
            | Out::Dereferenced((_, ctype))
            | Out::SubObject { subtype: ctype, .. } => ctype,
        };
        if let CType::Struct(tag_id) = base_type {
            let struct_data = self.scope.get_struct_by_id(*tag_id);
            let field_name = &expr.node.identifier.node.name;
            let field = struct_data.fields.get(field_name);

            match field {
                None => Err(IRGenerationError {
                    err: IRGenerationErrorType::InvalidStructMember(
                        field_name.clone(),
                        base_type.clone(),
                    ),
                    span: expr.node.identifier.span,
                }),
                Some((ctype, member_offset)) => match expr.node.operator.node {
                    MemberOperator::Direct => Ok(match &inner {
                        Out::Plain((base, _)) => Out::SubObject {
                            base: base.clone(),
                            subtype: ctype.clone(),
                            offset: *member_offset,
                        },
                        Out::SubObject { base, offset, .. } => Out::SubObject {
                            base: base.clone(),
                            subtype: ctype.clone(),
                            offset: offset + *member_offset,
                        },
                        Out::Dereferenced((val, _)) => {
                            let out = self.generate_pseudo(1);
                            self.push(IROp::AddPtr(
                                val.clone(),
                                IRValue::Immediate(*member_offset),
                                out.clone(),
                                1,
                            ));
                            Out::Dereferenced((out, ctype.clone()))
                        }
                    }),
                    MemberOperator::Indirect => Err(IRGenerationError {
                        err: IRGenerationErrorType::AttemptToAccessStructWithArrow(
                            base_type.clone(),
                        ),
                        span: expr.span,
                    }),
                },
            }
        } else if let CType::Pointer(CType::Struct(tag_id)) = base_type {
            let struct_data = self.scope.get_struct_by_id(*tag_id);
            let field_name = &expr.node.identifier.node.name;
            let field = struct_data.fields.get(field_name);

            match field {
                None => Err(IRGenerationError {
                    err: IRGenerationErrorType::InvalidStructMember(
                        field_name.clone(),
                        base_type.clone(),
                    ),
                    span: expr.node.identifier.span,
                }),
                Some((ctype, member_offset)) => match expr.node.operator.node {
                    MemberOperator::Indirect => Ok(match &inner {
                        Out::Plain((base, _)) => {
                            let out = self.generate_pseudo(1);
                            self.push(IROp::AddPtr(
                                base.clone(),
                                IRValue::Immediate(*member_offset),
                                out.clone(),
                                1,
                            ));
                            Out::Dereferenced((out, ctype.clone()))
                        }
                        Out::SubObject { base, offset, .. } => {
                            let ptr = self.generate_pseudo(1);
                            self.push(IROp::CopyFromOffset(base.clone(), ptr.clone(), *offset));
                            let out = self.generate_pseudo(1);
                            self.push(IROp::AddPtr(
                                ptr,
                                IRValue::Immediate(*member_offset),
                                out.clone(),
                                1,
                            ));
                            Out::Dereferenced((out, ctype.clone()))
                        }
                        Out::Dereferenced((val, _)) => {
                            let ptr = self.generate_pseudo(ctype.sizeof(&self.scope));
                            self.push(IROp::One(
                                UnaryOp::Dereference,
                                val.clone(),
                                ptr.clone(),
                                IRType::from_ctype(ctype, &self.scope),
                            ));
                            let out = self.generate_pseudo(1);
                            self.push(IROp::AddPtr(
                                ptr,
                                IRValue::Immediate(*member_offset),
                                out.clone(),
                                1,
                            ));
                            Out::Dereferenced((out, ctype.clone()))
                        }
                    }),
                    MemberOperator::Direct => Err(IRGenerationError {
                        err: IRGenerationErrorType::AttemptToAccessPointerToStructWithDot(
                            base_type.clone(),
                        ),
                        span: expr.span,
                    }),
                },
            }
        } else {
            Err(IRGenerationError {
                err: IRGenerationErrorType::AttemptToAccessNonStruct(base_type.clone()),
                span: expr.span,
            })
        }
    }

    fn parse_cast(
        &mut self,
        cast: &Node<CastExpression>,
    ) -> Result<(IRValue, CType), IRGenerationError> {
        let (expr, expr_type) = self.parse_expression(&cast.node.expression)?;
        let out_type = self.parse_type_name(&cast.node.type_name)?;
        let out = self.generate_pseudo(out_type.sizeof(&self.scope));

        self.push(IROp::Cast(
            IRType::from_ctype(&out_type, &self.scope),
            (expr, IRType::from_ctype(&expr_type, &self.scope)),
            out.clone(),
        ));
        Ok((out, out_type))
    }

    fn parse_sizeof_type(
        &mut self,
        expr: &Node<SizeOfTy>,
    ) -> Result<(IRValue, CType), IRGenerationError> {
        let ctype = self.parse_type_name(&expr.node.0)?;
        let size = ctype.sizeof(&self.scope);
        let out = self.generate_pseudo(CType::UnsignedInt.sizeof(&self.scope));
        self.ops.push(IROp::Copy(
            IRValue::Immediate(size),
            out.clone(),
            CType::SignedInt.sizeof(&self.scope),
        ));

        Ok((out, CType::UnsignedInt))
    }

    fn parse_sizeof_value(
        &mut self,
        expr: &Node<SizeOfVal>,
    ) -> Result<(IRValue, CType), IRGenerationError> {
        // construct a builder to be thrown away so the expression can be type checked
        let mut builder = TopLevelBuilder {
            ops: vec![],
            count: self.count,
            scope: self.scope.new_scope(),
            loop_id: None,
            break_last_seen: BreakTypes::None,
            is_const: false,
            switch_case_info: None,
            file_builder: self.file_builder,
            return_type: CType::Void,
        };
        let (_value, ctype) = builder.parse_expression(&expr.node.0)?;
        let size = ctype.sizeof(&self.scope);
        let out = self.generate_pseudo(CType::UnsignedInt.sizeof(&self.scope));
        self.ops.push(IROp::Copy(
            IRValue::Immediate(size),
            out.clone(),
            CType::SignedInt.sizeof(&self.scope),
        ));

        Ok((out, CType::UnsignedInt))
    }

    fn parse_ternary(
        &mut self,
        expr: &Node<ConditionalExpression>,
    ) -> Result<(IRValue, CType), IRGenerationError> {
        let (else_str, else_lbl) = self.generate_label("else");
        let (end_str, end_lbl) = self.generate_label("end");

        let (cond, _cond_type) = self.parse_expression(&expr.node.condition)?;
        self.push(IROp::CondBranch(BranchType::Zero, else_str, cond));
        let (temp1, temp1_type) = self.parse_expression(&expr.node.then_expression)?;
        let out = self.generate_pseudo(temp1_type.sizeof(&self.scope));
        self.push(IROp::Copy(
            temp1,
            out.clone(),
            temp1_type.sizeof(&self.scope),
        ));
        self.push(IROp::AlwaysBranch(end_str));
        self.push(else_lbl);
        let (temp2, temp2_type) = self.parse_expression(&expr.node.else_expression)?;
        self.push(IROp::Copy(
            temp2,
            out.clone(),
            temp2_type.sizeof(&self.scope),
        ));
        self.push(end_lbl);

        assert_eq!(temp2_type, temp1_type);
        Ok((out, temp2_type))
    }

    fn parse_call(
        &mut self,
        expr: &Node<CallExpression>,
    ) -> Result<(IRValue, CType), IRGenerationError> {
        let args = expr
            .node
            .arguments
            .iter()
            .map(|expr| self.parse_expression(expr))
            .collect::<Result<Vec<(IRValue, CType)>, _>>()?;

        match &expr.node.callee.node {
            Expression::Identifier(ident) => {
                let name = ident.node.name.clone();
                match self.scope.var_map.get(&name).cloned() {
                    None => Err(IRGenerationError {
                        err: IRGenerationErrorType::UnknownIdentifier,
                        span: ident.span,
                    })?,
                    Some((Some(_), CType::Function(..))) => unreachable!(),
                    Some((None, CType::Function(expected_args, return_type))) => {
                        let return_type = *return_type;
                        if expected_args.len() != args.len()
                            && !(matches!(expected_args[..], [CType::Void]) && args.is_empty())
                        {
                            return Err(IRGenerationError {
                                err: IRGenerationErrorType::IncorrectNumberOfArguments {
                                    expected: expected_args.len(),
                                    recieved: args.len(),
                                },
                                span: ident.span,
                            });
                        }

                        for i in 0..args.len() {
                            self.convert_to(args[i].clone(), &expected_args[i])
                                .map_err(|err| IRGenerationError {
                                    err,
                                    span: expr.node.arguments[i].span,
                                })?;
                        }

                        self.push(IROp::Call(
                            ident.node.name.clone(),
                            args.iter()
                                .map(|x| (x.0.clone(), x.1.sizeof(&self.scope)))
                                .collect(),
                        ));
                        let out = self.generate_pseudo(return_type.sizeof(&self.scope));
                        self.push(IROp::Copy(
                            IRValue::Register(0),
                            out.clone(),
                            return_type.sizeof(&self.scope),
                        ));

                        Ok((out, return_type))
                    }
                    _ => Err(IRGenerationError {
                        err: IRGenerationErrorType::CallNonFunction,
                        span: ident.span,
                    }),
                }
            }
            _ => Err(IRGenerationError {
                err: IRGenerationErrorType::CallNonFunction,
                span: expr.node.callee.span,
            }),
        }
    }

    fn parse_unary_expression(
        &mut self,
        expr: &Node<UnaryOperatorExpression>,
    ) -> Result<ExpressionOutput, IRGenerationError> {
        use ExpressionOutput as Out;
        match expr.node.operator.node {
            UnaryOperator::Address => match self.parse_expression_inner(&expr.node.operand)? {
                Out::Plain((val, ctype)) => {
                    let ptrtype = CType::Pointer(Box::new(ctype));
                    let out = self.generate_pseudo(ptrtype.sizeof(&self.scope));
                    self.push(IROp::AddressOf(val, out.clone()));
                    return Ok(Out::Plain((out, ptrtype)));
                }
                Out::Dereferenced(x) => return Ok(Out::Plain(x)),
                Out::SubObject {
                    base,
                    subtype,
                    offset,
                } => {
                    let out = self.generate_pseudo(subtype.sizeof(&self.scope));
                    self.push(IROp::AddressOf(base, out.clone()));
                    self.push(IROp::AddPtr(
                        out.clone(),
                        IRValue::Immediate(offset),
                        out.clone(),
                        1,
                    ));
                    return Ok(Out::Plain((out, subtype)));
                }
            },
            UnaryOperator::Indirection => {
                let (val, val_type) = self.parse_expression(&expr.node.operand)?;
                match val_type {
                    CType::Pointer(pointee_type) | CType::Array(pointee_type, _) => {
                        // will be dereferenced laterTM
                        return match *pointee_type {
                            // Value is already the pointer it is supposed to be
                            CType::Array(..) => Ok(Out::Plain((val, *pointee_type))),
                            CType::ImmediateArray(..) => unreachable!(),
                            // will be dereferenced laterTM
                            _ => Ok(Out::Dereferenced((val, *pointee_type))),
                        };
                    }
                    CType::ImmediateArray(pointee_type, _) => {
                        // array decay
                        let out = self.generate_pseudo(CType::UnsignedInt.sizeof(&self.scope));
                        self.push(IROp::AddressOf(val, out.clone()));
                        return match *pointee_type {
                            // value is now the pointer it is supposed to be
                            CType::Array(..) => Ok(Out::Plain((out, *pointee_type))),
                            CType::ImmediateArray(..) => unreachable!(),
                            // will be dereferenced laterTM
                            _ => Ok(Out::Dereferenced((out, *pointee_type))),
                        };
                    }
                    _ => Err(IRGenerationError {
                        err: IRGenerationErrorType::DereferenceNonPointer,
                        span: expr.node.operator.span,
                    })?,
                }
            }
            _ => (),
        }

        let (val, val_type) = self.parse_expression(&expr.node.operand)?;
        let (val, val_type) = self.attempt_array_decay((val, val_type));
        let out = self.generate_pseudo(val_type.sizeof(&self.scope));

        match expr.node.operator.node {
            UnaryOperator::Complement => {
                self.push(IROp::One(
                    UnaryOp::Complement,
                    val,
                    out.clone(),
                    IRType::from_ctype(&val_type, &self.scope),
                ));
            }
            UnaryOperator::Minus => self.push(IROp::One(
                UnaryOp::Minus,
                val,
                out.clone(),
                IRType::from_ctype(&val_type, &self.scope),
            )),
            UnaryOperator::Negate => {
                self.push(IROp::One(
                    UnaryOp::BooleanNegate,
                    val,
                    out.clone(),
                    IRType::from_ctype(&val_type, &self.scope),
                ));
                // ! is special, always returns Int
                return Ok(Out::Plain((out, CType::SignedInt)));
            }
            UnaryOperator::Plus => {
                self.push(IROp::Copy(val, out.clone(), val_type.sizeof(&self.scope)))
            } // silly

            // ++x, increment and evaluate to x+1
            UnaryOperator::PreIncrement => {
                self.push(IROp::Two(
                    BinOp::Add,
                    val.clone(),
                    IRValue::Immediate(1),
                    val.clone(),
                    IRType::from_ctype(&val_type, &self.scope),
                ));
                return Ok(Out::Plain((val, val_type)));
            }
            // --x, decrement and evaluate to x-1
            UnaryOperator::PreDecrement => {
                self.push(IROp::Two(
                    BinOp::Sub,
                    val.clone(),
                    IRValue::Immediate(1),
                    val.clone(),
                    IRType::from_ctype(&val_type, &self.scope),
                ));
                return Ok(Out::Plain((val, val_type)));
            }

            // x++, increment and evaluate to x
            UnaryOperator::PostIncrement => {
                self.push(IROp::Copy(
                    val.clone(),
                    out.clone(),
                    val_type.sizeof(&self.scope),
                ));
                self.push(IROp::Two(
                    BinOp::Add,
                    val.clone(),
                    IRValue::Immediate(1),
                    val,
                    IRType::from_ctype(&val_type, &self.scope),
                ));
            }
            // x--
            UnaryOperator::PostDecrement => {
                self.push(IROp::Copy(
                    val.clone(),
                    out.clone(),
                    val_type.sizeof(&self.scope),
                ));
                self.push(IROp::Two(
                    BinOp::Sub,
                    val.clone(),
                    IRValue::Immediate(1),
                    val,
                    IRType::from_ctype(&val_type, &self.scope),
                ));
            }

            // Memory stuff
            UnaryOperator::Address | UnaryOperator::Indirection => unreachable!(),
        }

        Ok(Out::Plain((out, val_type)))
    }

    fn parse_binary_expression(
        &mut self,
        expr: &Node<BinaryOperatorExpression>,
    ) -> Result<ExpressionOutput, IRGenerationError> {
        use BinaryOperator as CBinOp;
        if matches!(expr.node.operator.node, CBinOp::LogicalAnd) {
            let (skip_label_str, skip_label) = self.generate_label("logical_skip");
            let (end_label_str, end_label) = self.generate_label("logical_end");

            let (lhs, lhs_type) = self.parse_expression(&expr.node.lhs)?;
            let (lhs, _lhs_type) = self.attempt_array_decay((lhs, lhs_type));
            self.push(IROp::CondBranch(
                BranchType::Zero,
                skip_label_str.clone(),
                lhs,
            ));
            let (rhs, rhs_type) = self.parse_expression(&expr.node.rhs)?;
            let (rhs, _rhs_type) = self.attempt_array_decay((rhs, rhs_type));
            self.push(IROp::CondBranch(BranchType::Zero, skip_label_str, rhs));
            let out = self.generate_pseudo(CType::SignedInt.sizeof(&self.scope));
            self.ops.push(IROp::Copy(
                IRValue::Immediate(1),
                out.clone(),
                CType::SignedInt.sizeof(&self.scope),
            ));

            self.push(IROp::AlwaysBranch(end_label_str));
            self.push(skip_label);
            self.ops.push(IROp::Copy(
                IRValue::Immediate(0),
                out.clone(),
                CType::SignedInt.sizeof(&self.scope),
            ));
            self.push(end_label);
            return Ok(ExpressionOutput::Plain((out, CType::SignedInt)));
        }
        if matches!(expr.node.operator.node, CBinOp::LogicalOr) {
            let (skip_label_str, skip_label) = self.generate_label("logical_skip");
            let (end_label_str, end_label) = self.generate_label("logical_end");

            let (lhs, lhs_type) = self.parse_expression(&expr.node.lhs)?;
            let (lhs, _lhs_type) = self.attempt_array_decay((lhs, lhs_type));
            self.push(IROp::CondBranch(
                BranchType::NonZero,
                skip_label_str.clone(),
                lhs,
            ));
            let (rhs, rhs_type) = self.parse_expression(&expr.node.rhs)?;
            let (rhs, _rhs_type) = self.attempt_array_decay((rhs, rhs_type));
            self.push(IROp::CondBranch(BranchType::NonZero, skip_label_str, rhs));
            let out = self.generate_pseudo(CType::SignedInt.sizeof(&self.scope));
            self.ops.push(IROp::Copy(
                IRValue::Immediate(0),
                out.clone(),
                CType::SignedInt.sizeof(&self.scope),
            ));

            self.push(IROp::AlwaysBranch(end_label_str));
            self.push(skip_label);
            self.ops.push(IROp::Copy(
                IRValue::Immediate(1),
                out.clone(),
                CType::SignedInt.sizeof(&self.scope),
            ));
            self.push(end_label);
            return Ok(ExpressionOutput::Plain((out, CType::SignedInt)));
        }

        let ((lhs, lhs_type), (rhs, rhs_type), out_type, assignment_status) =
            match expr.node.operator.node {
                CBinOp::AssignPlus
                | CBinOp::AssignMinus
                | CBinOp::AssignMultiply
                | CBinOp::AssignDivide
                | CBinOp::AssignModulo
                | CBinOp::AssignShiftLeft
                | CBinOp::AssignShiftRight
                | CBinOp::AssignBitwiseAnd
                | CBinOp::AssignBitwiseXor
                | CBinOp::AssignBitwiseOr
                | CBinOp::Assign => {
                    // on assignment, rhs is cast to typeof(lhs)
                    let (lhs, lhs_type, assignment_status) =
                        match self.parse_expression_inner(&expr.node.lhs)? {
                            ExpressionOutput::Plain((val, ctype)) => {
                                let (val, ctype) = self.attempt_array_decay((val, ctype));
                                (val.clone(), ctype, AssignmentStatus::AssigningToValue(val))
                            }
                            ExpressionOutput::Dereferenced((val, ctype)) => {
                                // NOTE: I think this isn't needed?
                                let (val, ctype) = self.attempt_array_decay((val, ctype));
                                (
                                    val.clone(),
                                    ctype,
                                    AssignmentStatus::AssigningToPointer(val),
                                )
                            }
                            ExpressionOutput::SubObject {
                                base,
                                subtype,
                                offset,
                            } => {
                                let out = self.generate_pseudo(subtype.sizeof(&self.scope));
                                self.push(IROp::CopyFromOffset(base.clone(), out.clone(), offset));
                                (
                                    out,
                                    subtype,
                                    AssignmentStatus::AssigningToSubObject(base, offset),
                                )
                            }
                        };
                    let (rhs, rhs_type) = self.parse_expression(&expr.node.rhs)?;
                    let (mut rhs, rhs_type) = self.attempt_array_decay((rhs, rhs_type));
                    if !matches!(expr.node.operator.node, CBinOp::AssignMinus)
                        && !rhs_type.is_pointer()
                    {
                        rhs = self
                            .convert_to((rhs, rhs_type.clone()), &lhs_type)
                            .map_err(|err| IRGenerationError {
                                err,
                                span: expr.span,
                            })?;
                    }
                    (
                        (lhs, lhs_type.clone()),
                        (rhs, rhs_type),
                        lhs_type,
                        assignment_status,
                    )
                }

                CBinOp::Less
                | CBinOp::Index
                | CBinOp::Greater
                | CBinOp::LessOrEqual
                | CBinOp::GreaterOrEqual
                | CBinOp::Equals
                | CBinOp::NotEquals
                | CBinOp::Plus
                | CBinOp::Minus
                | CBinOp::Multiply
                | CBinOp::Divide
                | CBinOp::Modulo
                | CBinOp::ShiftLeft
                | CBinOp::ShiftRight
                | CBinOp::BitwiseAnd
                | CBinOp::BitwiseXor
                | CBinOp::BitwiseOr => {
                    let (lhs, lhs_type) = self.parse_expression(&expr.node.lhs)?;
                    let (mut lhs, lhs_type) = self.attempt_array_decay((lhs, lhs_type));
                    let (rhs, rhs_type) = self.parse_expression(&expr.node.rhs)?;
                    let (mut rhs, rhs_type) = self.attempt_array_decay((rhs, rhs_type));

                    let common_type = CType::get_common(&lhs_type, &rhs_type).map_err(|err| {
                        IRGenerationError {
                            err: err.into(),
                            span: expr.span,
                        }
                    })?;

                    if !matches!(
                        expr.node.operator.node,
                        CBinOp::Plus | CBinOp::AssignPlus | CBinOp::Minus | CBinOp::AssignMinus
                    ) && !lhs_type.is_pointer()
                    {
                        lhs = self
                            .convert_to((lhs, lhs_type.clone()), &common_type)
                            .map_err(|err| IRGenerationError {
                                err,
                                span: expr.span,
                            })?;
                    }
                    if !matches!(
                        expr.node.operator.node,
                        CBinOp::Plus | CBinOp::AssignPlus | CBinOp::Minus | CBinOp::AssignMinus
                    ) && !rhs_type.is_pointer()
                    {
                        rhs = self
                            .convert_to((rhs, rhs_type.clone()), &common_type)
                            .map_err(|err| IRGenerationError {
                                err,
                                span: expr.span,
                            })?;
                    }
                    (
                        (lhs, lhs_type),
                        (rhs, rhs_type),
                        common_type,
                        AssignmentStatus::NoAssignment,
                    )
                }

                // dealt with higher up
                CBinOp::LogicalAnd | CBinOp::LogicalOr => unreachable!(),
            };

        let out = self.generate_pseudo(out_type.sizeof(&self.scope));

        let op = match expr.node.operator.node {
            // FIXME: this codegen is wrong because it still generates the casts beforehand
            CBinOp::Plus | CBinOp::AssignPlus => match (lhs_type, rhs_type) {
                (CType::Pointer(..), CType::Pointer(..)) => {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::PointerAddition,
                        span: expr.span,
                    });
                }
                (CType::Pointer(inner), _) => {
                    IROp::AddPtr(lhs, rhs, out.clone(), inner.sizeof(&self.scope))
                }
                (_, CType::Pointer(inner)) => {
                    IROp::AddPtr(rhs, lhs, out.clone(), inner.sizeof(&self.scope))
                }

                _ => IROp::Two(
                    BinOp::Add,
                    lhs,
                    rhs,
                    out.clone(),
                    IRType::from_ctype(&out_type, &self.scope),
                ),
            },
            CBinOp::Index => match (lhs_type, rhs_type) {
                (CType::Pointer(..) | CType::Array(..), CType::Pointer(..) | CType::Array(..)) => {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::IndexPointerWithPointer,
                        span: expr.span,
                    });
                }
                (CType::Pointer(inner) | CType::Array(inner, _), _) => {
                    self.push(IROp::AddPtr(
                        lhs,
                        rhs,
                        out.clone(),
                        inner.sizeof(&self.scope),
                    ));
                    return match *inner {
                        // Value is already the pointer it is supposed to be
                        CType::Array(..) => Ok(ExpressionOutput::Plain((out, *inner))),
                        CType::ImmediateArray(..) => unreachable!(),
                        // will be dereferenced laterTM
                        _ => Ok(ExpressionOutput::Dereferenced((out, *inner))),
                    };
                }
                (_, CType::Pointer(inner) | CType::Array(inner, _)) => {
                    self.push(IROp::AddPtr(
                        rhs,
                        lhs,
                        out.clone(),
                        inner.sizeof(&self.scope),
                    ));
                    return match *inner {
                        // Value is already the pointer it is supposed to be
                        CType::Array(..) => Ok(ExpressionOutput::Plain((out, *inner))),
                        CType::ImmediateArray(..) => unreachable!(),
                        // will be dereferenced laterTM
                        _ => Ok(ExpressionOutput::Dereferenced((out, *inner))),
                    };
                }
                _ => {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::IndexWithoutPointer,
                        span: expr.span,
                    });
                }
            },
            // FIXME: this codegen is wrong because it still generates the casts beforehand
            CBinOp::Minus | CBinOp::AssignMinus => match (lhs_type, rhs_type) {
                (
                    CType::Pointer(inner_l) | CType::Array(inner_l, ..),
                    CType::Pointer(inner_r) | CType::Array(inner_r, ..),
                ) => {
                    assert_eq!(inner_l, inner_r);
                    let difference = self.generate_pseudo(out_type.sizeof(&self.scope));
                    self.push(IROp::Two(
                        BinOp::Sub,
                        lhs,
                        rhs,
                        difference.clone(),
                        IRType::from_ctype(&out_type, &self.scope),
                    ));
                    IROp::Two(
                        BinOp::Div,
                        difference,
                        IRValue::Immediate(inner_l.sizeof(&self.scope)),
                        out.clone(),
                        IRType::from_ctype(&out_type, &self.scope),
                    )
                }
                (CType::Pointer(inner) | CType::Array(inner, _), _) => {
                    let negated = self.generate_pseudo(CType::UnsignedInt.sizeof(&self.scope));
                    self.push(IROp::One(
                        UnaryOp::Complement,
                        rhs,
                        negated.clone(),
                        IRType::from_ctype(&out_type, &self.scope),
                    ));
                    IROp::AddPtr(lhs, negated, out.clone(), inner.sizeof(&self.scope))
                }
                (_, CType::Pointer(..) | CType::Array(..)) => {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::PointerSubtraction,
                        span: expr.span,
                    });
                }

                _ => IROp::Two(
                    BinOp::Sub,
                    lhs,
                    rhs,
                    out.clone(),
                    IRType::from_ctype(&out_type, &self.scope),
                ),
            },
            CBinOp::Multiply | CBinOp::AssignMultiply => IROp::Two(
                BinOp::Mult,
                lhs,
                rhs,
                out.clone(),
                IRType::from_ctype(&out_type, &self.scope),
            ),
            CBinOp::Divide | CBinOp::AssignDivide => IROp::Two(
                BinOp::Div,
                lhs,
                rhs,
                out.clone(),
                IRType::from_ctype(&out_type, &self.scope),
            ),
            CBinOp::Modulo | CBinOp::AssignModulo => IROp::Two(
                BinOp::Mod,
                lhs,
                rhs,
                out.clone(),
                IRType::from_ctype(&out_type, &self.scope),
            ),

            CBinOp::Less => IROp::Two(
                BinOp::LessThan,
                lhs,
                rhs,
                out.clone(),
                IRType::from_ctype(&CType::SignedInt, &self.scope),
            ),
            CBinOp::Greater => IROp::Two(
                BinOp::GreaterThan,
                lhs,
                rhs,
                out.clone(),
                IRType::from_ctype(&CType::SignedInt, &self.scope),
            ),
            CBinOp::LessOrEqual => IROp::Two(
                BinOp::LessOrEqual,
                lhs,
                rhs,
                out.clone(),
                IRType::from_ctype(&CType::SignedInt, &self.scope),
            ),
            CBinOp::GreaterOrEqual => IROp::Two(
                BinOp::GreaterOrEqual,
                lhs,
                rhs,
                out.clone(),
                IRType::from_ctype(&CType::SignedInt, &self.scope),
            ),
            CBinOp::Equals => IROp::Two(
                BinOp::Equal,
                lhs,
                rhs,
                out.clone(),
                IRType::from_ctype(&CType::SignedInt, &self.scope),
            ),
            CBinOp::NotEquals => IROp::Two(
                BinOp::NotEqual,
                lhs,
                rhs,
                out.clone(),
                IRType::from_ctype(&CType::SignedInt, &self.scope),
            ),

            // bitwise ops
            CBinOp::ShiftLeft | CBinOp::AssignShiftLeft => IROp::Two(
                BinOp::ShiftLeft,
                lhs,
                rhs,
                out.clone(),
                IRType::from_ctype(&out_type, &self.scope),
            ),
            CBinOp::ShiftRight | CBinOp::AssignShiftRight => IROp::Two(
                BinOp::ShiftRight,
                lhs,
                rhs,
                out.clone(),
                IRType::from_ctype(&out_type, &self.scope),
            ),
            CBinOp::BitwiseAnd | CBinOp::AssignBitwiseAnd => IROp::Two(
                BinOp::BitwiseAnd,
                lhs,
                rhs,
                out.clone(),
                IRType::from_ctype(&out_type, &self.scope),
            ),
            CBinOp::BitwiseXor | CBinOp::AssignBitwiseXor => IROp::Two(
                BinOp::BitwiseXor,
                lhs,
                rhs,
                out.clone(),
                IRType::from_ctype(&out_type, &self.scope),
            ),
            CBinOp::BitwiseOr | CBinOp::AssignBitwiseOr => IROp::Two(
                BinOp::BitwiseOr,
                lhs,
                rhs,
                out.clone(),
                IRType::from_ctype(&out_type, &self.scope),
            ),

            CBinOp::Assign => IROp::Copy(rhs, out.clone(), out_type.sizeof(&self.scope)),

            // dealt with higher up
            CBinOp::LogicalAnd | CBinOp::LogicalOr => unreachable!(),
        };
        self.push(op);

        match assignment_status {
            AssignmentStatus::NoAssignment => (),
            AssignmentStatus::AssigningToPointer(destination) => {
                self.push(IROp::Store(
                    out.clone(),
                    destination,
                    out_type.sizeof(&self.scope),
                ));
            }
            AssignmentStatus::AssigningToValue(destination) => {
                self.push(IROp::Copy(
                    out.clone(),
                    destination,
                    out_type.sizeof(&self.scope),
                ));
            }
            AssignmentStatus::AssigningToSubObject(base, offset) => {
                self.push(IROp::CopyToOffset(out.clone(), base, offset));
            }
        }
        Ok(ExpressionOutput::Plain((out, out_type)))
    }

    #[expect(clippy::match_same_arms)]
    fn parse_constant(&self, val: &Node<Constant>) -> Result<(IRValue, CType), IRGenerationError> {
        // TODO: add checks for size to stop overflow or smth
        match &val.node {
            Constant::Integer(int) => {
                let x = integer_constant_to_usize(int);
                let ctype = match int.suffix {
                    IntegerSuffix {
                        imaginary: true, ..
                    } => Err(IRGenerationErrorType::TODOImaginary),
                    IntegerSuffix {
                        size: IntegerSize::Int,
                        unsigned: false,
                        imaginary: false,
                    } => Ok(CType::SignedInt),
                    IntegerSuffix {
                        size: IntegerSize::Int,
                        unsigned: true,
                        imaginary: false,
                    } => Ok(CType::UnsignedInt),
                    IntegerSuffix {
                        size: IntegerSize::Long,
                        unsigned: false,
                        imaginary: false,
                    } => Ok(CType::SignedLong),
                    IntegerSuffix {
                        size: IntegerSize::Long,
                        unsigned: true,
                        imaginary: false,
                    } => Ok(CType::UnsignedLong),
                    IntegerSuffix {
                        size: IntegerSize::LongLong,
                        unsigned: false,
                        imaginary: false,
                    } => Err(IRGenerationErrorType::TODOTypeSuffix),
                    IntegerSuffix {
                        size: IntegerSize::LongLong,
                        unsigned: true,
                        imaginary: false,
                    } => Err(IRGenerationErrorType::TODOTypeSuffix),
                }
                .map_err(|err| IRGenerationError {
                    err,
                    span: val.span,
                })?;
                Ok((IRValue::Immediate(x), ctype))
            }
            Constant::Character(str) => {
                let x = char_constant_to_usize(str).map_err(|err| IRGenerationError {
                    err,
                    span: val.span,
                })?;
                // NOTE: This is not a typo, char literals are ints.
                Ok((IRValue::Immediate(x), CType::SignedInt))
            }
            Constant::Float(_) => Err(IRGenerationError {
                err: IRGenerationErrorType::TODOFloats,
                span: val.span,
            }),
        }
    }

    #[expect(clippy::unnecessary_wraps)]
    fn convert_to(
        &mut self,
        input: (IRValue, CType),
        ctype: &CType,
    ) -> Result<IRValue, IRGenerationErrorType> {
        if input.1 == *ctype {
            Ok(input.0)
        } else {
            let out = self.generate_pseudo(ctype.sizeof(&self.scope));
            // TODO: check cast is valid
            self.push(IROp::Cast(
                IRType::from_ctype(ctype, &self.scope),
                (input.0, IRType::from_ctype(&input.1, &self.scope)),
                out.clone(),
            ));
            Ok(out)
        }
    }

    fn attempt_array_decay(&mut self, val: (IRValue, CType)) -> (IRValue, CType) {
        match val.1 {
            CType::Array(inner_type, ..) => (val.0, CType::Pointer(inner_type)),
            CType::ImmediateArray(inner_type, ..) => {
                let new = self.generate_pseudo(CType::SignedInt.sizeof(&self.scope));
                self.push(IROp::AddressOf(val.0.clone(), new.clone()));
                (new, CType::Pointer(inner_type))
            }
            _ => val,
        }
    }

    fn generate_pseudo(&mut self, size: usize) -> IRValue {
        self.count += 1;
        IRValue::Psuedo {
            name: "tmp.".to_owned() + &self.count.to_string(),
            size,
        }
    }

    fn generate_named_pseudo(&mut self, name: String, size: usize) -> IRValue {
        self.count += 1;
        IRValue::Psuedo {
            name: name + "." + &self.count.to_string(),
            size,
        }
    }

    fn generate_unique_static_pseudo(&mut self, name: String, size: usize) -> IRValue {
        self.count += 1;
        IRValue::StaticPsuedo {
            name: name + "." + &self.count.to_string(),
            linkable: false,
            size,
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

    const fn new_loop_id(&mut self) {
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

#[expect(clippy::from_str_radix_10)]
fn integer_constant_to_usize(int: &Integer) -> usize {
    match int.base {
        IntegerBase::Decimal => usize::from_str_radix(&int.number, 10).unwrap(),
        IntegerBase::Octal => usize::from_str_radix(&int.number, 8).unwrap(),
        IntegerBase::Hexadecimal => usize::from_str_radix(&int.number, 16).unwrap(),
        IntegerBase::Binary => usize::from_str_radix(&int.number, 2).unwrap(),
    }
}

// minimal error handling is required here, because lang-c validates the literal
fn char_constant_to_usize(str: &str) -> Result<usize, IRGenerationErrorType> {
    let mut chars = str.chars().skip(1).collect::<Vec<_>>();
    chars.pop();

    Ok(match &chars {
        // octal
        ['\\', '0'..'7', ..] => {
            let num: String = chars.into_iter().skip(1).collect();
            usize::from_str_radix(&num, 8).unwrap()
        }
        // hex
        ['\\', 'x', ..] => {
            let num: String = chars.into_iter().skip(2).collect();
            usize::from_str_radix(&num, 16).unwrap()
        }
        // normal escape
        ['\\', ..] => {
            match chars[1] {
                // Single quote
                '\'' => 39,
                // Double quote
                '"' => 34,
                // Question mark
                '?' => 63,
                // Backslash
                '\\' => 92,
                // Audible alert
                'a' => 7,
                // Backspace
                'b' => 8,
                // Form feed
                'f' => 12,
                // New line
                'n' => 10,
                // Carriage return
                'r' => 13,
                // Horizontal tab
                't' => 9,
                // Vertical tab
                'v' => 11,
                a => unreachable!("\\{a} is not a valid escape sequence"),
            }
        }
        // normal character
        _ => {
            if chars.len() != 1 {
                return Err(IRGenerationErrorType::LongCharLiteral);
            }
            chars[0] as usize
        }
    })
}

fn parse_array_length(decl: &Node<ArrayDeclarator>) -> Result<usize, IRGenerationError> {
    assert_eq!(
        decl.node.qualifiers.len(),
        0,
        "Array qualifiers not yet supported"
    );
    Ok(match &decl.node.size {
        ArraySize::VariableExpression(expr) => {
            match &expr.node {
                // TODO: implement a "constant mode" or something, that
                // attempts to evaluate a tree of expressions?
                // the compiler will need to be able to run some of the IR at
                // compile time anyways, so perhaps it just parses the tree in
                // a mode that only allows "known constant" things, and then
                // evaluates all the IR it produces using the function that
                // evaluates IR at compile time? idk.
                Expression::Constant(val) => match &val.node {
                    Constant::Float(_) => {
                        return Err(IRGenerationError {
                            err: IRGenerationErrorType::NonIntegerArrayLength,
                            span: decl.span,
                        });
                    }
                    Constant::Integer(int) => integer_constant_to_usize(int),
                    Constant::Character(str) => {
                        char_constant_to_usize(str).map_err(|err| IRGenerationError {
                            err,
                            span: decl.span,
                        })?
                    }
                },
                _ => {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::TODONonConstantArrayLength,
                        span: expr.span,
                    });
                }
            }
        }
        ArraySize::StaticExpression(expr) => {
            match &expr.node {
                // TODO: ditto on that constant mode
                Expression::Constant(val) => match &val.node {
                    Constant::Float(_) => {
                        return Err(IRGenerationError {
                            err: IRGenerationErrorType::NonIntegerArrayLength,
                            span: val.span,
                        });
                    }
                    Constant::Integer(int) => integer_constant_to_usize(int),
                    Constant::Character(str) => {
                        char_constant_to_usize(str).map_err(|err| IRGenerationError {
                            err,
                            span: decl.span,
                        })?
                    }
                },
                _ => {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::NonStaticInStaticBlock,
                        span: expr.span,
                    });
                }
            }
        }

        ArraySize::VariableUnknown | ArraySize::Unknown => {
            return Err(IRGenerationError {
                err: IRGenerationErrorType::TODOUnknownArrayLength,
                span: decl.span,
            });
        }
    })
}
