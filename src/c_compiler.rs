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
    cell::OnceCell,
    collections::HashMap,
    fmt::{self, Debug, Display},
    io::{self, Write},
    mem,
    process::{Command, Stdio},
    rc::Rc,
};

use lang_c::{
    ast::{
        ArrayDeclarator, ArraySize, AsmStatement, BinaryOperator, BinaryOperatorExpression,
        BlockItem, CallExpression, CastExpression, ConditionalExpression, Constant, Declaration,
        DeclarationSpecifier, Declarator, DeclaratorKind, DerivedDeclarator, DoWhileStatement,
        Ellipsis, EnumType, Expression, ExternalDeclaration, Float, FloatBase, FloatFormat,
        ForInitializer, ForStatement, FunctionDefinition, FunctionSpecifier, GnuAsmOperand,
        Identifier, IfStatement, Initializer, Integer, IntegerBase, IntegerSize, IntegerSuffix,
        Label, LabeledStatement, MemberExpression, MemberOperator, PointerDeclarator, SizeOfTy,
        SizeOfVal, SpecifierQualifier, Statement, StorageClassSpecifier, StructDeclaration,
        StructKind, StructType, SwitchStatement, TypeName, TypeSpecifier, UnaryOperator,
        UnaryOperatorExpression, WhileStatement,
    },
    driver::{Flavor, parse_preprocessed},
    span::{Node, Span},
};
use thiserror::Error;

use crate::{
    ARGS,
    ir::{BinOp, BranchType, IROp, IRTopLevel, IRType, IRValue, UnaryOp},
};

#[derive(Error, Debug)]
pub enum IRGenerationErrorType {
    #[error("Invalid coersion between {0} and {1}")]
    InvalidCoercion(Box<str>, Box<str>),
    #[error("Unknown type")]
    TODOUnknownType,
    #[error("Invalid declaration specifier combination")]
    InvalidTypeSpecifier,
    #[error("Unknown struct")]
    InvalidStructType,
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
    #[error("Typedef keyword is invalid in this location")]
    InvalidTypedefLocation,
    #[error("Typedef name used as variable name")]
    TypedefUsedAsVariable,
    #[error("Typedef is missing declarator")]
    TypedefMissingDeclarator,
    #[error("Unknown function identifier")]
    UnknownFunction,
    #[error("Non-integer array length")]
    NonIntegerArrayLength,
    #[error("Arrays must be initialized with an initializer list or string literal")]
    InvalidArrayInit,
    #[error(
        "(TODO) Non trivial array length. VLAs are not supported and neither are constant expressions."
    )]
    TODONonConstantArrayLength,
    #[error("(TODO) Enumerator value is non-trival, only immediate constants are allowed for now")]
    TODONonConstantEnumValue,
    #[error("Enumerator value is not an integer constant.")]
    NonIntegerEnumValue,
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
    InvalidASMSymbol(Box<str>),
    #[error("Excess elements in array initializer")]
    ExcessElementsInInitializer,
    #[error("Char literals must be one character (or an escape sequence)")]
    LongCharLiteral,
    #[error("Cannot combine with previous {0}")]
    MultipleStorageClasses(StorageDuration),

    #[error("INTERNAL: A non func declarator in the func declarator parser? type: {0:?}")]
    INTERNALNonFuncDeclaratorInFuncDeclaratorParser(PointerDeclarator),

    // todos
    #[error("GNU block types are not supported")]
    TODOBlockTypes,
    #[error("Imaginary numbers are not supported")]
    TODOImaginary,
    #[error("Given type suffix is not yet implemented")]
    TODOTypeSuffix,
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
    #[error("Auto is not yet supported")]
    TODOAuto,
    #[error("Inline is not yet supported")]
    TODOInline,
    #[error("Noreturn is not yet supported")]
    TODONoreturn,
    #[error("Typeof is not yet supported")]
    TODOTypeof,

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
    #[error("Type '{0}' is a struct, use '.' instead of '->'")]
    AttemptToAccessStructWithArrow(Box<str>),
    #[error("Type '{0}' is a pointer to a struct, use '->' instead of '.'")]
    AttemptToAccessPointerToStructWithDot(Box<str>),
    #[error("Attempt to access member '{0}' non struct type: '{1}'")]
    AttemptToAccessNonStruct(Box<str>, Box<str>),
    #[error("'{0}' is not a member of '{1}'")]
    InvalidStructMember(Box<str>, Box<str>),
    #[error("Struct has no members")]
    StructWithoutMembers,
    #[error("Cannot redefine existing struct in same scope")]
    StructRedefinition,

    // TODO: store the span of the location of the first declaration!
    #[error("Type '{0}' does not match earlier defined '{1}'")]
    NonMatchingDeclarations(Box<str>, Box<str>),
    #[error("Type '{0}' is not compatible with '{1}'")]
    IncompatibleTypes(Box<str>, Box<str>),
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
                .with_message("Preprocessor (gcc) error")
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructData {
    pub name: Option<String>,
    pub fields: IndexMap<String, (CType, usize)>,
    pub size: usize,
}

impl StructData {
    fn new(name: Option<String>) -> Self {
        Self {
            name,
            fields: IndexMap::new(),
            size: 0,
        }
    }
}

#[derive(Debug, Clone, Eq)]
pub enum CType {
    Bool,

    UnsignedChar,
    SignedChar,
    Char,

    SignedShort,
    SignedInt,
    SignedLong,

    UnsignedShort,
    UnsignedInt,
    UnsignedLong,

    Double,

    Void,
    Pointer(Box<CType>),
    // A fancy pointer
    Array(Box<CType>, usize),
    // The immediate location of an array
    ImmediateArray(Box<CType>, usize),
    Function(Box<[CType]>, Box<CType>),
    Struct(TagID),
}

// this makes me unhappy
#[allow(clippy::match_same_arms)]
impl PartialEq for CType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool, Self::Bool) => true,

            (Self::UnsignedChar, Self::UnsignedChar) => true,
            (Self::SignedChar, Self::SignedChar) => true,
            (Self::Char, Self::Char) => true,

            (Self::SignedShort, Self::SignedShort) => true,
            (Self::SignedInt, Self::SignedInt) => true,
            (Self::SignedLong, Self::SignedLong) => true,

            (Self::UnsignedShort, Self::UnsignedShort) => true,
            (Self::UnsignedInt, Self::UnsignedInt) => true,
            (Self::UnsignedLong, Self::UnsignedLong) => true,

            (Self::Double, Self::Double) => true,

            (Self::Void, Self::Void) => true,
            (Self::Pointer(a), Self::Pointer(b)) => a == b,
            (
                Self::Array(a, size_a) | Self::ImmediateArray(a, size_a),
                Self::Array(b, size_b) | Self::ImmediateArray(b, size_b),
            ) => (a == b) && (size_a == size_b),
            (Self::Function(args_a, return_a), Self::Function(args_b, return_b)) => {
                (args_a == args_b) && (return_a == return_b)
            }
            (Self::Struct(a), Self::Struct(b)) => a == b,
            _ => false,
        }
    }
}

impl CType {
    pub const fn is_integer(&self) -> bool {
        matches!(
            self,
            Self::SignedInt
                | Self::SignedShort
                | Self::SignedLong
                | Self::SignedChar
                | Self::UnsignedInt
                | Self::UnsignedShort
                | Self::UnsignedLong
                | Self::UnsignedChar
                | Self::Char
        )
    }

    pub const fn is_pointer(&self) -> bool {
        matches!(self, Self::Pointer(..) | Self::Array(..))
    }

    pub const fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    pub const fn is_function_pointer(&self) -> bool {
        matches!(self, Self::Pointer(Self::Function(..)))
    }

    pub fn zero_init(&self) -> Vec<(IRValue, usize)> {
        match self {
            Self::Pointer(..)
            | Self::Bool
            | Self::Char
            | Self::SignedChar
            | Self::SignedShort
            | Self::SignedInt
            | Self::SignedLong
            | Self::UnsignedChar
            | Self::UnsignedShort
            | Self::UnsignedInt
            | Self::UnsignedLong => {
                vec![(IRValue::int(0), 1)]
            }
            Self::Double => {
                vec![(IRValue::float(0.0), 1)]
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

    pub fn get_common<'a>(type1: &'a Self, type2: &'a Self) -> Result<Self, (&'a Self, &'a Self)> {
        // If the same, no conversions are needed
        if type1 == type2 {
            return Ok(type1.clone());
        }

        match (type1, type2) {
            (Self::Pointer(Self::Void), Self::Pointer(..))
            | (Self::Pointer(..), Self::Pointer(Self::Void)) => {
                return Ok(Self::Pointer(Box::new(Self::Void)));
            }
            _ => (),
        }
        // TODO: array types!

        // If one is a pointer, they must be the same (already checked) or one must be an integer
        if matches!(type1, Self::Pointer(_)) {
            return if type2.is_integer() {
                Ok(type1.clone())
            } else {
                Err((type1, type2))
            };
        }

        if matches!(type2, Self::Pointer(_)) {
            return if type1.is_integer() {
                Ok(type2.clone())
            } else {
                Err((type1, type2))
            };
        }

        Ok(Self::SignedInt)
    }

    fn display_type_inner(&self, scope: &ScopeInfo, inner: &str) -> String {
        match self {
            Self::Bool => format!("bool {inner}"),

            Self::UnsignedChar => format!("unsigned char {inner}"),
            Self::SignedChar => format!("signed char {inner}"),
            Self::Char => format!("char {inner}"),

            Self::SignedShort => format!("short {inner}"),
            Self::SignedInt => format!("int {inner}"),
            Self::SignedLong => format!("long {inner}"),

            Self::UnsignedShort => format!("unsigned short {inner}"),
            Self::UnsignedInt => format!("unsigned int {inner}"),
            Self::UnsignedLong => format!("unsigned long {inner}"),

            Self::Double => format!("double {inner}"),

            Self::Void => format!("void {inner}"),
            Self::Pointer(inner_type) => match inner_type {
                Self::Array(..) | Self::ImmediateArray(..) | Self::Function(..) => {
                    inner_type.display_type_inner(scope, &format!("(*{inner})"))
                }
                _ => inner_type.display_type_inner(scope, &format!("*{inner}")),
            },
            Self::Array(inner_type, len) | Self::ImmediateArray(inner_type, len) => {
                inner_type.display_type_inner(scope, &format!("{inner}[{len}]"))
            }
            Self::Function(args, return_type) => {
                let args = if args.is_empty() {
                    "void"
                } else {
                    &args
                        .iter()
                        .map(|a| a.display_type(scope))
                        .collect::<Vec<_>>()
                        .join(", ")
                };
                return_type.display_type_inner(scope, &format!("{inner}({args})",))
            }
            Self::Struct(tag_id) => {
                let tag_data = scope.get_struct_by_id(*tag_id);
                tag_data.name.map_or_else(
                    || format!("struct ANON {inner}"),
                    |name| format!("struct {name} {inner}"),
                )
            }
        }
        .trim()
        .to_string()
    }

    pub fn function<I: IntoIterator<Item = Self>>(args: I, return_type: Self) -> Self {
        Self::Function(
            args.into_iter()
                // decay arrays in args into pointers
                .map(|ctype| match ctype {
                    Self::Array(inner, _) => *inner,
                    Self::ImmediateArray(..) => panic!("immediate array in function args"),
                    //Self::Void => panic!("void in function args"),
                    ctype => ctype,
                })
                .collect(),
            Box::new(return_type),
        )
    }

    pub fn display_type(&self, scope: &ScopeInfo) -> Box<str> {
        self.display_type_inner(scope, "").into()
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
    Union,
}

type TagID = usize;

#[derive(Debug, Clone)]
struct TagData {
    source_depth: usize,
    tag_id: TagID,
    tag_type: TagType,
}

// TODO: make the invariants in here specified in type system
#[derive(Debug, Clone)]
enum VarData {
    // CType is never function hopefully
    Variable(IRValue, CType),
    // CType is always function hopefully
    Function(CType),
    Typedef(CType),
}

impl VarData {
    const fn ctype(&self) -> &CType {
        match self {
            Self::Variable(_, ctype) | Self::Function(ctype) | Self::Typedef(ctype) => ctype,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct ScopeInfo {
    // IRValue is only None when CType is Function
    var_map: HashMap<String, VarData>,
    tag_map: HashMap<String, TagData>,
    structs: Rc<Mutex<Vec<OnceCell<StructData>>>>,
    depth: usize,
}

impl ScopeInfo {
    fn new_scope(&self) -> Self {
        let mut scope = self.clone();
        scope.depth += 1;
        scope
    }

    fn generate_struct_id(&self, name: Option<&str>) -> (TagID, bool) {
        match name {
            None => (self.structs.lock().len(), true),
            Some(name) => match self.tag_map.get(name) {
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
            },
        }
    }

    fn insert_incomplete_struct(&mut self, name: Option<&str>) -> TagID {
        let (id, new) = self.generate_struct_id(name);
        if !new {
            return id;
        }
        let mut structs = self.structs.lock();
        if let Some(name) = name {
            self.tag_map.insert(
                name.to_string(),
                TagData {
                    source_depth: self.depth,
                    tag_id: id,
                    tag_type: TagType::Struct,
                },
            );
        }
        structs.push(OnceCell::new());
        id
    }

    fn insert_struct(
        &mut self,
        name: Option<&str>,
        struct_data: StructData,
    ) -> Result<TagID, IRGenerationErrorType> {
        let (id, new) = self.generate_struct_id(name);
        if !new && self.structs.lock()[id].get().is_some() {
            return Err(IRGenerationErrorType::StructRedefinition);
        }
        self.structs.lock().push(OnceCell::from(struct_data));

        if let Some(name) = name {
            self.tag_map.insert(
                name.to_string(),
                TagData {
                    source_depth: self.depth,
                    tag_id: id,
                    tag_type: TagType::Struct,
                },
            );
        }
        Ok(id)
    }

    pub fn get_struct_by_id(&self, id: TagID) -> StructData {
        let structs = self.structs.lock();
        // TODO: don't clone here
        structs[id].get().expect("all tag ids are valid").clone()
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
    cmd.stderr(Stdio::piped());

    for item in &config.cpp_options {
        cmd.arg(item);
    }

    let mut cmd = cmd.spawn().expect("Failed to spawn gcc");

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
    pub fn parse_c(
        source: &[u8],
        filename: &str,
        linked: &[&str],
    ) -> Result<Vec<IRTopLevel>, Box<CompilerError>> {
        let mut builder = Self {
            count: 0,
            scope: ScopeInfo::default(),
            funcs: vec![],
        };

        let mut command = vec![
            // don't include normal libc
            "-nostdinc".into(),
            // only preprocess
            "-E".into(),
            // read c file from stdin
            "-xc".into(),
            "-".into(),
        ];

        for include in linked {
            command.push("-I".into());
            command.push((*include).to_string());
        }

        let config = lang_c::driver::Config {
            flavor: Flavor::GnuC11,
            cpp_command: "gcc".into(),
            cpp_options: command,
        };
        let preproccessed =
            preprocess(&config, source).map_err(|err| CompilerError::ParseError {
                err: lang_c::driver::Error::PreprocessorError(err),
                filename: filename.to_string(),
            })?;

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
        let info =
            SimpleDeclarationInfo::from_decl_specifiers(&func.node.specifiers, &mut self.scope)?;
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
        let size = if builder.return_type.is_void() {
            1
        } else {
            builder.return_type.sizeof(&builder.scope)
        };
        builder.push(IROp::Return(IRValue::int(0), size));

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

enum IntType {
    Char,
    Short,
    Int,
    Long,
    LongLong,
}

enum Sign {
    Unsigned,
    Signed,
    None,
}

impl CType {
    #[expect(clippy::match_same_arms)]
    fn from_specifiers(
        ctypes: &[&Node<TypeSpecifier>],
        scope: &mut ScopeInfo,
        is_statement: bool,
    ) -> Result<Self, IRGenerationError> {
        assert!(!ctypes.is_empty());

        if ctypes.len() == 1 {
            return match &ctypes[0].node {
                TypeSpecifier::Void => Ok(Self::Void),
                TypeSpecifier::Char => Ok(Self::Char),
                TypeSpecifier::Short => Ok(Self::SignedShort),
                TypeSpecifier::Int => Ok(Self::SignedInt),
                TypeSpecifier::Long => Ok(Self::SignedLong),
                TypeSpecifier::Float => Err(IRGenerationErrorType::TODOUnknownType),
                TypeSpecifier::Double => Ok(Self::Double),
                TypeSpecifier::Signed => Ok(Self::SignedInt),
                TypeSpecifier::Unsigned => Ok(Self::UnsignedInt),
                TypeSpecifier::Bool => Ok(Self::Bool),
                TypeSpecifier::Complex => Err(IRGenerationErrorType::TODOImaginary),
                TypeSpecifier::Atomic(..) => Err(IRGenerationErrorType::TODOUnknownType),
                TypeSpecifier::TypedefName(ident) => {
                    match scope.var_map.get(&ident.node.name) {
                        Some(VarData::Typedef(ctype)) => Ok(ctype.clone()),
                        // lang_c should make this impossible
                        Some(_) | None => unreachable!("{:?}", scope),
                    }
                }
                TypeSpecifier::TypeOf(..) => Err(IRGenerationErrorType::TODOTypeof),
                TypeSpecifier::TS18661Float(..) => Err(IRGenerationErrorType::TODOUnknownType),
                TypeSpecifier::Enum(enum_data) => Ok(Self::parse_enum_data(enum_data, scope)?),
                TypeSpecifier::Struct(struct_data) => {
                    Ok(Self::parse_struct_data(struct_data, scope, is_statement)?)
                }
            }
            .map_err(|err| IRGenerationError {
                err,
                span: ctypes[0].span,
            });
        }
        // it's an integer or invalid

        let mut int_type = None;
        let mut sign = Sign::None;
        let mut int_count = 0;

        for x in ctypes {
            match &x.node {
                TypeSpecifier::Char => match int_type {
                    None => {
                        int_type = Some(IntType::Char);
                        Ok(())
                    }
                    Some(_) => Err(IRGenerationErrorType::InvalidTypeSpecifier),
                },
                TypeSpecifier::Short => match int_type {
                    None | Some(IntType::Int) => {
                        int_type = Some(IntType::Short);
                        Ok(())
                    }
                    Some(_) => Err(IRGenerationErrorType::InvalidTypeSpecifier),
                },
                TypeSpecifier::Int => {
                    int_count += 1;
                    assert!(int_count <= 1);
                    match int_type {
                        None => {
                            int_type = Some(IntType::Int);
                            Ok(())
                        }
                        Some(IntType::Int) => Err(IRGenerationErrorType::InvalidTypeSpecifier),
                        Some(ty) => {
                            int_type = Some(ty);
                            Ok(())
                        }
                    }
                }
                TypeSpecifier::Long => match int_type {
                    None | Some(IntType::Int) => {
                        int_type = Some(IntType::Long);
                        Ok(())
                    }
                    Some(IntType::Long) => {
                        int_type = Some(IntType::LongLong);
                        Ok(())
                    }
                    Some(_) => Err(IRGenerationErrorType::InvalidTypeSpecifier),
                },
                TypeSpecifier::Signed => match sign {
                    Sign::None => {
                        sign = Sign::Signed;
                        Ok(())
                    }
                    _ => Err(IRGenerationErrorType::InvalidTypeSpecifier),
                },
                TypeSpecifier::Unsigned => match sign {
                    Sign::None => {
                        sign = Sign::Unsigned;
                        Ok(())
                    }
                    _ => Err(IRGenerationErrorType::InvalidTypeSpecifier),
                },

                TypeSpecifier::Complex => Err(IRGenerationErrorType::TODOImaginary),
                _ => Err(IRGenerationErrorType::InvalidTypeSpecifier),
            }
            .map_err(|err| IRGenerationError { err, span: x.span })?;
        }

        Ok(match (int_type.unwrap_or(IntType::Int), sign) {
            (IntType::Char, Sign::None) => Self::Char,
            (IntType::Char, Sign::Unsigned) => Self::UnsignedChar,
            (IntType::Char, Sign::Signed) => Self::SignedChar,

            (IntType::Short, Sign::Unsigned) => Self::UnsignedShort,
            (IntType::Short, Sign::Signed | Sign::None) => Self::SignedShort,

            (IntType::Int, Sign::Unsigned) => Self::UnsignedInt,
            (IntType::Int, Sign::Signed | Sign::None) => Self::SignedInt,

            (IntType::Long, Sign::Unsigned) => Self::UnsignedLong,
            (IntType::Long, Sign::Signed | Sign::None) => Self::SignedLong,

            (IntType::LongLong, Sign::Unsigned) => todo!("unsigned long long"),
            (IntType::LongLong, Sign::Signed | Sign::None) => todo!("long long"),
        })
    }

    fn parse_enum_data(
        enum_type: &Node<EnumType>,
        scope: &mut ScopeInfo,
    ) -> Result<Self, IRGenerationError> {
        if enum_type.node.enumerators.is_empty() {
            // using existing enum
            Err(IRGenerationError {
                err: IRGenerationErrorType::TODOUnknownType,
                span: enum_type.span,
            })
        } else {
            let mut last_value = 0;
            for member in &enum_type.node.enumerators {
                // FIXME: check that insert is valid here
                let value = match &member.node.expression {
                    // TODO: allow arbitrary expressions here.
                    // this will require refactoring a lot
                    // because it would require the whole builder to be available all the way in here
                    // also probably that const eval mode
                    Some(expr) => match &expr.node {
                        Expression::Constant(val) => match &val.node {
                            Constant::Float(_) => {
                                return Err(IRGenerationError {
                                    err: IRGenerationErrorType::NonIntegerEnumValue,
                                    span: expr.span,
                                });
                            }
                            Constant::Integer(int) => integer_constant_to_usize(int),
                            Constant::Character(str) => {
                                char_constant_to_usize(str).map_err(|err| IRGenerationError {
                                    err,
                                    span: expr.span,
                                })?
                            }
                        },
                        _ => {
                            return Err(IRGenerationError {
                                err: IRGenerationErrorType::TODONonConstantEnumValue,
                                span: expr.span,
                            });
                        }
                    },
                    None => last_value,
                };
                last_value = value + 1;
                scope.var_map.insert(
                    member.node.identifier.node.name.clone(),
                    VarData::Variable(IRValue::int(value), Self::SignedInt),
                );
            }
            // constructing new enum
            //FIXME: this is super cheating, but should allow some programs to compile
            Ok(Self::SignedInt)
            /*match &enum_type.node.identifier {
                None => Err(IRGenerationError {
                    err: IRGenerationErrorType::TODOUnknownType,
                    span: enum_type.span,
                }),
                Some(name) => Err(IRGenerationError {
                    err: IRGenerationErrorType::TODOUnknownType,
                    span: enum_type.span,
                }),
            }*/
        }
    }

    fn parse_struct_data(
        struct_type: &Node<StructType>,
        scope: &mut ScopeInfo,
        is_statement: bool,
    ) -> Result<Self, IRGenerationError> {
        let struct_kind = struct_type.node.kind.node;

        let name: Option<String> = struct_type
            .node
            .identifier
            .clone()
            .map(|inner| inner.node.name);

        let tag_id = match &struct_type.node.declarations {
            // means it's just a definition not a declaration
            None => {
                let j = match name {
                    Some(ref name) => scope.tag_map.get(name),
                    None => None,
                };
                match j {
                    // if it has not been declared yet then just insert
                    None => {
                        if is_statement {
                            scope.insert_incomplete_struct(name.as_deref())
                        } else {
                            return Err(IRGenerationError {
                                err: IRGenerationErrorType::InvalidStructType,
                                span: struct_type.span,
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
                            scope.insert_incomplete_struct(name.as_deref())
                        } else {
                            assert_eq!(*tag_type, TagType::Struct);
                            *tag_id
                        }
                    }
                }
            }
            // it's a definition
            Some(decls) => {
                if decls.is_empty() {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::StructWithoutMembers,
                        span: struct_type.span,
                    });
                }
                let mut struct_data = StructData::new(name.clone());
                for decl in decls {
                    match &decl.node {
                        StructDeclaration::StaticAssert(..) => {
                            return Err(IRGenerationError {
                                err: IRGenerationErrorType::TODOAbstractDeclarator,
                                span: decl.span,
                            });
                        }
                        StructDeclaration::Field(field) => {
                            let ctype = Self::from_qualifiers(&field.node.specifiers, scope)?;
                            assert!(field.node.declarators[0].node.bit_width.is_none());
                            for declarator in &field.node.declarators {
                                let declarator = &declarator.node.declarator.clone().unwrap();
                                let mut ctype = Self::from_declarator(declarator, &ctype, scope)?;
                                if let Self::Array(inner, length) = ctype {
                                    ctype = Self::ImmediateArray(inner, length);
                                }
                                let sizeof = ctype.sizeof(scope);
                                match struct_kind {
                                    StructKind::Struct => struct_data.fields.insert(
                                        parse_declarator_name(declarator)?,
                                        (ctype, struct_data.size),
                                    ),
                                    StructKind::Union => struct_data
                                        .fields
                                        .insert(parse_declarator_name(declarator)?, (ctype, 0)),
                                };
                                struct_data.size += sizeof;
                            }
                        }
                    }
                }
                scope
                    .insert_struct(name.as_deref(), struct_data)
                    .map_err(|err| IRGenerationError {
                        span: struct_type.span,
                        err,
                    })?
            }
        };
        Ok(Self::Struct(tag_id))
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

        for modifier in &declarator.node.pointer {
            match &modifier.node {
                PointerDeclarator::Pointer(qualifiers) => {
                    assert_eq!(qualifiers.len(), 0, "Pointer qualifiers not yet supported");
                    out = Self::Pointer(Box::new(out));
                }
                PointerDeclarator::Block(_) => {
                    return Err(IRGenerationError {
                        err: IRGenerationErrorType::TODOBlockTypes,
                        span: modifier.span,
                    });
                }
            }
        }

        for modifier in declarator.node.derived.iter().rev() {
            match &modifier.node {
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
                        let info = SimpleDeclarationInfo::from_decl_specifiers(
                            &param.node.specifiers,
                            scope,
                        )?;
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
            }
        }

        out = if let Some(params) = param_list {
            let params = if matches!(params, [Self::Void]) {
                vec![]
            } else {
                params
            };
            Self::function(params, out)
        } else {
            out
        };

        match &declarator.node.kind.node {
            DeclaratorKind::Identifier(_) | DeclaratorKind::Abstract => (),
            DeclaratorKind::Declarator(nested_decl) => {
                out = Self::from_declarator(nested_decl, &out, scope)?;
            }
        }

        Ok(out)
    }
}

#[derive(Debug)]
pub enum StorageDuration {
    Default,
    Extern,
    Static,
}

impl fmt::Display for StorageDuration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::Default => write!(f, "default"),
            Self::Extern => write!(f, "extern"),
            Self::Static => write!(f, "static"),
        }
    }
}

#[derive(Debug)]
struct SimpleDeclarationInfo {
    c_type: CType,
    duration: StorageDuration,
}

#[derive(Debug)]
struct DeclarationInfo {
    c_type: CType,
    duration: StorageDuration,
    typedef: bool,
}

impl SimpleDeclarationInfo {
    fn from_decl_specifiers(
        specifiers: &[Node<DeclarationSpecifier>],
        scope: &mut ScopeInfo,
    ) -> Result<Self, IRGenerationError> {
        let info = DeclarationInfo::from_decl_specifiers(specifiers, scope, false, false)?;
        assert!(!info.typedef);
        Ok(Self {
            duration: info.duration,
            c_type: info.c_type,
        })
    }
}

impl DeclarationInfo {
    fn from_decl_specifiers(
        specifiers: &[Node<DeclarationSpecifier>],
        scope: &mut ScopeInfo,
        is_statement: bool,
        has_declarator: bool,
    ) -> Result<Self, IRGenerationError> {
        let mut c_types = vec![];
        let mut duration = None;
        let mut typedef = false;

        for specifier in specifiers {
            match &specifier.node {
                DeclarationSpecifier::StorageClass(spec) => {
                    if let Some(duration) = duration {
                        return Err(IRGenerationError {
                            err: IRGenerationErrorType::MultipleStorageClasses(duration),
                            span: specifier.span,
                        });
                    }
                    match spec.node {
                        StorageClassSpecifier::Typedef => {
                            // jank, just to ban other storage durations
                            duration = Some(StorageDuration::Default);
                            // typedef only allowed when in a statement
                            if has_declarator {
                                typedef = true;
                            } else if is_statement {
                                return Err(IRGenerationError {
                                    err: IRGenerationErrorType::TypedefMissingDeclarator,
                                    span: spec.span,
                                });
                            } else {
                                return Err(IRGenerationError {
                                    err: IRGenerationErrorType::InvalidTypedefLocation,
                                    span: spec.span,
                                });
                            }
                        }
                        StorageClassSpecifier::Extern => duration = Some(StorageDuration::Extern),
                        StorageClassSpecifier::Static => duration = Some(StorageDuration::Static),
                        StorageClassSpecifier::ThreadLocal => println!("_Thread_local is ignored"),
                        StorageClassSpecifier::Auto => Err(IRGenerationError {
                            err: IRGenerationErrorType::TODOAuto,
                            span: specifier.span,
                        })?,
                        StorageClassSpecifier::Register => println!("register keyword is ignored"),
                    }
                }
                DeclarationSpecifier::TypeSpecifier(spec) => c_types.push(spec),
                // Consider implementing just volatile
                DeclarationSpecifier::TypeQualifier(_) => {
                    println!("WARNING: All type qualifiers are ignored {specifier:?}");
                }
                DeclarationSpecifier::Function(spec) => match spec.node {
                    FunctionSpecifier::Inline => eprintln!("WARNING: Inline is ignored"),
                    FunctionSpecifier::Noreturn => Err(IRGenerationError {
                        err: IRGenerationErrorType::TODONoreturn,
                        span: specifier.span,
                    })?,
                },
                DeclarationSpecifier::Alignment(_) => println!(
                    "WARNING: All declaration alignment specifiers are ignored {specifier:?}"
                ),
                DeclarationSpecifier::Extension(_) => println!(
                    "WARNING: GNU declaration specifier extensions are ignored {specifier:?}"
                ),
            }
        }

        Ok(Self {
            duration: duration.unwrap_or(StorageDuration::Default),
            c_type: CType::from_specifiers(&c_types, scope, is_statement)?,
            typedef,
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
                        let info = SimpleDeclarationInfo::from_decl_specifiers(
                            &param.node.specifiers,
                            &mut self.scope,
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

                DerivedDeclarator::Array(array_decl) => {
                    // TODO: does this make sense?
                    let size = parse_array_length(array_decl)?;
                    self.return_type = CType::Array(Box::new(self.return_type.clone()), size);
                }
            }
        }

        for node in &decl.node.pointer {
            match &node.node {
                PointerDeclarator::Pointer(qualifiers) => {
                    assert_eq!(qualifiers.len(), 0, "Pointer qualifiers not yet supported");
                    self.return_type = CType::Pointer(Box::new(self.return_type.clone()));
                }
                PointerDeclarator::Block(..) => {
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
            VarData::Function(CType::function(
                params.iter().map(|(_, ctype)| ctype).cloned(),
                self.return_type.clone(),
            )),
        );
        self.file_builder.scope.var_map.insert(
            name,
            VarData::Function(CType::function(
                params.iter().map(|(_, ctype)| ctype).cloned(),
                self.return_type.clone(),
            )),
        );

        let mut i = 1;
        for (name, ctype) in params.iter().cloned() {
            let size = ctype.sizeof(&self.scope);
            self.scope
                .var_map
                .insert(name, VarData::Variable(IRValue::Stack(i), ctype));
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
                    self.push(IROp::Return(out, final_type.sizeof(&self.scope)));
                } else {
                    // return type check not needed, as this is only reachable via UB
                    self.push(IROp::Return(IRValue::int(0), 1));
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
        let (c_value, ctype) = self.parse_expression(&op.node.value)?;
        let asm_value = Self::parse_asm_symbolic(&op.node.asm_location)
            .map_err(|err| IRGenerationError { err, span: op.span })?;
        if input {
            self.push(IROp::Copy(c_value, asm_value, ctype.sizeof(&self.scope)));
        } else {
            self.push(IROp::Copy(asm_value, c_value, ctype.sizeof(&self.scope)));
        }
        Ok(())
    }

    fn parse_asm_symbolic(str: &str) -> Result<IRValue, IRGenerationErrorType> {
        if let Some(rest) = str.strip_prefix('r') {
            Ok(IRValue::Register(rest.parse().unwrap()))
        } else if str == "bstack" {
            Ok(IRValue::BefungeStack)
        } else {
            Err(IRGenerationErrorType::InvalidASMSymbol(str.into()))
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
                            target_type.display_type(&self.scope),
                            rhs_type.display_type(&self.scope),
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
        let info = DeclarationInfo::from_decl_specifiers(
            &decls.node.specifiers,
            &mut self.scope,
            decls.node.declarators.is_empty(),
            !decls.node.declarators.is_empty(),
        )?;

        if info.typedef {
            for decl in &decls.node.declarators {
                let name = parse_declarator_name(&decl.node.declarator)?;
                let ctype =
                    CType::from_declarator(&decl.node.declarator, &info.c_type, &mut self.scope)?;
                self.scope.var_map.insert(name, VarData::Typedef(ctype));
            }
            return Ok(());
        }

        // FIXME: this is horrific.
        // TODO: sort out repeated declarations
        for decl in &decls.node.declarators {
            let name = parse_declarator_name(&decl.node.declarator)?;
            let mut ctype =
                CType::from_declarator(&decl.node.declarator, &info.c_type, &mut self.scope)?;

            if let CType::Function(..) = ctype {
                match self.scope.var_map.get(&name) {
                    Some(VarData::Function(prev_ctype)) => {
                        if *prev_ctype != ctype {
                            return Err(IRGenerationError {
                                err: IRGenerationErrorType::NonMatchingDeclarations(
                                    ctype.display_type(&self.scope),
                                    prev_ctype.display_type(&self.scope),
                                ),
                                span: decl.span,
                            });
                        }
                    }
                    _ => {
                        self.scope.var_map.insert(name, VarData::Function(ctype));
                    }
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
                                VarData::Typedef(..) => Err(IRGenerationError {
                                    err: IRGenerationErrorType::TypedefUsedAsVariable,
                                    span: decl.node.declarator.span,
                                })?,
                                // FIXME: this should now be doable, look inside parse_identifier
                                VarData::Function(_) => Err(IRGenerationError {
                                    err: IRGenerationErrorType::FunctionUsedAsVariable,
                                    span: decl.node.declarator.span,
                                })?,
                                VarData::Variable(loc, stored_ctype) => {
                                    if *stored_ctype != ctype {
                                        return Err(IRGenerationError {
                                            err: IRGenerationErrorType::NonMatchingDeclarations(
                                                ctype.display_type(&self.scope),
                                                stored_ctype.display_type(&self.scope),
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
                                .insert(name.clone(), VarData::Variable(j.clone(), ctype.clone()));
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
                .insert(name, VarData::Variable(loc.clone(), ctype.clone()));

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
                    ctype.zero_init()
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
                } else if self.is_const && !matches!(info.duration, StorageDuration::Extern) {
                    ctype.zero_init()
                } else {
                    continue;
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
                self.push(IROp::Dereference(
                    ptr,
                    out.clone(),
                    CType::sizeof(&ctype, &self.scope),
                ));
                Ok((out, ctype))
            }
            ExpressionOutput::SubObject {
                base,
                subtype,
                offset,
            } => {
                let size = subtype.sizeof(&self.scope);
                let out = self.generate_pseudo(subtype.sizeof(&self.scope));
                for i in 0..size {
                    let tmp = self.generate_pseudo(1);
                    // TODO: add a copy that has both offsets
                    self.push(IROp::CopyFromOffset(base.clone(), tmp.clone(), offset + i));
                    self.push(IROp::CopyToOffset(tmp.clone(), out.clone(), i));
                }
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
            Expression::Identifier(ident) => Out::Plain(self.parse_identifier(ident)?),
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
        let field_name = &expr.node.identifier.node.name;

        if let CType::Struct(tag_id) = base_type {
            let struct_data = self.scope.get_struct_by_id(*tag_id);
            let field = struct_data.fields.get(field_name);

            match field {
                None => Err(IRGenerationError {
                    err: IRGenerationErrorType::InvalidStructMember(
                        field_name.clone().into(),
                        base_type.display_type(&self.scope),
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
                                IRValue::int(*member_offset),
                                out.clone(),
                                1,
                            ));
                            Out::Dereferenced((out, ctype.clone()))
                        }
                    }),
                    MemberOperator::Indirect => Err(IRGenerationError {
                        err: IRGenerationErrorType::AttemptToAccessStructWithArrow(
                            base_type.display_type(&self.scope),
                        ),
                        span: expr.span,
                    }),
                },
            }
        } else if let CType::Pointer(CType::Struct(tag_id)) = base_type {
            let struct_data = self.scope.get_struct_by_id(*tag_id);
            let field = struct_data.fields.get(field_name);

            match field {
                None => Err(IRGenerationError {
                    err: IRGenerationErrorType::InvalidStructMember(
                        field_name.clone().into(),
                        base_type.display_type(&self.scope),
                    ),
                    span: expr.node.identifier.span,
                }),
                Some((ctype, member_offset)) => match expr.node.operator.node {
                    MemberOperator::Indirect => Ok(match &inner {
                        Out::Plain((base, _)) => {
                            let out = self.generate_pseudo(1);
                            self.push(IROp::AddPtr(
                                base.clone(),
                                IRValue::int(*member_offset),
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
                                IRValue::int(*member_offset),
                                out.clone(),
                                1,
                            ));
                            Out::Dereferenced((out, ctype.clone()))
                        }
                        Out::Dereferenced((val, _)) => {
                            let ptr = self.generate_pseudo(ctype.sizeof(&self.scope));
                            self.push(IROp::Dereference(
                                val.clone(),
                                ptr.clone(),
                                CType::sizeof(ctype, &self.scope),
                            ));
                            let out = self.generate_pseudo(1);
                            self.push(IROp::AddPtr(
                                ptr,
                                IRValue::int(*member_offset),
                                out.clone(),
                                1,
                            ));
                            Out::Dereferenced((out, ctype.clone()))
                        }
                    }),
                    MemberOperator::Direct => Err(IRGenerationError {
                        err: IRGenerationErrorType::AttemptToAccessPointerToStructWithDot(
                            base_type.display_type(&self.scope),
                        ),
                        span: expr.span,
                    }),
                },
            }
        } else {
            Err(IRGenerationError {
                err: IRGenerationErrorType::AttemptToAccessNonStruct(
                    field_name.clone().into(),
                    base_type.display_type(&self.scope),
                ),
                span: expr.span,
            })
        }
    }

    fn parse_cast(
        &mut self,
        cast: &Node<CastExpression>,
    ) -> Result<(IRValue, CType), IRGenerationError> {
        let (expr, expr_type) = self.parse_expression(&cast.node.expression)?;
        let (expr, expr_type) = self.attempt_array_decay((expr, expr_type));
        let out_type = self.parse_type_name(&cast.node.type_name)?;

        let out = self
            .convert_to((expr, expr_type), &out_type)
            .map_err(|err| IRGenerationError {
                span: cast.span,
                err,
            })?;
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
            IRValue::int(size),
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
            IRValue::int(size),
            out.clone(),
            CType::SignedInt.sizeof(&self.scope),
        ));

        Ok((out, CType::UnsignedInt))
    }

    // TODO: check and coerce types properly here
    fn parse_ternary(
        &mut self,
        expr: &Node<ConditionalExpression>,
    ) -> Result<(IRValue, CType), IRGenerationError> {
        let (else_str, else_lbl) = self.generate_label("else");
        let (end_str, end_lbl) = self.generate_label("end");

        let (cond, _cond_type) = self.parse_expression(&expr.node.condition)?;
        // TODO: assert cond_type is booleanish?
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

        let common_type =
            CType::get_common(&temp2_type, &temp1_type).map_err(|err| IRGenerationError {
                err: IRGenerationErrorType::InvalidCoercion(
                    err.0.display_type(&self.scope),
                    err.1.display_type(&self.scope),
                ),
                span: expr.span,
            })?;
        // TODO: small correctness issue here, but fixing it would require moving
        // the cast inside both branches, which would require parsing both expressions
        // before constructing the branches, which would require the same kind of jank
        // as there is in switch statements w/ mem::swap
        let out = self
            .convert_to((out, temp2_type), &common_type)
            .map_err(|err| IRGenerationError {
                err,
                span: expr.span,
            })?;
        Ok((out, common_type))
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
            .collect::<Result<Vec<(IRValue, CType)>, _>>()?
            .into_iter()
            .map(|val| self.attempt_array_decay(val))
            .collect::<Vec<_>>();

        match &expr.node.callee.node {
            Expression::Identifier(ident) => {
                let name = ident.node.name.clone();
                match self.scope.var_map.get(&name).cloned() {
                    None => Err(IRGenerationError {
                        err: IRGenerationErrorType::UnknownFunction,
                        span: ident.span,
                    })?,
                    Some(VarData::Function(CType::Function(expected_args, return_type))) => {
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
                        if return_type == CType::Void {
                            Ok((IRValue::int(0), return_type))
                        } else {
                            let out = self.generate_pseudo(return_type.sizeof(&self.scope));
                            self.push(IROp::GetReturnValue(
                                out.clone(),
                                return_type.sizeof(&self.scope),
                            ));
                            Ok((out, return_type))
                        }
                    }
                    Some(VarData::Typedef(..)) => Err(IRGenerationError {
                        err: IRGenerationErrorType::TypedefUsedAsVariable,
                        span: ident.span,
                    })?,
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
                    if ctype.is_function_pointer() {
                        return Ok(Out::Plain((val, ctype)));
                    }

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
                    if subtype.is_function_pointer() {
                        return Ok(Out::SubObject {
                            base,
                            subtype,
                            offset,
                        });
                    }

                    let out = self.generate_pseudo(subtype.sizeof(&self.scope));
                    self.push(IROp::AddressOf(base, out.clone()));
                    self.push(IROp::AddPtr(
                        out.clone(),
                        IRValue::int(offset),
                        out.clone(),
                        1,
                    ));
                    return Ok(Out::Plain((out, subtype)));
                }
            },
            UnaryOperator::Indirection => {
                let (val, val_type) = self.parse_expression(&expr.node.operand)?;
                if val_type.is_function_pointer() {
                    return Ok(Out::Plain((val, val_type)));
                }

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
                self.push(IROp::Copy(val, out.clone(), val_type.sizeof(&self.scope)));
            } // silly

            // ++x, increment and evaluate to x+1
            UnaryOperator::PreIncrement => {
                self.push(IROp::Two(
                    BinOp::Add,
                    val.clone(),
                    IRValue::int(1),
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
                    IRValue::int(1),
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
                    IRValue::int(1),
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
                    IRValue::int(1),
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
                IRValue::int(1),
                out.clone(),
                CType::SignedInt.sizeof(&self.scope),
            ));

            self.push(IROp::AlwaysBranch(end_label_str));
            self.push(skip_label);
            self.ops.push(IROp::Copy(
                IRValue::int(0),
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
                IRValue::int(0),
                out.clone(),
                CType::SignedInt.sizeof(&self.scope),
            ));

            self.push(IROp::AlwaysBranch(end_label_str));
            self.push(skip_label);
            self.ops.push(IROp::Copy(
                IRValue::int(1),
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
                            err: IRGenerationErrorType::InvalidCoercion(
                                err.0.display_type(&self.scope),
                                err.1.display_type(&self.scope),
                            ),
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
                        IRValue::int(inner_l.sizeof(&self.scope)),
                        out.clone(),
                        IRType::from_ctype(&out_type, &self.scope),
                    )
                }
                (CType::Pointer(inner) | CType::Array(inner, _), _) => {
                    let negated = self.generate_pseudo(CType::UnsignedInt.sizeof(&self.scope));
                    self.push(IROp::One(
                        UnaryOp::Minus,
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

    fn parse_identifier(
        &mut self,
        ident: &Node<Identifier>,
    ) -> Result<(IRValue, CType), IRGenerationError> {
        let x = self.scope.var_map.get(&ident.node.name);
        match x {
            None => Err(IRGenerationError {
                err: IRGenerationErrorType::UnknownIdentifier,
                span: ident.span,
            }),
            Some(VarData::Typedef(..)) => Err(IRGenerationError {
                err: IRGenerationErrorType::TypedefUsedAsVariable,
                span: ident.span,
            })?,
            Some(VarData::Variable(loc, ctype)) => Ok((loc.clone(), ctype.clone())),
            Some(VarData::Function(ctype)) => {
                let ctype = ctype.clone();
                let out = self.generate_pseudo(1);
                self.push(IROp::GetIdOfFunction(ident.node.name.clone(), out.clone()));
                Ok((out, CType::Pointer(Box::new(ctype))))
            }
        }
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
                Ok((IRValue::int(x), ctype))
            }
            Constant::Character(str) => {
                let x = char_constant_to_usize(str).map_err(|err| IRGenerationError {
                    err,
                    span: val.span,
                })?;
                // NOTE: This is not a typo, char literals are ints.
                Ok((IRValue::int(x), CType::SignedInt))
            }
            Constant::Float(float) => {
                let x = float_constant_to_value(float).map_err(|err| IRGenerationError {
                    err,
                    span: val.span,
                })?;
                Ok((x, CType::Double))
            }
        }
    }

    #[expect(clippy::unnecessary_wraps)]
    fn convert_to(
        &mut self,
        input: (IRValue, CType),
        ctype: &CType,
    ) -> Result<IRValue, IRGenerationErrorType> {
        if input.1 == *ctype {
            return Ok(input.0);
        }

        if matches!(ctype, CType::Void) {
            // NOTE: this is a garbage value
            return Ok(input.0);
        }

        let out = self.generate_pseudo(ctype.sizeof(&self.scope));
        let input = self.attempt_array_decay(input);

        // TODO: check cast is valid
        self.push(IROp::Cast(
            IRType::from_ctype(ctype, &self.scope),
            (input.0, IRType::from_ctype(&input.1, &self.scope)),
            out.clone(),
        ));
        Ok(out)
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

fn float_constant_to_value(float: &Float) -> Result<IRValue, IRGenerationErrorType> {
    assert!(float.suffix.format == FloatFormat::Double);
    if float.suffix.imaginary {
        return Err(IRGenerationErrorType::TODOImaginary);
    }

    Ok(match float.base {
        FloatBase::Decimal => IRValue::float(
            float
                .number
                .parse()
                .expect("lang c should only provide valid doubles here"),
        ),
        FloatBase::Hexadecimal => todo!("hex floats"),
    })
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
