use codespan_preprocessed::PreprocessedFile;
use codespan_preprocessed::reporting::Diagnostic;
use codespan_reporting::term::{
    self,
    termcolor::{ColorChoice, StandardStream},
};
use lang_c::ast::PointerDeclarator;
use std::fmt::Display;

use thiserror::Error;

use crate::c_compiler::StorageDuration;

#[derive(Error, Debug)]
pub enum IRGenerationErrorType {
    #[error("Invalid coersion between {0} and {1}")]
    InvalidCoercion(Box<str>, Box<str>),
    #[error("Unknown type")]
    TODOUnknownType,
    #[error("Variable declared as void")]
    VariableDeclaredAsVoid,
    #[error("Variable has incomplete type")]
    VariableDeclaredIncomplete,
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

#[derive(Debug)]
pub enum CompilerError {
    IRGenerationError {
        err: IRGenerationErrorType,
        span: lang_c::span::Span,
        source: String,
    },
    ParseError {
        err: lang_c::driver::Error,
    },
}

impl CompilerError {
    pub fn print(&self) {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        let source = match self {
            Self::IRGenerationError { source, .. } => source,
            Self::ParseError {
                err: lang_c::driver::Error::SyntaxError(err),
                ..
            } => &err.source,
            Self::ParseError {
                err: lang_c::driver::Error::PreprocessorError(_),
                ..
            } => &String::new(),
        };

        let file = PreprocessedFile::new(source);
        let diag = self.report();

        term::emit_to_write_style(
            &mut writer.lock(),
            &config,
            &file,
            &diag.to_diagnostic(&file),
        )
        .expect("if io fails i cant do anything anyways");
    }

    fn report(&self) -> Diagnostic<&str> {
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
                        .with_primary_label(err.offset - i - 1..err.offset - i, "insert `;` here")
                        .with_secondary_label(err.offset..err.offset + 1, "Unexpected token")
                } else {
                    Diagnostic::error()
                        .with_message("Unexpected token")
                        .with_primary_label(err.offset..err.offset + 1, "")
                        .with_note(format!("Expected one of {:?}", err.expected))
                }
            }
            Self::ParseError {
                err: lang_c::driver::Error::PreprocessorError(err),
                ..
            } => Diagnostic::error()
                .with_message("Preprocessor (gcc) error")
                .with_note(err.to_string()),
            Self::IRGenerationError { err, span, .. } => Diagnostic::error()
                .with_message(err.to_string())
                .with_primary_label(span.start..span.end, ""),
        }
    }
}
