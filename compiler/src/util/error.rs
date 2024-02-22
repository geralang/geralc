
use crate::util::{source::SourceRange, strings::{StringMap, StringIdx}};


#[macro_export]
macro_rules! style_reset { () => { "\x1b[0m" } }
#[macro_export]
macro_rules! style_dark_red { () => { "\x1b[0;31m" } }
#[macro_export]
macro_rules! style_bold_dark_red { () => { "\x1b[1;31m" } }
#[macro_export]
macro_rules! style_red { () => { "\x1b[0;91m" } }
#[macro_export]
macro_rules! style_bold_green { () => { "\x1b[1;92m" } }
#[macro_export]
macro_rules! style_bold_cyan { () => { "\x1b[1;96m" } }
#[macro_export]
macro_rules! style_cyan { () => { "\x1b[96m" } }
#[macro_export]
macro_rules! style_gray { () => { "\x1b[0;90m" } }


#[derive(Debug)]
pub enum ErrorType {

    // cli errors
    ArgumentDoesNotExist(String),
    InvalidArgumentCount(String, usize, usize),
    MissingArgument(&'static str),
    FileSystemError(String),
    InvalidFileExtension(String),

    // lexer errors
    InvalidCharacter(char),
    IntLiteralOverflows(StringIdx),
    FloatLiteralOverflows(StringIdx),

    // external mappings parser errors
    TypeDoesNotExist(StringIdx),
    NoDefinedModule(&'static str, StringIdx),

    // parser errors
    MissingLeftExpr(&'static str),
    UnexpectedEnd(&'static str),
    UnexpectedToken(&'static str, StringIdx),
    TotallyUnexpectedToken(StringIdx),
    MayNotBePublic,
    NotACall,

    // grammar checking errors
    InvalidContext(&'static str, &'static str, &'static str),
    DuplicateFunctionParameter(StringIdx),

    // module errors
    ModuleDeclarationNotAtTop,
    SymbolAlreadyExists(String),
    ModuleDoesNotExist(String),
    SymbolDoesNotExist(String),
    SymbolIsNotPublic(String),
    ModuleAlreadyDefined(String),

    // type errors
    NoPossibleTypes,
    VariableDoesNotExist(StringIdx),
    ImmutableAssignment(String),
    RecursiveConstant(String),
    InvalidParameterCount(String, usize, usize),
    VariableWithoutValue(StringIdx),
    
    // interpreter errors
    ConstExpressionPanics,
    ConstDependsOnExternal(String),
    
    // ir lowering errors
    NoMainProcedureDefined(String),
    InvalidMainProcedure(String),

    // code generation
    InvalidCompileTarget(String)

}

impl ErrorType {
    pub fn display(&self, strings: &StringMap, color: bool) -> String {
        match self {
            ErrorType::ArgumentDoesNotExist(got) => format!(
                "{}'{}'{} is not a valid command line argument",
                if color { style_red!() } else { "" },
                got,
                if color { style_dark_red!() } else { "" }
            ),
            ErrorType::InvalidArgumentCount(argument, expected, got) => format!(
                "The command line argument {}'{}'{} expects {} value{}, but got {} instead",
                if color { style_red!() } else { "" },
                argument,
                if color { style_dark_red!() } else { "" },
                expected,
                if *expected == 1 { "" } else { "s" },
                got
            ),
            ErrorType::MissingArgument(name) => format!(
                "The command line argument {}'{}'{} is required, but was not provided",
                if color { style_red!() } else { "" },
                name,
                if color { style_dark_red!() } else { "" }
            ),
            ErrorType::FileSystemError(error) => format!(
                "An error occured while interacting with the file system: {}'{}'{}",
                if color { style_red!() } else { "" },
                error,
                if color { style_dark_red!() } else { "" }
            ),
            ErrorType::InvalidFileExtension(file_path) => format!(
                "The file {}'{}'{} has an extension not recognized by the compiler",
                if color { style_red!() } else { "" },
                file_path,
                if color { style_dark_red!() } else { "" }
            ),

            ErrorType::InvalidCharacter(got) => format!(
                "Encountered {}'{}'{}, which is an invalid character",
                if color { style_red!() } else { "" },
                got,
                if color { style_dark_red!() } else { "" }
            ),
            ErrorType::IntLiteralOverflows(value) => format!(
                "The value {}'{}'{} does not fit into a single integer",
                if color { style_red!() } else { "" },
                strings.get(*value),
                if color { style_dark_red!() } else { "" }
            ),
            ErrorType::FloatLiteralOverflows(value) => format!(
                "The value {}'{}'{} does not fit into a single floating point number",
                if color { style_red!() } else { "" },
                strings.get(*value),
                if color { style_dark_red!() } else { "" }
            ),

            ErrorType::TypeDoesNotExist(name) => format!(
                "No type with the name {}'{}'{} has been defined up to this point",
                if color { style_red!() } else { "" },
                strings.get(*name),
                if color { style_dark_red!() } else { "" }
            ),
            ErrorType::NoDefinedModule(thing, name) => format!(
                "The {} {}'{}'{} is not defined to be in any module",
                thing,
                if color { style_red!() } else { "" },
                strings.get(*name),
                if color { style_dark_red!() } else { "" }
            ),

            ErrorType::MissingLeftExpr(expected) => format!(
                "Expected {} on the left, but got nothing instead",
                expected
            ),
            ErrorType::UnexpectedEnd(expected) => format!(
                "Expected {}, but reached the end of the expression",
                expected
            ),
            ErrorType::UnexpectedToken(expected, got) => format!(
                "Expected {}, but got {}'{}'{} instead",
                expected,
                if color { style_red!() } else { "" },
                strings.get(*got),
                if color { style_dark_red!() } else { "" }
            ),
            ErrorType::TotallyUnexpectedToken(got) => format!(
                "{} is totally unexpected here",
                strings.get(*got)
            ),
            ErrorType::MayNotBePublic => format!(
                "This syntax may not be marked as public"
            ),
            ErrorType::NotACall => format!(
                "Expected a call, but got something else instead"
            ),

            ErrorType::InvalidContext(thing, expected, got) => format!(
                "{} may only be used as {}, but here one was used as {} instead",
                thing,
                expected,
                got
            ),
            ErrorType::DuplicateFunctionParameter(name) => format!(
                "the call parameter {}'{}'{} exists more than once in the same argument list",
                if color { style_red!() } else { "" },
                strings.get(*name),
                if color { style_dark_red!() } else { "" }
            ),

            ErrorType::ModuleDeclarationNotAtTop => format!(
                "The parent module must be declared at the top of the file"
            ),
            ErrorType::SymbolAlreadyExists(path) => format!(
                concat!("the symbol {}'{}'{} already exists"),
                if color { style_red!() } else { "" },
                path,
                if color { style_dark_red!() } else { "" }
            ),
            ErrorType::ModuleDoesNotExist(path) => format!(
                "{}'{}'{} is not a known module",
                if color { style_red!() } else { "" },
                path,
                if color { style_dark_red!() } else { "" }
            ),
            ErrorType::SymbolDoesNotExist(path) => format!(
                "{}'{}'{} is is not a known symbol",
                if color { style_red!() } else { "" },
                path,
                if color { style_dark_red!() } else { "" }
            ),
            ErrorType::SymbolIsNotPublic(path) => format!(
                "{}'{}'{} exists but is not public; therefore, this module may not access it",
                if color { style_red!() } else { "" },
                path,
                if color { style_dark_red!() } else { "" }
            ),
            ErrorType::ModuleAlreadyDefined(path) => format!(
                "The module {}'{}'{} is already defined in another file",
                if color { style_red!() } else { "" },
                path,
                if color { style_dark_red!() } else { "" }
            ),

            ErrorType::NoPossibleTypes => format!(
                "Incompatible types"
            ),
            ErrorType::VariableDoesNotExist(name) => format!(
                "There is no variable called {}'{}'{} in the current scope",
                if color { style_red!() } else { "" },
                strings.get(*name),
                if color { style_dark_red!() } else { "" }
            ),
            ErrorType::ImmutableAssignment(name) => format!(
                "The variable {}'{}'{} was not as declared as mutable, but a new value is assigned",
                if color { style_red!() } else { "" },
                name,
                if color { style_dark_red!() } else { "" }
            ),
            ErrorType::RecursiveConstant(name) => format!(
                "The global constant {}'{}'{} is recursive",
                if color { style_red!() } else { "" },
                name,
                if color { style_dark_red!() } else { "" }
            ),
            ErrorType::InvalidParameterCount(argument, expected, got) => format!(
                "{}'{}'{} expects {} parameter{}, got {} instead",
                if color { style_red!() } else { "" },
                argument,
                if color { style_dark_red!() } else { "" },
                expected,
                if *expected == 1 { "" } else { "s" },
                got
            ),
            ErrorType::VariableWithoutValue(name) => format!(
                "There is a variable called {}'{}'{} in the current scope, but it is not guaranteed to have a value at this point",
                if color { style_red!() } else { "" },
                strings.get(*name),
                if color { style_dark_red!() } else { "" }
            ),

            ErrorType::ConstExpressionPanics => format!(
                "A panic occured while evaluating a constant expression:"
            ),
            ErrorType::ConstDependsOnExternal(path) => format!(
                "The symbol {}'{}'{} is implemented externally, meaning it may not be used in constant values",
                if color { style_red!() } else { "" },
                path,
                if color { style_dark_red!() } else { "" }
            ),

            ErrorType::NoMainProcedureDefined(target) => format!(
                "The target format {}'{}'{} requires a main procedure to be defined",
                if color { style_red!() } else { "" },
                target,
                if color { style_dark_red!() } else { "" }
            ),
            ErrorType::InvalidMainProcedure(path) => format!(
                "{}'{}'{} is not the name of a valid main procedure",
                if color { style_red!() } else { "" },
                path,
                if color { style_dark_red!() } else { "" }
            ),

            ErrorType::InvalidCompileTarget(target) => format!(
                "{}'{}'{} is not a valid compilation target",
                if color { style_red!() } else { "" },
                target,
                if color { style_dark_red!() } else { "" }
            )
        }
    }
}


#[derive(Debug)]
pub enum ErrorSection {
    Error(ErrorType),
    Info(String),
    Help(String),
    Code(SourceRange),
    Raw(String)
}

impl ErrorSection {
    pub fn display(&self, strings: &StringMap, color: bool) -> String {
        match self {
            ErrorSection::Error(error_type) => format!(
                "{}error: {}{}{}",
                if color { style_bold_dark_red!() } else { "" },
                if color { style_dark_red!() } else { "" },
                error_type.display(strings, color),
                if color { style_reset!() } else { "" }
            ),
            ErrorSection::Info(message) => format!(
                "{}info: {}{}{}",
                if color { style_bold_cyan!() } else { "" },
                if color { style_reset!() } else { "" },
                message,
                if color { style_reset!() } else { "" }
            ),
            ErrorSection::Help(message) => format!(
                "{}help: {}{}{}",
                if color { style_bold_green!() } else { "" },
                if color { style_reset!() } else { "" },
                message,
                if color { style_reset!() } else { "" }
            ),
            ErrorSection::Code(source) => {
                let mut output = String::new();
                let mut source_file: Vec<char> = strings.get(source.file_content()).chars().collect::<Vec<char>>();
                source_file.push('\n');
                let mut source_start_line = 1usize;
                let mut source_end_line = 1usize;
                {
                    let mut position = 0usize;
                    let mut last_c = '\0';
                    for c in &source_file {
                        if (*c == '\n' && last_c != '\r') || *c == '\r' {
                            if position < source.start_position() { source_start_line += 1; }
                            if position < source.end_position() { source_end_line += 1; }
                        }
                        position += 1;
                        last_c = *c;
                    }
                }
                let displayed_lines_start = source_start_line.max(1) - 1;
                let displayed_lines_end = source_end_line + 2;
                output.push_str(&format!("at: {}:{}",
                    strings.get(source.file_name()),
                    displayed_lines_start + 1
                ));
                {
                    let mut position = 0usize;
                    let mut line = 1usize;
                    let mut last_c = '\0';
                    let mut line_content = Vec::new();
                    let mut printed_filler = false;
                    for c in &source_file {
                        line_content.push(*c);
                        if (*c == '\n' && last_c != '\r') || *c == '\r' {
                            if line >= displayed_lines_start && line <= displayed_lines_end {
                                if (line.max(source_start_line) - source_start_line).min(source_end_line.max(line) - line) < 2 {
                                    let mut marked = vec![false; line_content.len() - 1];
                                    for i in source.start_position()..source.end_position() {
                                        if i < (position.max(1) - line_content.len() + 1) { continue; }
                                        if i >= position { continue; }
                                        marked.insert(i - (position - line_content.len() + 1), true);
                                    }
                                    output.push('\n');
                                    if !marked.contains(&true) {
                                        if color {
                                            output.push_str(style_gray!());
                                        }
                                    }
                                    output.push_str(&format!(" {: >1$}  ", line, displayed_lines_end.to_string().len()));
                                    let mut last_marked: bool = false;
                                    for c in 0..line_content.len() - 1 {
                                        if marked[c] && !last_marked {
                                            if color {
                                                output.push_str(style_dark_red!());
                                            }
                                            last_marked = true;
                                        } else if !marked[c] && last_marked {
                                            if color {
                                                output.push_str(style_reset!());
                                            }
                                            last_marked = false;
                                        }
                                        output.push(line_content[c]);
                                    }
                                    if color {
                                        output.push_str(style_reset!());
                                    } else if marked.contains(&true) {
                                        output.push('\n');
                                        output.push_str(&" ".repeat(displayed_lines_end.to_string().len() + 3));
                                        for m in marked {
                                            output.push(if m { '^' } else { ' ' });
                                        }
                                    }
                                } else if !printed_filler {
                                    output.push('\n');
                                    output.push_str(&format!(" {}  ", " ".repeat(displayed_lines_end.to_string().len())));
                                    if color {
                                        output.push_str(style_gray!());
                                    }
                                    output.push_str(&format!("... ({} lines hidden)", source_end_line - source_start_line - 3 /* don't ask where '3' comes from, it just works */));
                                    if color {
                                        output.push_str(style_reset!());
                                    }
                                    printed_filler = true;
                                }
                            }
                            line_content.clear();
                            line += 1;
                        }
                        position += 1;
                        last_c = *c;
                    }
                }
                output
            },
            ErrorSection::Raw(message) => message.clone()
        }
    }
}


#[derive(Debug)]
pub struct Error {
    sections: Box<[ErrorSection]>
}

impl Error {
    pub fn new(sections: Box<[ErrorSection]>) -> Error {
        Error {
            sections
        }
    }

    pub fn display(&self, strings: &StringMap, color: bool) -> String {
        let mut output = String::new();
        for section in &*self.sections {
            output.push_str(&section.display(strings, color));
            output.push('\n');
        }
        output
    }
}