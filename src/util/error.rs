
use crate::util::{source::SourceRange, strings::{StringMap, StringIdx}};


macro_rules! style_reset { () => { "\x1b[0m" } }
macro_rules! style_dark_red { () => { "\x1b[0;31m" } }
macro_rules! style_bold_dark_red { () => { "\x1b[1;31m" } }
macro_rules! style_red { () => { "\x1b[0;91m" } }
macro_rules! style_bold_green { () => { "\x1b[1;92m" } }
macro_rules! style_bold_cyan { () => { "\x1b[1;96m" } }
macro_rules! style_gray { () => { "\x1b[0;90m" } }


pub enum ErrorType {

    // cli errors
    ArgumentDoesNotExist(String),
    InvalidArgumentCount(String, usize, usize),
    MissingArgument(&'static str),
    FileSystemError(String),
    InvalidFileExtension(String),

    // lexer errors
    InvalidCharacter(char),

    // external mappings parser errors
    TypeDoesNotExist(StringIdx),
    NoDefinedModule(&'static str, StringIdx),

    // parser errors
    MissingLeftExpr(&'static str),
    UnexpectedEnd(&'static str),
    UnexpectedToken(&'static str, StringIdx),
    TotallyUnexpectedToken(StringIdx),
    MayNotBePublic,

    // grammar checking errors
    InvalidContext(&'static str, &'static str, &'static str),

    // module errors
    ModuleDeclarationNotAtTop,
    SymbolAlreadyExists(String),
    ModuleDoesNotExist(String),
    SymbolDoesNotExist(String),
    SymbolIsNotPublic(String),
    ModuleAlreadyDefined(String),

    // type errors
    NoPossibleTypes(String, String),
    VariableDoesNotExist(StringIdx),
    ImmutableAssignmant(StringIdx),
    RecursiveConstant(String),
    InvalidParameterCount(String, usize, usize),
    DoesNotAlwaysReturn(&'static str),
    VariableWithoutValue(StringIdx),
    
    // interpreter errors
    ArrayIndexOutOfBounds(usize, i64),
    ConstantDependsOnExternal(String),
    
    // ir lowering errors
    InvalidMainProcedure(String)

}

impl ErrorType {
    pub fn display(&self, strings: &StringMap) -> String {
        match self {
            ErrorType::ArgumentDoesNotExist(got) => format!(
                concat!(style_red!(), "{}", style_dark_red!(), " is not a valid argument"),
                got
            ),
            ErrorType::InvalidArgumentCount(argument, expected, got) => format!(
                concat!(style_red!(), "{}", style_dark_red!(), " expects {} argument{}, got {} instead"),
                argument,
                expected,
                if *expected == 1 { "" } else { "s" },
                got
            ),
            ErrorType::MissingArgument(name) => format!(
                concat!("The argument ", style_red!(), "{}", style_dark_red!(), " is required, but was not provided"),
                name
            ),
            ErrorType::FileSystemError(error) => format!(
                concat!("An error occured while interacting with the file system: ", style_red!(), "{}", style_dark_red!()),
                error
            ),
            ErrorType::InvalidFileExtension(file_path) => format!(
                concat!("The file ", style_red!(), "{}", style_dark_red!(), " has an extension not recognized by the compiler"),
                file_path
            ),

            ErrorType::InvalidCharacter(got) => format!(
                concat!("Encountered ", style_red!(), "{}", style_dark_red!(), ", which is an invalid character"),
                got
            ),

            ErrorType::TypeDoesNotExist(name) => format!(
                concat!("No type with the name ", style_red!(), "{}", style_dark_red!(), " has been defined up to this point"),
                strings.get(*name)
            ),
            ErrorType::NoDefinedModule(thing, name) => format!(
                concat!("The {} ", style_red!(), "{}", style_dark_red!(), " is not defined to be in any module"),
                thing,
                strings.get(*name)
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
                concat!("Expected {}, but got ", style_red!(), "{}", style_dark_red!(), " instead"),
                expected,
                strings.get(*got)
            ),
            ErrorType::TotallyUnexpectedToken(got) => format!(
                concat!(style_red!(), "{}", style_dark_red!(), " is totally unexpected here"),
                strings.get(*got)
            ),
            ErrorType::MayNotBePublic => format!(
                "This syntax may not be marked as public"
            ),

            ErrorType::InvalidContext(thing, expected, got) => format!(
                "{} may only be used as {}, but here one was used as {} instead",
                thing,
                expected,
                got
            ),

            ErrorType::ModuleDeclarationNotAtTop => format!(
                "The parent module must be declared at the top of the file"
            ),
            ErrorType::SymbolAlreadyExists(path) => format!(
                concat!("the symbol ", style_red!(), "{}", style_dark_red!(), " already exists"),
                path
            ),
            ErrorType::ModuleDoesNotExist(path) => format!(
                concat!(style_red!(), "{}", style_dark_red!(), " is not a known module"),
                path
            ),
            ErrorType::SymbolDoesNotExist(path) => format!(
                concat!(style_red!(), "{}", style_dark_red!(), " is not a known symbol"),
                path
            ),
            ErrorType::SymbolIsNotPublic(path) => format!(
                concat!(style_red!(), "{}", style_dark_red!(), " exists but is not public; therefore, this module may not access it"),
                path
            ),
            ErrorType::ModuleAlreadyDefined(path) => format!(
                concat!("The module ", style_red!(), "{}", style_dark_red!(), " is already defined in another file"),
                path
            ),

            ErrorType::NoPossibleTypes(limited, to_only) => format!(
                concat!("The expression is expected to have incompatible types:\n1) ", style_red!(), "{}", style_dark_red!(), "\n2) ", style_red!(), "{}", style_dark_red!()),
                limited,
                to_only
            ),
            ErrorType::VariableDoesNotExist(name) => format!(
                concat!("There is no variable called ", style_red!(), "{}", style_dark_red!(), " in the current scope"),
                strings.get(*name)
            ),
            ErrorType::ImmutableAssignmant(name) => format!(
                concat!("The variable ", style_red!(), "{}", style_dark_red!(), " was not as declared as mutable, but a new value is assigned"),
                strings.get(*name)
            ),
            ErrorType::RecursiveConstant(name) => format!(
                concat!("The global constant ", style_red!(), "{}", style_dark_red!(), " is recursive"),
                name
            ),
            ErrorType::InvalidParameterCount(argument, expected, got) => format!(
                concat!(style_red!(), "{}", style_dark_red!(), " expects {} parameter{}, got {} instead"),
                argument,
                expected,
                if *expected == 1 { "" } else { "s" },
                got
            ),
            ErrorType::DoesNotAlwaysReturn(thing) => format!(
                "{} only sometimes returns a value",
                thing
            ),
            ErrorType::VariableWithoutValue(name) => format!(
                concat!("There is a variable called ", style_red!(), "{}", style_dark_red!(), " in the current scope, but it is not guaranteed to have a value at this point"),
                strings.get(*name)
            ),

            ErrorType::ArrayIndexOutOfBounds(length, accessed) => format!(
                concat!("The index ", style_red!(), "{}", style_dark_red!(), " is out of bounds for an array of length ", style_red!(), "{}", style_dark_red!()),
                accessed,
                length
            ),
            ErrorType::ConstantDependsOnExternal(path) => format!(
                concat!("The symbol ", style_red!(), "{}", style_dark_red!(), " is implemented externally, meaning it may not be used in constant values"),
                path
            ),

            ErrorType::InvalidMainProcedure(path) => format!(
                concat!(style_red!(), "{}", style_dark_red!(), " is not the name of a valid main procedure"),
                path
            )
        }
    }
}


pub enum ErrorSection {
    Error(ErrorType),
    Info(String),
    Help(String),
    Code(SourceRange)
}

impl ErrorSection {
    pub fn display(&self, strings: &StringMap) -> String {
        match self {
            ErrorSection::Error(error_type) => format!(concat!(style_bold_dark_red!(), "error: ", style_dark_red!(), "{}", style_reset!()), error_type.display(strings)),
            ErrorSection::Info(message) => format!(concat!(style_bold_cyan!(), "info: ", style_reset!(), "{}", style_reset!()), message),
            ErrorSection::Help(message) => format!(concat!(style_bold_green!(), "help: ", style_reset!(), "{}", style_reset!()), message),
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
                                    if !marked.contains(&true) { output.push_str(style_gray!()); }
                                    output.push_str(&format!(" {: >1$}  ", line, displayed_lines_end.to_string().len()));
                                    let mut last_marked: bool = false;
                                    for c in 0..line_content.len() - 1 {
                                        if marked[c] && !last_marked {
                                            output.push_str(style_dark_red!());
                                            last_marked = true;
                                        } else if !marked[c] && last_marked {
                                            output.push_str(style_reset!());
                                            last_marked = false;
                                        }
                                        output.push(line_content[c]);
                                    }
                                    output.push_str(style_reset!());
                                } else if !printed_filler {
                                    output.push('\n');
                                    output.push_str(&format!(" {}  ", " ".repeat(displayed_lines_end.to_string().len())));
                                    output.push_str(style_gray!());
                                    output.push_str(&format!("... ({} lines hidden)", source_end_line - source_start_line - 3 /* don't ask where '3' comes from, it just works */));
                                    output.push_str(style_reset!());
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
            }
        }
    }
}


pub struct Error {
    sections: Box<[ErrorSection]>
}

impl Error {
    pub fn new(sections: Box<[ErrorSection]>) -> Error {
        Error {
            sections
        }
    }

    pub fn display(&self, strings: &StringMap) -> String {
        let mut output = String::new();
        for section in &*self.sections {
            output.push_str(&section.display(strings));
            output.push('\n');
        }
        output
    }
}