
use crate::util::{source::SourceRange, strings::{StringMap, StringIdx}};


macro_rules! style_reset { () => { "\x1b[0m" } }
macro_rules! style_dark_red { () => { "\x1b[0;31m" } }
macro_rules! style_bold_dark_red { () => { "\x1b[1;31m" } }
macro_rules! style_red { () => { "\x1b[0;91m" } }
macro_rules! style_bold_green { () => { "\x1b[1;92m" } }
macro_rules! style_bold_cyan { () => { "\x1b[1;96m" } }


pub enum ErrorType {

    // cli errors
    ArgumentDoesNotExist(String),
    InvalidArgumentCount(String, usize, usize),
    FileSystemError(String),

    // lexer errors
    InvalidCharacter(char),

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
    NoPossibleTypes(String, String)

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
            ErrorType::FileSystemError(error) => format!(
                concat!("An error occured while interacting with the file system: ", style_red!(), "{}", style_dark_red!()),
                error
            ),
            ErrorType::InvalidCharacter(got) => format!(
                concat!("Encountered ", style_red!(), "{}", style_dark_red!(), ", which is an invalid character"),
                got
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
            ErrorType::MayNotBePublic => format!(
                "This syntax may not be marked as public"
            ),
            ErrorType::TotallyUnexpectedToken(got) => format!(
                concat!(style_red!(), "{}", style_dark_red!(), " is totally unexpected here"),
                strings.get(*got)
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
                concat!("Limiting ", style_red!(), "{}", style_dark_red!(), " to only ", style_red!(), "{}", style_dark_red!(), " results in no remaining possible types!"),
                limited,
                to_only
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
                output.push_str(&format!("at: {}:{}", strings.get(source.file_name()), source_start_line));
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
                                    output.push('\n');
                                    output.push_str(&format!(" {: >1$} | ", line, displayed_lines_end.to_string().len()));
                                    for l in &line_content[0..line_content.len() - 1] { output.push(*l); }
                                    let mut marked = vec![' '; line_content.len() - 1];
                                    let mut is_marked = false;
                                    for i in source.start_position()..source.end_position() {
                                        if i < (position.max(1) - line_content.len() + 1) { continue; }
                                        if i >= position { continue; }
                                        marked.insert(i - (position - line_content.len() + 1), '^');
                                        is_marked = true;
                                    }
                                    if is_marked {
                                        output.push('\n');
                                        output.push_str(&format!(" {} | ", " ".repeat(displayed_lines_end.to_string().len())));
                                        output.push_str(style_bold_dark_red!());
                                        for m in marked { output.push(m); }
                                        output.push_str(style_reset!());
                                    }
                                } else if !printed_filler {
                                    output.push('\n');
                                    output.push_str(&format!(" {} | ", " ".repeat(displayed_lines_end.to_string().len())));
                                    output.push_str(style_dark_red!());
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