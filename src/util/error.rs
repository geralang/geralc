
use crate::util::{source::SourceRange, strings::{StringMap, StringIdx}};


pub enum ErrorType {

    // cli errors
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
    SymbolAlreadyExists(String)

}

impl ErrorType {
    pub fn display(&self, strings: &StringMap) -> String {
        match self {
            ErrorType::FileSystemError(error) => format!("An error occured while interacting with the file system: '{}'", error),
            ErrorType::InvalidCharacter(got) => format!("Encountered '{}', which is an invalid character", got),
            ErrorType::MissingLeftExpr(expected) => format!("Expected {} on the left, but got nothing instead", expected),
            ErrorType::UnexpectedEnd(expected) => format!("Expected {}, but reached the end of the expression", expected),
            ErrorType::UnexpectedToken(expected, got) => format!("Expected {}, but got '{}' instead", expected, strings.get(*got)),
            ErrorType::MayNotBePublic => format!("This syntax may not be marked as public"),
            ErrorType::TotallyUnexpectedToken(got) => format!("'{}' is totally unexpected here", strings.get(*got)),
            ErrorType::InvalidContext(thing, expected, got) => format!("{} may only be used as {}, but here one was used as {} instead", thing, expected, got),
            ErrorType::ModuleDeclarationNotAtTop => format!("The parent module must be declared at the top of the file"),
            ErrorType::SymbolAlreadyExists(path) => format!("'{}' already exists", path)
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
            ErrorSection::Error(error_type) => format!("[error] {}", error_type.display(strings)),
            ErrorSection::Info(message) => format!("[info] {}", message),
            ErrorSection::Help(message) => format!("[help] {}", message),
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
                let displayed_lines_start = source_start_line.max(2) - 2;
                let displayed_lines_end = source_end_line + 2;
                output.push_str(&format!("[{}:{}]", strings.get(source.file_name()), source_start_line));
                {
                    let mut position = 0usize;
                    let mut line = 1usize;
                    let mut last_c = '\0';
                    let mut line_content = Vec::new();
                    for c in &source_file {
                        line_content.push(*c);
                        if (*c == '\n' && last_c != '\r') || *c == '\r' {
                            if line >= displayed_lines_start && line <= displayed_lines_end {
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
                                    for m in marked { output.push(m); }
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