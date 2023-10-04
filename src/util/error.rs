use crate::util::{source::SourceRange, strings::StringMap};


pub enum ErrorType {

    // parser errors

    UnexpectedEnd(&'static str),
    UnexpectedToken(&'static str, String),
    UnclosedGroupingSymbol(&'static str, &'static str)

}

impl ErrorType {
    pub fn display(&self) -> String {
        match self {
            ErrorType::UnexpectedEnd(expected) => format!("Expected {}, but reached the end of the expression", expected),
            ErrorType::UnexpectedToken(expected, got) => format!("Expected {}, but got '{}' instead", expected, got),
            ErrorType::UnclosedGroupingSymbol(opened, closed) => format!("'{}' was not closed with the corresponding '{}'", opened, closed)
        }
    }
}


pub enum ErrorSection {
    Error(ErrorType),
    Info(&'static str),
    Help(&'static str),
    Code(SourceRange)
}

impl ErrorSection {
    pub fn display(&self, strings: &StringMap) -> String {
        match self {
            ErrorSection::Error(error_type) => format!("[error] {}", error_type.display()),
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
                let displayed_lines_start = source_start_line.max(1) - 1;
                let displayed_lines_end = source_end_line + 1;
                output.push_str(&format!("[{}:{}]\n", strings.get(source.file_name()), source_start_line));
                {
                    let mut position = 0usize;
                    let mut line = 1usize;
                    let mut last_c = '\0';
                    let mut line_content = Vec::new();
                    for c in &source_file {
                        line_content.push(*c);
                        if (*c == '\n' && last_c != '\r') || *c == '\r' {
                            if line >= displayed_lines_start && line <= displayed_lines_end {
                                output.push_str(&format!(" {: >1$} | ", line, displayed_lines_end.to_string().len()));
                                for l in &line_content { output.push(*l); }
                                let mut marked = vec![' '; line_content.len() - 1];
                                let mut is_marked = false;
                                for i in source.start_position()..source.end_position() {
                                    if i < (position - line_content.len() + 1) { continue; }
                                    if i >= position { continue; }
                                    marked.insert(i - (position - line_content.len() + 1), '^');
                                    is_marked = true;
                                }
                                if is_marked {
                                    output.push_str(&format!(" {} | ", " ".repeat(displayed_lines_end.to_string().len())));
                                    for m in marked { output.push(m); }
                                    output.push('\n');
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