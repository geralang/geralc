
use crate::compiler::tokens::{TokenType, Token};
use crate::util::{
    strings::{StringMap, StringIdx},
    source::SourceRange,
    error::{Error, ErrorSection, ErrorType}
};

pub struct Lexer {
    file_name: StringIdx,
    file_content: StringIdx,
    source_chars: Vec<char>,
    position: usize
}

impl Lexer {
    pub fn new(file_name: StringIdx, file_content: StringIdx, string_map: &mut StringMap) -> Lexer {
        Lexer {
            file_name,
            file_content,
            source_chars: string_map.get(file_content).chars().collect(),
            position: 0
        }
    }

    fn has(&self) -> bool { self.position < self.source_chars.len() }
    fn current(&self) -> char { self.source_chars[self.position] }
    fn has_next(&self) -> bool { self.position + 1 < self.source_chars.len() }
    fn peek(&self) -> char { self.source_chars[self.position + 1] }
    fn next(&mut self) { self.position += 1; }

    fn make_token(&self, content: &str, token_type: TokenType, string_map: &mut StringMap) -> Token {
        Token {
            token_type,
            token_content: string_map.insert(content),
            source: SourceRange::new(self.file_name, self.file_content, self.position - content.len(), self.position)
        }
    }

    pub fn next_token(&mut self, string_map: &mut StringMap) -> Option<Result<Token, Error>> {
        loop {
            if !self.has() { return None; }
            match self.current() {
                '\n' | '\r' => {
                    let c = self.current();
                    let p = self.position;
                    self.next();
                    while self.has() && self.current().is_whitespace() && self.current() != '\n' && self.current() != '\r' {
                        self.next();
                    }
                    if self.has() && self.current() == '|' { continue }
                    return Some(Ok(Token {
                        token_type: TokenType::Newline,
                        token_content: string_map.insert(c.encode_utf8(&mut [0; 4])),
                        source: SourceRange::new(self.file_name, self.file_content, p, p + 1)
                    }))
                }
                '|' => {
                    self.next();
                    return Some(Ok(if self.has() && self.current() == '|' {
                        self.next();
                        self.make_token("||", TokenType::DoublePipe, string_map)
                    } else {
                        self.make_token("|", TokenType::Pipe, string_map)
                    }))
                }
                '=' => {
                    self.next();
                    return Some(Ok(if self.has() && self.current() == '=' {
                        self.next();
                        self.make_token("==", TokenType::DoubleEquals, string_map)
                    } else {
                        self.make_token("=", TokenType::Equals, string_map)
                    }))
                }
                '#' => { self.next(); return Some(Ok(self.make_token("#", TokenType::Hashtag, string_map))) },
                '+' => { self.next(); return Some(Ok(self.make_token("+", TokenType::Plus, string_map))) },
                '-' => {
                    self.next();
                    return Some(Ok(if self.has() && self.current() == '>' {
                        self.next();
                        self.make_token("->", TokenType::Arrow, string_map)
                    } else {
                        self.make_token("-", TokenType::Minus, string_map)
                    }))
                },
                '*' => { self.next(); return Some(Ok(self.make_token("*", TokenType::Asterisk, string_map))) },
                '/' => {
                    self.next();
                    if self.has() && self.current() == '/' {
                        while self.has() {
                            match self.current() {
                                '\n' | '\r' => break,
                                _ => self.next()
                            }
                        }
                        continue;
                    } else {
                        return Some(Ok(self.make_token("/", TokenType::Slash, string_map)))
                    }
                },
                '%' => { self.next(); return Some(Ok(self.make_token("%", TokenType::Percent, string_map))) },
                '<' => {
                    self.next();
                    return Some(Ok(if self.has() && self.current() == '=' {
                        self.next();
                        self.make_token("<=", TokenType::LessThanEqual, string_map)
                    } else {
                        self.make_token("<", TokenType::LessThan, string_map)
                    }))
                }
                '>' => {
                    self.next();
                    return Some(Ok(if self.has() && self.current() == '=' {
                        self.next();
                        self.make_token(">=", TokenType::GreaterThanEqual, string_map)
                    } else {
                        self.make_token(">", TokenType::GreaterThan, string_map)
                    }))
                }
                '!' => {
                    self.next();
                    return Some(Ok(if self.has() && self.current() == '=' {
                        self.next();
                        self.make_token("!=", TokenType::NotEquals, string_map)
                    } else {
                        self.make_token("!", TokenType::ExclamationMark, string_map)
                    }))
                }
                '&' => if self.has_next() && self.peek() == '&' {
                    self.next();
                    self.next();
                    return Some(Ok(self.make_token("&&", TokenType::DoubleAmpersand, string_map)))
                }
                ':' => if self.has_next() && self.peek() == ':' {
                    self.next();
                    self.next();
                    return Some(Ok(self.make_token("::", TokenType::NamespaceSeparator, string_map)))
                }
                ',' => { self.next(); return Some(Ok(self.make_token(",", TokenType::Comma, string_map))) }
                '(' => { self.next(); return Some(Ok(self.make_token("(", TokenType::ParenOpen, string_map))) }
                ')' => { self.next(); return Some(Ok(self.make_token(")", TokenType::ParenClose, string_map))) }
                '[' => { self.next(); return Some(Ok(self.make_token("[", TokenType::BracketOpen, string_map))) }
                ']' => { self.next(); return Some(Ok(self.make_token("]", TokenType::BracketClose, string_map))) }
                '{' => { self.next(); return Some(Ok(self.make_token("{", TokenType::BraceOpen, string_map))) }
                '}' => { self.next(); return Some(Ok(self.make_token("}", TokenType::BraceClose, string_map))) }
                '"' => {
                    let start = self.position;
                    self.next();
                    let mut content = String::new();
                    let mut escaped = false;
                    while self.has() && (escaped || self.current() != '"') {
                        let is_backslash = self.current() == '\\';
                        if escaped {
                            match self.current() {
                                '\n' => {}
                                '\\' => content.push('\\'),
                                'n' => content.push('\n'),
                                'r' => content.push('\r'),
                                other => content.push(other)
                            }
                        } else if !is_backslash {
                            content.push(self.current());
                        }
                        escaped = is_backslash;
                        self.next();
                    }
                    self.next();
                    return Some(Ok(Token {
                        token_type: TokenType::String,
                        token_content: string_map.insert(&content),
                        source: SourceRange::new(self.file_name, self.file_content, start, self.position)
                    }));
                }
                _ => {}
            }
            if self.current().is_whitespace() {
                while self.has() && self.current().is_whitespace() {
                    match self.current() {
                        '\n' | '\r' => break,
                        _ => self.next()
                    }
                }
                continue;
            } else if '0' <= self.current() && self.current() <= '9' {
                let mut literal = String::new();
                let start = self.position;
                let mut is_fraction = false;
                while self.has() {
                    if self.current() == '.' && !is_fraction {
                        is_fraction = true;
                    } else if '0' <= self.current() && self.current() <= '9' {
                    } else {
                        break;
                    }
                    literal.push(self.current());
                    self.next();
                }
                return Some(Ok(Token {
                    token_type: if is_fraction { TokenType::Fraction } else { TokenType::Integer },
                    token_content: string_map.insert(&literal),
                    source: SourceRange::new(self.file_name, self.file_content, start, self.position)
                }))

            } else if !self.current().is_ascii_alphanumeric() && self.current() != '_' {
                return Some(Err(Error::new([
                    ErrorSection::Error(ErrorType::InvalidCharacter(self.current())),
                    ErrorSection::Code(SourceRange::new(self.file_name, self.file_content, self.position, self.position + 1))
                ].into())))
            }
            let mut identifier = String::new();
            let start = self.position;
            while self.has() && (self.current().is_ascii_alphanumeric() || self.current() == '_') {
                identifier.push(self.current());
                self.next();
            }
            match &identifier[..] {
                "proc" => return Some(Ok(self.make_token("proc", TokenType::KeywordProcedure, string_map))),
                "func" => return Some(Ok(self.make_token("func", TokenType::KeywordFunction, string_map))),
                "case" => return Some(Ok(self.make_token("case", TokenType::KeywordCase, string_map))),
                "var" => return Some(Ok(self.make_token("var", TokenType::KeywordVariable, string_map))),
                "mut" => return Some(Ok(self.make_token("mut", TokenType::KeywordMutable, string_map))),
                "ret" => return Some(Ok(self.make_token("ret", TokenType::KeywordReturn, string_map))),
                "mod" => return Some(Ok(self.make_token("mod", TokenType::KeywordModule, string_map))),
                "pub" => return Some(Ok(self.make_token("pub", TokenType::KeywordPublic, string_map))),
                "use" => return Some(Ok(self.make_token("use", TokenType::KeywordUse, string_map))),
                "true" => return Some(Ok(self.make_token("true", TokenType::KeywordTrue, string_map))),
                "false" => return Some(Ok(self.make_token("false", TokenType::KeywordFalse, string_map))),
                _ => return Some(Ok(Token {
                    token_type: TokenType::Identifier,
                    token_content: string_map.insert(&identifier),
                    source: SourceRange::new(self.file_name, self.file_content, start, self.position)
                })),
            }
        }
    }
}