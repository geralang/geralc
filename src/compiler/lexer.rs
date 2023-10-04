
use crate::compiler::tokens::{TokenType, Token};
use crate::util::{
    strings::{StringMap, StringIdx},
    source::SourceRange
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

    pub fn next_token(&mut self, string_map: &mut StringMap) -> Option<Token> {
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
                    return Some(Token {
                        token_type: TokenType::Newline,
                        token_content: string_map.insert(c.encode_utf8(&mut [0; 4])),
                        source: SourceRange::new(self.file_name, self.file_content, p, p + 1)
                    })
                },
                '|' => if !self.has_next() || self.peek().is_whitespace() { self.next(); return Some(self.make_token("|", TokenType::Pipe, string_map)) },
                '=' => if !self.has_next() || self.peek().is_whitespace() { self.next(); return Some(self.make_token("=", TokenType::Equals, string_map)) },
                '#' => { self.next(); return Some(self.make_token("#", TokenType::Hashtag, string_map)) },
                ',' => { self.next(); return Some(self.make_token(",", TokenType::Comma, string_map)) },
                '(' => { self.next(); return Some(self.make_token("(", TokenType::ParenOpen, string_map)) },
                ')' => { self.next(); return Some(self.make_token(")", TokenType::ParenClose, string_map)) },
                '[' => { self.next(); return Some(self.make_token("[", TokenType::BracketOpen, string_map)) },
                ']' => { self.next(); return Some(self.make_token("]", TokenType::BracketClose, string_map)) },
                '{' => { self.next(); return Some(self.make_token("{", TokenType::BraceOpen, string_map)) },
                '}' => { self.next(); return Some(self.make_token("}", TokenType::BraceClose, string_map)) },
                '-' => if self.has_next() && self.peek() == '>' {
                    self.next();
                    self.next();
                    return Some(self.make_token("->", TokenType::Arrow, string_map))
                },
                '/' => if self.has_next() && self.peek() == '/' {
                    while self.has() {
                        match self.current() {
                            '\n' | '\r' => break,
                            _ => self.next()
                        }
                    }
                    continue;     
                },
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
                    return Some(Token {
                        token_type: TokenType::String,
                        token_content: string_map.insert(&content),
                        source: SourceRange::new(self.file_name, self.file_content, start, self.position)
                    });
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
            }
            let mut identifier = String::new();
            let start = self.position;
            let mut is_integer = true;
            let mut is_fraction = true;
            while self.has() && !self.current().is_whitespace() {
                match self.current() {
                    '#' | ',' |
                    '(' | ')' |
                    '[' | ']' |
                    '{' | '}' => break,
                    '-' => if self.has_next() && self.peek() == '>' { break },
                    '/' => if self.has_next() && self.peek() == '/' { break },
                    _ => {}
                }
                if self.current() < '0' || '9' < self.current() {
                    if self.current() != '.' || !is_integer { is_fraction = false; }
                    is_integer = false;
                }
                identifier.push(self.current());
                self.next();
            }
            match &identifier[..] {
                "proc" => return Some(self.make_token("proc", TokenType::KeywordProcedure, string_map)),
                "func" => return Some(self.make_token("func", TokenType::KeywordFunction, string_map)),
                "case" => return Some(self.make_token("case", TokenType::KeywordCase, string_map)),
                "var" => return Some(self.make_token("var", TokenType::KeywordVariable, string_map)),
                "mut" => return Some(self.make_token("mut", TokenType::KeywordMutable, string_map)),
                "return" => return Some(self.make_token("return", TokenType::KeywordReturn, string_map)),
                _ => return Some(Token {
                    token_type: if is_integer { TokenType::Integer }
                        else if is_fraction { TokenType::Fraction }
                        else { TokenType::Identifier },
                    token_content: string_map.insert(&identifier),
                    source: SourceRange::new(self.file_name, self.file_content, start, self.position)
                }),
            }
        }
    }
}