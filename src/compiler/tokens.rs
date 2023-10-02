
use crate::compiler::strings::StringIdx;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TokenType {
    Newline,
    Identifier,
    Integer,
    Fraction,
    String,
    Pipe,
    Equals,
    Hashtag,
    Comma,
    Arrow,
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
    BraceOpen,
    BraceClose,
    KeywordProcedure,
    KeywordFunction,
    KeywordCase,
    KeywordVariable,
    KeywordMutable,
    KeywordReturn
}

#[derive(Debug)]
pub struct Token {
    pub content: StringIdx,
    pub file: StringIdx,
    pub token_type: TokenType
}