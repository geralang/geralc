
use crate::util::{strings::StringIdx, source::SourceRange};

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
    pub token_type: TokenType,
    pub token_content: StringIdx,
    pub source: SourceRange
}