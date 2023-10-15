
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
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    DoubleEquals,
    NotEquals,
    DoublePipe,
    DoubleAmpersand,
    ExclamationMark,
    Comma,
    Arrow,
    NamespaceSeparator,
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
    KeywordReturn,
    KeywordModule,
    KeywordPublic,
    KeywordUse,
    KeywordTrue,
    KeywordFalse,
    KeywordElse
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub token_content: StringIdx,
    pub source: SourceRange
}