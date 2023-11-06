
use crate::util::{strings::StringIdx, source::SourceRange};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TokenType {
    Identifier,
    Integer,
    Fraction,
    String,
    Pipe,
    Equals,
    Dot,
    DoubleDot,
    DoubleDotEquals,
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
    FunctionPipe,
    DoubleAmpersand,
    ExclamationMark,
    Hashtag,
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
    KeywordCase,
    KeywordVariable,
    KeywordMutable,
    KeywordReturn,
    KeywordModule,
    KeywordPublic,
    KeywordUse,
    KeywordTrue,
    KeywordFalse,
    KeywordElse,
    KeywordUnit
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub token_content: StringIdx,
    pub source: SourceRange
}