
#[derive(Debug)]
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
pub struct Token<'t> {
    pub content: String,
    pub file: &'t str,
    pub token_type: TokenType
}