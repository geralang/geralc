
use crate::util::{
    strings::StringMap,
    error::{Error, ErrorSection, ErrorType}
};
use crate::compiler::{
    lexer::Lexer, 
    ast::{AstNode, AstNodeVariant, HasAstNodeVariant},
    tokens::{TokenType, Token}
};


pub struct Parser {
    current: Token,
    reached_end: bool,
    current_result: Result<Option<AstNode>, Error>
}

impl Parser {
    pub fn new(strings: &mut StringMap, lexer: &mut Lexer) -> Option<Parser> {
        lexer.next_token(strings)
            .map(|current| Parser {
                current,
                reached_end: false,
                current_result: Ok(None)
            })
    }

    fn next(&mut self, strings: &mut StringMap, lexer: &mut Lexer) -> bool {
        if let Some(token) = lexer.next_token(strings) {
            self.current = token;
            true
        } else {
            self.reached_end = true;
            false
        }
    }

    pub fn parse_block(&mut self, strings: &mut StringMap, lexer: &mut Lexer) -> Result<Vec<AstNode>, Error> {
        let mut nodes = Vec::new();
        while self.current.token_type == TokenType::Newline && self.next(strings, lexer) {}
        loop {
            match self.parse_expression(strings, lexer, &mut vec![&[TokenType::BraceClose, TokenType::Newline]]) {
                Ok(None) => break,
                Ok(Some(node)) => {
                    nodes.push(node);
                    match self.current.token_type {
                        TokenType::Newline => while self.next(strings, lexer) && self.current.token_type == TokenType::Newline {},
                        _ => break
                    }
                },
                Err(error) => return Err(error)
            }
        }
        Ok(nodes)
    }

    fn parse_expression_until(&mut self, strings: &mut StringMap, lexer: &mut Lexer, end_at_types: &mut Vec<&[TokenType]>, until: &'static [TokenType]) -> Result<Option<AstNode>, Error> {
        end_at_types.push(until);
        let result = self.parse_expression(strings, lexer, end_at_types);
        end_at_types.pop();
        return result;
    }

    fn parse_expression(&mut self, strings: &mut StringMap, lexer: &mut Lexer, end_at_types: &mut Vec<&[TokenType]>) -> Result<Option<AstNode>, Error> {
        self.reached_end = false;
        let mut previous: Option<AstNode> = None;
        macro_rules! enforce_previous {
            ($expected: expr) => {
                if let Some(previous) = previous { previous }
                else { return Err(Error::new([
                    ErrorSection::Error(ErrorType::MissingLeftExpr($expected)),
                    ErrorSection::Code(self.current.source.clone())
                ].into())); }
            };
        }
        macro_rules! enforce_next {
            ($expected: expr) => {
                if !self.next(strings, lexer) { return Err(Error::new([
                    ErrorSection::Error(ErrorType::UnexpectedEnd($expected)),
                    ErrorSection::Code(self.current.source.clone())
                ].into())); }
            };
        }
        macro_rules! enforce_current_type {
            ($types: expr, $expected: expr) => {
                if !$types.contains(&self.current.token_type) { return Err(Error::new([
                    ErrorSection::Error(ErrorType::UnexpectedToken($expected, self.current.token_content)),
                    ErrorSection::Code(self.current.source.clone())
                ].into())); }
            };
        }
        macro_rules! enforce_expression {
            ($until: expr, $expected: expr) => {
                match self.parse_expression_until(strings, lexer, end_at_types, $until) {
                    Ok(None) => return Err(Error::new([
                        ErrorSection::Error(ErrorType::UnexpectedEnd($expected)),
                        ErrorSection::Code(self.current.source.clone())
                    ].into())),
                    Ok(Some(node)) => node,
                    Err(error) => return Err(error)
                }
            };
        }
        macro_rules! enforce_not_reached_end {
            ($expected: expr) => {
                if self.reached_end { return Err(Error::new([
                    ErrorSection::Error(ErrorType::UnexpectedEnd($expected)),
                    ErrorSection::Code(self.current.source.clone())
                ].into())); }
            };
        }
        loop {
            for end_at in &*end_at_types {
                if end_at.contains(&self.current.token_type) { return Ok(previous) }
            }
            match self.current.token_type {
                TokenType::Hashtag => {
                    let accessed = enforce_previous!("the thing to access");
                    enforce_next!("the name or index of the part to access");
                    enforce_current_type!(&[TokenType::Identifier, TokenType::BracketOpen], "the name or brackets (for index) of the part to access");
                    let accessed_source = accessed.source().clone();
                    match self.current.token_type {
                        TokenType::Identifier => {
                            previous = Some(AstNode::new(
                                AstNodeVariant::ObjectAccess { object: Box::new(accessed), member: self.current.token_content },
                                (&accessed_source..&self.current.source).into()
                            ));
                        }
                        TokenType::BracketOpen => {
                            enforce_next!("the index to access");
                            let index = enforce_expression!(&[TokenType::BracketClose], "the index to access");
                            enforce_current_type!(&[TokenType::BracketClose], "the matching closing bracket");
                            previous = Some(AstNode::new(
                                AstNodeVariant::ArrayAccess { array: Box::new(accessed), index: Box::new(index) },
                                (&accessed_source..&self.current.source).into()
                            ));
                        }
                        _ => panic!("unreachable")
                    }
                    if !self.next(strings, lexer) { return Ok(previous); }
                }
                TokenType::Pipe => {
                    let piped = enforce_previous!("the thing to pipe");
                    let piped_source = piped.source().clone();
                    enforce_next!("the call to pipe into");
                    let into = enforce_expression!(&[TokenType::Pipe], "the call to pipe into");
                    let into_source = into.source().clone();
                    if let AstNodeVariant::Call { called, mut arguments } = into.node_variant().clone() {
                        arguments.insert(0, piped);
                        previous = Some(AstNode::new(
                            AstNodeVariant::Call { called, arguments },
                            (&piped_source..&into_source).into()
                        ));
                    } else {
                        previous = Some(AstNode::new(
                            AstNodeVariant::Call { called: Box::new(into), arguments: vec![piped] },
                            (&piped_source..&into_source).into()
                        ));
                    }
                    if self.reached_end { return Ok(previous); }
                }
                TokenType::Equals => {
                    let assigned_to = enforce_previous!("the thing to assign to");
                    let assigned_to_source = assigned_to.source().clone();
                    enforce_next!("the value to assign");
                    let assigned = enforce_expression!(&[], "the value to assign");
                    let assigned_source = assigned.source().clone();
                    previous = Some(AstNode::new(
                        AstNodeVariant::Assignment { variable: Box::new(assigned_to), value: Box::new(assigned) },
                        (&assigned_to_source..&assigned_source).into()
                    ));
                    if self.reached_end { return Ok(previous); }
                }
                _ => {
                    let new;
                    let mut fetch_next = true;
                    match self.current.token_type {
                        TokenType::Identifier => new = AstNode::new(
                            AstNodeVariant::VariableAccess { name: self.current.token_content },
                            self.current.source.clone()
                        ),
                        TokenType::String => new = AstNode::new(
                            AstNodeVariant::StringLiteral { value: self.current.token_content },
                            self.current.source.clone()
                        ),
                        TokenType::Integer => new = AstNode::new(
                            AstNodeVariant::IntegerLiteral { value: strings.get(self.current.token_content).parse().expect("Lexer done messed up lmfao") },
                            self.current.source.clone()
                        ),
                        TokenType::Fraction => new = AstNode::new(
                            AstNodeVariant::FractionLiteral { value: strings.get(self.current.token_content).parse().expect("Lexer done messed up lmfao") },
                            self.current.source.clone()
                        ),
                        TokenType::ParenOpen => {
                            enforce_next!("the contained expression");
                            new = enforce_expression!(&[TokenType::ParenClose], "the contained expression");
                            enforce_current_type!(&[TokenType::ParenClose], "a closing parenthesis (')')");
                        }
                        TokenType::BraceOpen => {
                            let mut values = Vec::new();
                            let source_start = self.current.source.clone();
                            enforce_next!("the name of an object member or a closing brace ('}')");
                            while self.current.token_type != TokenType::BraceClose {
                                enforce_current_type!(&[TokenType::Identifier], "the name of an object member");
                                let member = self.current.token_content;
                                enforce_next!("an equals sign ('=')");
                                enforce_current_type!(&[TokenType::Equals], "an equals sign ('=')");
                                enforce_next!("the member value");
                                let value = enforce_expression!(&[TokenType::Comma, TokenType::BraceClose], "the member value");
                                values.push((member, value));
                                enforce_current_type!(&[TokenType::Comma, TokenType::BraceClose], "a comma (',') or a closing brace ('}')");
                                if self.current.token_type == TokenType::Comma {
                                    enforce_next!("the name of an object member or a closing brace ('}')");
                                }
                            }
                            // enforce_current_type!(&[TokenType::BraceClose], "a closing brace ('}')");
                            new = AstNode::new(
                                if values.len() == 0 { AstNodeVariant::UnitLiteral } else { AstNodeVariant::Object { values } },
                                (&source_start..&self.current.source).into()
                            );
                        }
                        TokenType::BracketOpen => {
                            let mut values = Vec::new();
                            let source_start = self.current.source.clone();
                            enforce_next!("an array value or a closing brace (']')");
                            while self.current.token_type != TokenType::BracketClose {
                                values.push(enforce_expression!(&[TokenType::Comma, TokenType::BracketClose], "an array value"));
                                enforce_current_type!(&[TokenType::Comma, TokenType::BracketClose], "a comma (',') or a closing bracket (']')");
                                if self.current.token_type == TokenType::Comma {
                                    enforce_next!("an array value or a closing brace (']')");
                                }
                            }
                            // enforce_current_type!(&[TokenType::BracketClose], "a closing bracket (']')");
                            new = AstNode::new(
                                AstNodeVariant::Array { values },
                                (&source_start..&self.current.source).into()
                            );
                        }
                        TokenType::KeywordProcedure => {
                            let source_start = self.current.source.clone();
                            enforce_next!("the procedure's name");
                            enforce_current_type!(&[TokenType::Identifier], "the procedure's name");
                            let name = self.current.token_content;
                            let mut arguments = Vec::new();
                            enforce_next!("a procedure parameter's name");
                            enforce_current_type!(&[TokenType::Identifier], "a procedure parameter's name");
                            loop {
                                enforce_current_type!(&[TokenType::Identifier, TokenType::BraceOpen], "a procedure parameter's name or an opening brace ('{')");
                                match self.current.token_type {
                                    TokenType::Identifier => arguments.push(self.current.token_content),
                                    TokenType::BraceOpen => break,
                                    _ => panic!("unreachable")
                                }
                                enforce_next!("a procedure parameter's name or an opening brace ('{')");
                            }
                            enforce_next!("the procedure's body");
                            let body = match self.parse_block(strings, lexer) {
                                Ok(body) => body,
                                Err(error) => return Err(error)
                            };
                            enforce_not_reached_end!("a closing brace ('}')");
                            enforce_current_type!(&[TokenType::BraceClose], "a closing brace ('}')");
                            new = AstNode::new(
                                AstNodeVariant::Procedure { name, arguments, body },
                                (&source_start..&self.current.source).into()
                            )
                        }
                        TokenType::KeywordFunction => {
                            let source_start = self.current.source.clone();
                            let mut arguments = Vec::new();
                            enforce_next!("a function parameter's name");
                            enforce_current_type!(&[TokenType::Identifier], "a function parameter's name");
                            loop {
                                enforce_current_type!(&[TokenType::Identifier, TokenType::BraceOpen], "a function parameter's name or an opening brace ('{')");
                                match self.current.token_type {
                                    TokenType::Identifier => arguments.push(self.current.token_content),
                                    TokenType::BraceOpen => break,
                                    _ => panic!("unreachable")
                                }
                                enforce_next!("a function parameter's name or an opening brace ('{')");
                            }
                            enforce_next!("the function's body");
                            let body = match self.parse_block(strings, lexer) {
                                Ok(body) => body,
                                Err(error) => return Err(error)
                            };
                            enforce_not_reached_end!("a closing brace ('}')");
                            enforce_current_type!(&[TokenType::BraceClose], "a closing brace ('}')");
                            new = AstNode::new(
                                AstNodeVariant::Function { arguments, body },
                                (&source_start..&self.current.source).into()
                            )
                        }
                        TokenType::KeywordVariable |
                        TokenType::KeywordMutable => {
                            let source_start = self.current.source.clone();
                            let mutable = self.current.token_type == TokenType::KeywordMutable;
                            if mutable {
                                enforce_next!("'var'");
                                enforce_current_type!(&[TokenType::KeywordVariable], "'var'");
                            }
                            enforce_next!("the variable's name");
                            enforce_current_type!(&[TokenType::Identifier], "the variable's name");
                            let name = self.current.token_content;
                            enforce_next!("an equals sign ('=')");
                            enforce_current_type!(&[TokenType::Equals], "an equals sign ('=')");
                            enforce_next!("the variable's value");
                            let value = enforce_expression!(&[], "the variable's value");
                            let value_source = value.source().clone();
                            fetch_next = false;
                            new = AstNode::new(
                                AstNodeVariant::Variable { name, mutable, value: Box::new(value) },
                                (&source_start..&value_source).into()
                            )
                        }
                        TokenType::KeywordCase => {
                            let source_start = self.current.source.clone();
                            enforce_next!("the condition value");
                            let value = enforce_expression!(&[TokenType::Arrow, TokenType::BraceOpen], "the condition value");
                            enforce_not_reached_end!("an arrow ('->') (conditonal case) or an opening brace ('{') (branching case)");
                            enforce_current_type!(&[TokenType::Arrow, TokenType::BraceOpen], "an arrow ('->') (conditonal case) or an opening brace ('{') (branching case)");
                            match self.current.token_type {
                                TokenType::Arrow => {
                                    enforce_next!("the body of the conditional branch");
                                    let source_end;
                                    let body = if self.current.token_type == TokenType::BraceOpen {
                                        enforce_next!("the body of the conditional branch");
                                        let body = match self.parse_block(strings, lexer) {
                                            Ok(body) => body,
                                            Err(error) => return Err(error)
                                        };
                                        enforce_not_reached_end!("a closing brace ('}')");
                                        enforce_current_type!(&[TokenType::BraceClose], "a closing brace ('}')");
                                        source_end = self.current.source.clone();
                                        body
                                    } else {
                                        fetch_next = false;
                                        let body = enforce_expression!(&[], "the body of the conditional branch");
                                        source_end = body.source().clone();
                                        vec![body]
                                    };
                                    new = AstNode::new(
                                        AstNodeVariant::CaseConditon { condition: Box::new(value), body },
                                        (&source_start..&source_end).into()
                                    )
                                }
                                TokenType::BraceOpen => {
                                    let mut branches = Vec::new();
                                    enforce_next!("a branch value");
                                    while self.current.token_type == TokenType::Newline && self.next(strings, lexer) {}
                                    while self.current.token_type != TokenType::BraceClose {
                                        match self.parse_expression(strings, lexer, &mut vec![&[TokenType::Arrow]]) {
                                            Ok(None) => break,
                                            Ok(Some(value)) => {
                                                enforce_not_reached_end!("an arrow ('->')");
                                                enforce_current_type!(&[TokenType::Arrow], "an arrow ('->')");
                                                enforce_next!("the body of the branch");
                                                let body = if self.current.token_type == TokenType::BraceOpen {
                                                    enforce_next!("the body of the conditional branch");
                                                    let body = match self.parse_block(strings, lexer) {
                                                        Ok(body) => body,
                                                        Err(error) => return Err(error)
                                                    };
                                                    enforce_not_reached_end!("a closing brace ('}')");
                                                    enforce_current_type!(&[TokenType::BraceClose], "a closing brace ('}')");
                                                    body
                                                } else {
                                                    vec![enforce_expression!(&[TokenType::Newline, TokenType::BraceClose], "the body of the conditional branch")]
                                                };
                                                branches.push((value, body));
                                                while self.current.token_type == TokenType::Newline && self.next(strings, lexer) {}
                                                if self.current.token_type == TokenType::BraceClose { break }
                                            },
                                            Err(error) => return Err(error)
                                        }
                                    }
                                    enforce_current_type!(&[TokenType::BraceClose], "a closing brace ('}')");
                                    new = AstNode::new(
                                        AstNodeVariant::CaseBranches { value: Box::new(value), branches },
                                        (&source_start..&self.current.source).into()
                                    );
                                }
                                _ => panic!("unreachable")
                            }
                        }
                        TokenType::KeywordReturn => {
                            let start_source = self.current.source.clone();
                            let value = if self.next(strings, lexer) {
                                match self.parse_expression(strings, lexer, end_at_types) {
                                    Ok(None) => None,
                                    Ok(Some(node)) => Some(Box::new(node)),
                                    Err(error) => return Err(error)
                                }
                            } else { None };
                            let result_source = match &value {
                                Some(node) => (&start_source..node.source()).into(),
                                None => start_source,
                            };
                            new = AstNode::new(
                                AstNodeVariant::Return { value },
                                result_source
                            );
                        }
                        _ => return Err(Error::new([
                            ErrorSection::Error(ErrorType::TotallyUnexpectedToken(self.current.token_content)),
                            ErrorSection::Code(self.current.source.clone())
                        ].into()))
                    }
                    if let Some(prev) = previous {
                        let prev_source = prev.source().clone();
                        let new_source = new.source().clone();
                        if let AstNodeVariant::Call { called, mut arguments } = prev.node_variant().clone() {
                            arguments.push(new);
                            previous = Some(AstNode::new(
                                AstNodeVariant::Call { called, arguments },
                                (&prev_source..&new_source).into()
                            ));
                        } else {
                            previous = Some(AstNode::new(
                                AstNodeVariant::Call { called: Box::new(prev), arguments: vec![new] },
                                (&prev_source..&new_source).into()
                            ));
                        }
                    } else {
                        previous = Some(new);
                    }
                    if fetch_next && !self.next(strings, lexer) || self.reached_end { return Ok(previous); }
                }
            }
        }
    }
}