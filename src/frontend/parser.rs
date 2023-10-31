
use crate::util::{
    strings::StringMap,
    error::{Error, ErrorSection, ErrorType},
    source::{HasSource, SourceRange}
};
use crate::frontend::{
    lexer::Lexer,
    ast::{AstNode, AstNodeVariant, HasAstNodeVariant},
    tokens::{TokenType, Token},
    modules::NamespacePath
};


const PREFIX_MINUS_PRECEDENCE: Option<usize> = Some(0);
const HASHTAG_PRECEDENCE: Option<usize> = Some(7);

fn get_operator_precedence(token_type: TokenType) -> Option<usize> {
    match token_type {
        TokenType::ParenOpen |
        TokenType::ExclamationMark => Some(0),
        TokenType::Asterisk |
        TokenType::Slash |
        TokenType::Percent => Some(1),
        TokenType::Plus |
        TokenType::Minus => Some(2),
        TokenType::LessThan |
        TokenType::GreaterThan |
        TokenType::LessThanEqual |
        TokenType::GreaterThanEqual => Some(3),
        TokenType::Equals |
        TokenType::NotEquals => Some(4),
        TokenType::DoubleAmpersand => Some(5),
        TokenType::DoublePipe => Some(6),
        TokenType::FunctionPipe => Some(7),
        _ => None
    }
}


pub struct Parser {
    current: Token,
    reached_end: bool
}

impl Parser {
    pub fn new(strings: &mut StringMap, lexer: &mut Lexer) -> Option<Result<Parser, Error>> {
        lexer.next_token(strings)
            .map(|res| res.map(|token| Parser {
                current: token,
                reached_end: false
            }))
    }

    fn next(&mut self, strings: &mut StringMap, lexer: &mut Lexer) -> Result<bool, Error> {
        match lexer.next_token(strings) {
            None => {
                self.reached_end = true;
                Ok(false)       
            }
            Some(Err(error)) => Err(error),
            Some(Ok(token)) => {
                self.current = token;
                Ok(true)
            }
        }
    }

    pub fn parse_block(&mut self, strings: &mut StringMap, lexer: &mut Lexer) -> Result<Vec<AstNode>, Error> {
        let mut nodes = Vec::new();
        while !self.reached_end {
            match self.parse_expression(strings, lexer, &mut vec![&[TokenType::BraceClose]], None) {
                Ok(None) => break,
                Ok(Some(node)) => nodes.push(node),
                Err(error) => return Err(error)
            }
        }
        Ok(nodes)
    }

    fn parse_expression_until(&mut self, strings: &mut StringMap, lexer: &mut Lexer, end_at_types: &mut Vec<&[TokenType]>, until: &'static [TokenType], precedence: Option<usize>) -> Result<Option<AstNode>, Error> {
        end_at_types.push(until);
        let result = self.parse_expression(strings, lexer, end_at_types, precedence);
        end_at_types.pop();
        return result;
    }

    fn parse_expression(&mut self, strings: &mut StringMap, lexer: &mut Lexer, end_at_types: &mut Vec<&[TokenType]>, precedence: Option<usize>) -> Result<Option<AstNode>, Error> {
        self.reached_end = false;
        let mut previous: Option<AstNode> = None;
        macro_rules! next {
            () => {
                match self.next(strings, lexer) {
                    Ok(did) => did,
                    Err(error) => return Err(error)
                }
            };
        }
        macro_rules! enforce_previous {
            ($expected: expr) => {
                if let Some(previous) = previous { previous }
                else { return Err(Error::new([
                    ErrorSection::Error(ErrorType::MissingLeftExpr($expected)),
                    ErrorSection::Code(self.current.source)
                ].into())); }
            };
        }
        macro_rules! enforce_next {
            ($expected: expr) => {
                if !match self.next(strings, lexer) {
                    Ok(did) => did,
                    Err(error) => return Err(error)
                } { return Err(Error::new([
                    ErrorSection::Error(ErrorType::UnexpectedEnd($expected)),
                    ErrorSection::Code(self.current.source)
                ].into())); }
            };
        }
        macro_rules! enforce_current_type {
            ($types: expr, $expected: expr) => {
                if !$types.contains(&self.current.token_type) { return Err(Error::new([
                    ErrorSection::Error(ErrorType::UnexpectedToken($expected, self.current.token_content)),
                    ErrorSection::Code(self.current.source)
                ].into())); }
            };
        }
        macro_rules! enforce_expression {
            ($until: expr, $precedence: expr, $expected: expr) => {
                match self.parse_expression_until(strings, lexer, end_at_types, $until, $precedence) {
                    Ok(None) => return Err(Error::new([
                        ErrorSection::Error(ErrorType::UnexpectedEnd($expected)),
                        ErrorSection::Code(self.current.source)
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
                    ErrorSection::Code(self.current.source)
                ].into())); }
            };
        }
        macro_rules! parse_infix_operator {
            ($result_type: ident, $precedence: expr, $left_name: expr, $right_name: expr) => {
                let a = enforce_previous!($left_name);
                let a_source = a.source();
                enforce_next!($right_name);
                let b = enforce_expression!(&[], $precedence, $right_name);
                let b_source = b.source();
                previous = Some(AstNode::new(
                    AstNodeVariant::$result_type { a: Box::new(a), b: Box::new(b) },
                    (&a_source..&b_source).into()
                ));
                if self.reached_end { return Ok(previous); }
            };
        }
        loop {
            if self.reached_end { return Ok(previous); }
            for end_at in &*end_at_types {
                if end_at.contains(&self.current.token_type) { return Ok(previous) }
            }
            if let Some(precedence) = precedence {
                if let Some(c_precedence) = get_operator_precedence(self.current.token_type) {
                    if c_precedence >= precedence { return Ok(previous); }
                }
            }
            match self.current.token_type {
                TokenType::Dot => {
                    let accessed = enforce_previous!("the object to access a member of");
                    enforce_next!("the name of the member to access");
                    enforce_current_type!(&[TokenType::Identifier], "the name of the member to access");
                    let accessed_source = accessed.source();
                    previous = Some(AstNode::new(
                        AstNodeVariant::ObjectAccess { object: Box::new(accessed), member: self.current.token_content },
                        (&accessed_source..&self.current.source).into()
                    ));
                    if !next!() { return Ok(previous); }
                    continue;
                }
                TokenType::BracketOpen => {
                    if previous.is_some() {
                        let accessed = enforce_previous!("the array to access an element of");
                        let accessed_source = accessed.source();
                        enforce_next!("the index to access");
                        let index = enforce_expression!(&[TokenType::BracketClose], None, "the index to access");
                        enforce_current_type!(&[TokenType::BracketClose], "a closing bracket (']')");
                        previous = Some(AstNode::new(
                            AstNodeVariant::ArrayAccess { array: Box::new(accessed), index: Box::new(index) },
                            (&accessed_source..&self.current.source).into()
                        ));
                        if !next!() { return Ok(previous); }
                        continue;
                    }
                }
                TokenType::FunctionPipe => {
                    let piped = enforce_previous!("the thing to pipe");
                    let piped_source = piped.source();
                    enforce_next!("the call to pipe into");
                    let into = enforce_expression!(&[], get_operator_precedence(TokenType::FunctionPipe), "the call to pipe into");
                    let into_source = into.source();
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
                    continue;
                }
                TokenType::Equals => {
                    let assigned_to = enforce_previous!("the thing to assign to");
                    let assigned_to_source = assigned_to.source();
                    enforce_next!("the value to assign");
                    let assigned = enforce_expression!(&[], None, "the value to assign");
                    let assigned_source = assigned.source();
                    previous = Some(AstNode::new(
                        AstNodeVariant::Assignment { variable: Box::new(assigned_to), value: Box::new(assigned) },
                        (&assigned_to_source..&assigned_source).into()
                    ));
                    if self.reached_end { return Ok(previous); }
                    continue;
                }
                TokenType::Plus => {
                    parse_infix_operator!(Add, get_operator_precedence(TokenType::Plus), "the first thing to add", "the second thing to add");
                    continue;
                }
                TokenType::Minus => {
                    if previous.is_some() {
                        parse_infix_operator!(Subtract, get_operator_precedence(TokenType::Minus), "the thing to subtract from", "the amount to subtract");
                        continue;
                    }
                }
                TokenType::Asterisk => {
                    parse_infix_operator!(Multiply, get_operator_precedence(TokenType::Asterisk), "the first thing to multiply", "the first thing to multiply");
                    continue;
                }
                TokenType::Slash => {
                    if previous.is_some() {
                        parse_infix_operator!(Divide, get_operator_precedence(TokenType::Slash), "the thing to divide", "the divisor");
                        continue;
                    }
                }
                TokenType::Percent => {
                    parse_infix_operator!(Modulo, get_operator_precedence(TokenType::Percent), "the thing to divide", "the divisor");
                    continue;
                }
                TokenType::LessThan => {
                    parse_infix_operator!(LessThan, get_operator_precedence(TokenType::LessThan), "the first thing to compare", "the second thing to compare");
                    continue;
                }
                TokenType::LessThanEqual => {
                    parse_infix_operator!(LessThanEqual, get_operator_precedence(TokenType::LessThanEqual), "the first thing to compare", "the second thing to compare");
                    continue;
                }
                TokenType::GreaterThan => {
                    parse_infix_operator!(GreaterThan, get_operator_precedence(TokenType::GreaterThan), "the first thing to compare", "the second thing to compare");
                    continue;
                }
                TokenType::GreaterThanEqual => {
                    parse_infix_operator!(GreaterThanEqual, get_operator_precedence(TokenType::GreaterThanEqual), "the first thing to compare", "the second thing to compare");
                    continue;
                }
                TokenType::DoubleEquals => {
                    parse_infix_operator!(Equals, get_operator_precedence(TokenType::DoubleEquals), "the first thing to compare", "the second thing to compare");
                    continue;
                }
                TokenType::NotEquals => {
                    parse_infix_operator!(NotEquals, get_operator_precedence(TokenType::NotEquals), "the first thing to compare", "the second thing to compare");
                    continue;
                }
                TokenType::DoubleAmpersand => {
                    parse_infix_operator!(And, get_operator_precedence(TokenType::DoubleAmpersand), "the first operand", "the second operand");
                    continue;
                }
                TokenType::DoublePipe => {
                    if previous.is_some() {
                        parse_infix_operator!(Or, get_operator_precedence(TokenType::DoublePipe), "the first operand", "the second operand");
                        continue;
                    }
                }
                TokenType::ParenOpen => if previous.is_some() {
                    let called = enforce_previous!("the thing to call");
                    enforce_next!("a call parameter or a closing parenthesis (')')");
                    let mut args = Vec::new();
                    while self.current.token_type != TokenType::ParenClose {
                        args.push(enforce_expression!(&[TokenType::Comma, TokenType::ParenClose], None, "a call parameter"));
                        enforce_current_type!(&[TokenType::Comma, TokenType::ParenClose], "a comma (',') or a closing parenthesis (')')");
                        if self.current.token_type == TokenType::Comma {
                            enforce_next!("a call parameter or a closing parenthesis (')')");
                        }
                    }
                    let called_source = called.source();
                    previous = Some(AstNode::new(
                        AstNodeVariant::Call { called: Box::new(called), arguments: args },
                        (&called_source..&self.current.source).into()
                    ));
                    next!();
                    continue;
                }
                _ => if previous.is_some() {
                    return Ok(previous);
                }
            }
            match self.current.token_type {
                TokenType::Identifier => {
                    let start = self.current.clone();
                    match (next!(), self.current.token_type) {
                        (true, TokenType::NamespaceSeparator) => {
                            let mut path_segments = vec![start.token_content];
                            let mut end_source = start.source;
                            while self.current.token_type == TokenType::NamespaceSeparator {
                                enforce_next!("the path to access");
                                enforce_current_type!(&[TokenType::Identifier], "the path to access");
                                path_segments.push(self.current.token_content);
                                end_source = self.current.source;
                                if !next!() { break; }
                            }
                            previous = Some(AstNode::new(
                                AstNodeVariant::ModuleAccess { path: NamespacePath::new(path_segments) },
                                (&start.source..&end_source).into()
                            ));
                        }
                        (_, _) => previous = Some(AstNode::new(
                            AstNodeVariant::VariableAccess { name: start.token_content },
                            start.source
                        )),
                    }
                }
                TokenType::String => {
                    previous = Some(AstNode::new(
                        AstNodeVariant::StringLiteral { value: self.current.token_content },
                        self.current.source
                    ));
                    next!();
                }
                TokenType::Integer => {
                    previous = Some(AstNode::new(
                        AstNodeVariant::IntegerLiteral { value: strings.get(self.current.token_content).parse().expect("Lexer done messed up lmfao") },
                        self.current.source
                    ));
                    next!();
                }
                TokenType::Fraction => {
                    previous = Some(AstNode::new(
                        AstNodeVariant::FloatLiteral { value: strings.get(self.current.token_content).parse().expect("Lexer done messed up lmfao") },
                        self.current.source
                    ));
                    next!();
                }
                TokenType::Minus => {
                    let source_start = self.current.source;
                    enforce_next!("the thing to mathematically negate");
                    let x = enforce_expression!(&[], PREFIX_MINUS_PRECEDENCE, "the thing to mathematically negate");
                    let x_source = x.source();
                    previous = Some(AstNode::new(
                        AstNodeVariant::Negate { x: Box::new(x) },
                        (&source_start..&x_source).into()
                    ));
                }
                TokenType::ExclamationMark => {
                    let source_start = self.current.source;
                    enforce_next!("the thing to logically negate");
                    let x = enforce_expression!(&[], get_operator_precedence(TokenType::ExclamationMark), "the thing to logically negate");
                    let x_source = x.source();
                    previous = Some(AstNode::new(
                        AstNodeVariant::Not { x: Box::new(x) },
                        (&source_start..&x_source).into()
                    ));
                }
                TokenType::ParenOpen => {
                    enforce_next!("the contained expression");
                    previous = Some(enforce_expression!(&[TokenType::ParenClose], None, "the contained expression"));
                    enforce_current_type!(&[TokenType::ParenClose], "a closing parenthesis (')')");
                    next!();
                }
                TokenType::BraceOpen => {
                    let mut values = Vec::new();
                    let source_start = self.current.source;
                    enforce_next!("the name of an object member or a closing brace ('}')");
                    while self.current.token_type != TokenType::BraceClose {
                        enforce_current_type!(&[TokenType::Identifier], "the name of an object member");
                        let member = self.current.token_content;
                        enforce_next!("an equals sign ('=')");
                        enforce_current_type!(&[TokenType::Equals], "an equals sign ('=')");
                        enforce_next!("the member value");
                        let value = enforce_expression!(&[TokenType::Comma, TokenType::BraceClose], None, "the member value");
                        values.push((member, value));
                        enforce_current_type!(&[TokenType::Comma, TokenType::BraceClose], "a comma (',') or a closing brace ('}')");
                        if self.current.token_type == TokenType::Comma {
                            enforce_next!("the name of an object member or a closing brace ('}')");
                        }
                    }
                    // enforce_current_type!(&[TokenType::BraceClose], "a closing brace ('}')");
                    previous = Some(AstNode::new(
                        AstNodeVariant::Object { values },
                        (&source_start..&self.current.source).into()
                    ));
                    next!();
                }
                TokenType::BracketOpen => {
                    let mut values = Vec::new();
                    let source_start = self.current.source;
                    enforce_next!("an array value or a closing brace (']')");
                    while self.current.token_type != TokenType::BracketClose {
                        values.push(enforce_expression!(&[TokenType::Comma, TokenType::BracketClose], None, "an array value"));
                        enforce_current_type!(&[TokenType::Comma, TokenType::BracketClose], "a comma (',') or a closing bracket (']')");
                        if self.current.token_type == TokenType::Comma {
                            enforce_next!("an array value or a closing brace (']')");
                        }
                    }
                    // enforce_current_type!(&[TokenType::BracketClose], "a closing bracket (']')");
                    previous = Some(AstNode::new(
                        AstNodeVariant::Array { values },
                        (&source_start..&self.current.source).into()
                    ));
                    next!();
                }
                TokenType::KeywordUnit => {
                    previous = Some(AstNode::new(
                        AstNodeVariant::UnitLiteral {},
                        self.current.source
                    ));
                    next!();
                }
                TokenType::Pipe | TokenType::DoublePipe => {
                    let source_start = self.current.source;
                    let mut arguments = Vec::new();
                    if TokenType::Pipe == self.current.token_type {
                        enforce_next!("a function parameter's name or a pipe ('|')");
                        loop {
                            enforce_current_type!(&[TokenType::Identifier, TokenType::Pipe], "a function parameter's name or a pipe ('|')");
                            match self.current.token_type {
                                TokenType::Identifier => arguments.push(self.current.token_content),
                                TokenType::Pipe => break,
                                _ => panic!("unreachable")
                            }
                            enforce_next!("a comma (',') or a pipe ('|')");
                            enforce_current_type!(&[TokenType::Comma, TokenType::Pipe], "a comma (',') or a pipe ('|')");
                            match self.current.token_type {
                                TokenType::Comma => enforce_next!("a function parameter's name or a pipe ('|')"),
                                TokenType::Pipe => break,
                                _ => panic!("unreachable")
                            }
                        }
                        enforce_current_type!(&[TokenType::Pipe], "a pipe ('|')");
                    }
                    enforce_next!("the function's body");
                    if self.current.token_type == TokenType::BraceOpen {
                        enforce_next!("the function's body");
                        let body = match self.parse_block(strings, lexer) {
                            Ok(body) => body,
                            Err(error) => return Err(error)
                        };
                        enforce_not_reached_end!("a closing brace ('}')");
                        enforce_current_type!(&[TokenType::BraceClose], "a closing brace ('}')");
                        previous = Some(AstNode::new(
                            AstNodeVariant::Function { arguments, body },
                            (&source_start..&self.current.source).into()
                        ));
                        next!();
                    } else {
                        let return_value: AstNode = enforce_expression!(&[], None, "the function's result value");
                        let return_value_source = return_value.source();
                        previous = Some(AstNode::new(
                            AstNodeVariant::Function {
                                arguments,
                                body: vec![AstNode::new(
                                    AstNodeVariant::Return { value: Box::new(return_value) },
                                    return_value_source
                                )] },
                            (&source_start..&return_value_source).into()
                        ));
                    }
                }
                TokenType::KeywordProcedure => {
                    let source_start = self.current.source;
                    enforce_next!("the procedure's name");
                    enforce_current_type!(&[TokenType::Identifier], "the procedure's name");
                    let name = self.current.token_content;
                    enforce_next!("an opening parenthesis ('(')");
                    enforce_current_type!(&[TokenType::ParenOpen], "an opening parenthesis ('(')");
                    enforce_next!("a procedure parameter's name or a closing parenthesis (')')");
                    let mut arguments = Vec::new();
                    loop {
                        enforce_current_type!(&[TokenType::Identifier, TokenType::ParenClose], "a procedure parameter's name or a closing parenthesis (')')");
                        match self.current.token_type {
                            TokenType::Identifier => arguments.push(self.current.token_content),
                            TokenType::ParenClose => break,
                            _ => panic!("unreachable")
                        }
                        enforce_next!("a comma or an closing parenthesis (')')");
                        enforce_current_type!(&[TokenType::Comma, TokenType::ParenClose], "a comma (',') or a closing parenthesis (')')");
                        match self.current.token_type {
                            TokenType::Comma => enforce_next!("a procedure parameter's name or a closing parenthesis (')')"),
                            TokenType::ParenClose => break,
                            _ => panic!("unreachable")
                        }
                    }
                    enforce_current_type!(&[TokenType::ParenClose], "a closing parenthesis (')')");
                    enforce_next!("an opening brace ('{')");
                    enforce_current_type!(&[TokenType::BraceOpen], "an opening brace ('{')");
                    enforce_next!("the procedure's body");
                    let body = match self.parse_block(strings, lexer) {
                        Ok(body) => body,
                        Err(error) => return Err(error)
                    };
                    enforce_not_reached_end!("a closing brace ('}')");
                    enforce_current_type!(&[TokenType::BraceClose], "a closing brace ('}')");
                    previous = Some(AstNode::new(
                        AstNodeVariant::Procedure { public: false, name, arguments, body },
                        (&source_start..&self.current.source).into()
                    ));
                    next!();
                }
                TokenType::KeywordVariable |
                TokenType::KeywordMutable => {
                    let source_start = self.current.source;
                    let mutable = self.current.token_type == TokenType::KeywordMutable;
                    if mutable {
                        enforce_next!("'var'");
                        enforce_current_type!(&[TokenType::KeywordVariable], "'var'");
                    }
                    enforce_next!("the variable's name");
                    enforce_current_type!(&[TokenType::Identifier], "the variable's name");
                    let name = self.current.token_content;
                    let name_source = self.current.source;
                    previous = if next!() && self.current.token_type == TokenType::Equals {
                        enforce_next!("the variable's value");
                        let value = enforce_expression!(&[], None, "the variable's value");
                        let value_source = value.source();
                        Some(AstNode::new(
                            AstNodeVariant::Variable { public: false, mutable, name, value: Some(Box::new(value)) },
                            (&source_start..&value_source).into()
                        ))
                    } else {
                        Some(AstNode::new(
                            AstNodeVariant::Variable { public: false, mutable, name, value: None },
                            (&source_start..&name_source).into()
                        ))
                    }
                }
                TokenType::KeywordCase => {
                    let source_start = self.current.source;
                    enforce_next!("the condition value");
                    let value = enforce_expression!(&[], None, "the condition value");
                    enforce_not_reached_end!("an arrow ('->') (conditonal case) or an opening brace ('{') (branching case)");
                    enforce_current_type!(&[TokenType::Arrow, TokenType::BraceOpen], "an arrow ('->') (conditonal case) or an opening brace ('{') (branching case)");
                    match self.current.token_type {
                        TokenType::Arrow => {
                            enforce_next!("the body of the conditional branch");
                            let mut source_end;
                            let body = if self.current.token_type == TokenType::BraceOpen {
                                enforce_next!("the body of the conditional branch");
                                let body = match self.parse_block(strings, lexer) {
                                    Ok(body) => body,
                                    Err(error) => return Err(error)
                                };
                                enforce_not_reached_end!("a closing brace ('}')");
                                enforce_current_type!(&[TokenType::BraceClose], "a closing brace ('}')");
                                source_end = self.current.source;
                                next!();
                                body
                            } else {
                                let body = enforce_expression!(&[], None, "the body of the conditional branch");
                                source_end = body.source();
                                vec![body]
                            };
                            let else_body = if !self.reached_end && self.current.token_type == TokenType::KeywordElse {
                                enforce_next!("the body of the 'else'-branch");
                                if self.current.token_type == TokenType::BraceOpen {
                                    enforce_next!("the body of the 'else'-branch");
                                    let body = match self.parse_block(strings, lexer) {
                                        Ok(body) => body,
                                        Err(error) => return Err(error)
                                    };
                                    enforce_not_reached_end!("a closing brace ('}')");
                                    enforce_current_type!(&[TokenType::BraceClose], "a closing brace ('}')");
                                    source_end = self.current.source;
                                    next!();
                                    body
                                } else {
                                    let body = enforce_expression!(&[], None, "the body of the 'else'-branch");
                                    source_end = body.source();
                                    vec![body]
                                }
                            } else { Vec::new() };
                            previous = Some(AstNode::new(
                                AstNodeVariant::CaseConditon { condition: Box::new(value), body, else_body },
                                (&source_start..&source_end).into()
                            ));
                        }
                        TokenType::BraceOpen => {
                            enforce_next!("a branch value");
                            if self.current.token_type == TokenType::Hashtag {
                                let mut branches = Vec::new();
                                while self.current.token_type != TokenType::BraceClose {
                                    enforce_current_type!(&[TokenType::Hashtag], "a hashtag ('#')");
                                    enforce_next!("the variant's name");
                                    enforce_current_type!(&[TokenType::Identifier], "the variant's name");
                                    let branch_variant_name = self.current.token_content;
                                    enforce_next!("the value's variable's name");
                                    enforce_current_type!(&[TokenType::Identifier], "the value's variable's name");
                                    let branch_variable_name = self.current.token_content;
                                    enforce_next!("an arrow ('->')");
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
                                        enforce_next!("the value for the branch or a closing brace ('}')");
                                        body
                                    } else {
                                        vec![enforce_expression!(&[TokenType::BraceClose], None, "the body of the conditional branch")]
                                    };
                                    branches.push((branch_variant_name, branch_variable_name, None, body));
                                }
                                enforce_current_type!(&[TokenType::BraceClose], "a closing brace ('}')");
                                let mut source_end = self.current.source;
                                next!();
                                let else_body = if !self.reached_end && self.current.token_type == TokenType::KeywordElse {
                                    enforce_next!("the body of the 'else'-branch");
                                    if self.current.token_type == TokenType::BraceOpen {
                                        enforce_next!("the body of the 'else'-branch");
                                        let body = match self.parse_block(strings, lexer) {
                                            Ok(body) => body,
                                            Err(error) => return Err(error)
                                        };
                                        enforce_not_reached_end!("a closing brace ('}')");
                                        enforce_current_type!(&[TokenType::BraceClose], "a closing brace ('}')");
                                        source_end = self.current.source;
                                        next!();
                                        Some(body)
                                    } else {
                                        let body = enforce_expression!(&[], None, "the body of the 'else'-branch");
                                        source_end = body.source();
                                        Some(vec![body])
                                    }
                                } else { None };
                                previous = Some(AstNode::new(
                                    AstNodeVariant::CaseVariant { value: Box::new(value), branches, else_body },
                                    (&source_start..&source_end).into()
                                ));
                            } else {
                                let mut branches = Vec::new();
                                while self.current.token_type != TokenType::BraceClose {
                                    match self.parse_expression(strings, lexer, &mut vec![&[TokenType::Arrow]], None) {
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
                                                enforce_next!("the value for the branch or a closing brace ('}')");
                                                body
                                            } else {
                                                vec![enforce_expression!(&[TokenType::BraceClose], None, "the body of the conditional branch")]
                                            };
                                            branches.push((value, body));
                                            if self.current.token_type == TokenType::BraceClose { break }
                                        },
                                        Err(error) => return Err(error)
                                    }
                                }
                                enforce_current_type!(&[TokenType::BraceClose], "a closing brace ('}')");
                                let mut source_end = self.current.source;
                                next!();
                                let else_body = if !self.reached_end && self.current.token_type == TokenType::KeywordElse {
                                    enforce_next!("the body of the 'else'-branch");
                                    if self.current.token_type == TokenType::BraceOpen {
                                        enforce_next!("the body of the 'else'-branch");
                                        let body = match self.parse_block(strings, lexer) {
                                            Ok(body) => body,
                                            Err(error) => return Err(error)
                                        };
                                        enforce_not_reached_end!("a closing brace ('}')");
                                        enforce_current_type!(&[TokenType::BraceClose], "a closing brace ('}')");
                                        source_end = self.current.source;
                                        next!();
                                        body
                                    } else {
                                        let body = enforce_expression!(&[], None, "the body of the 'else'-branch");
                                        source_end = body.source();
                                        vec![body]
                                    }
                                } else { Vec::new() };
                                previous = Some(AstNode::new(
                                    AstNodeVariant::CaseBranches { value: Box::new(value), branches, else_body },
                                    (&source_start..&source_end).into()
                                ));
                            }
                        }
                        _ => panic!("unreachable")
                    }
                }
                TokenType::KeywordReturn => {
                    let start_source = self.current.source;
                    enforce_next!("the thing to return");
                    let value = enforce_expression!(&[], None, "the thing to return");
                    let value_source = value.source();
                    previous = Some(AstNode::new(
                        AstNodeVariant::Return { value: Box::new(value) },
                        (&start_source..&value_source).into()
                    ));
                }
                TokenType::KeywordModule => {
                    let start_source = self.current.source;
                    enforce_next!("the name of the module");
                    let mut path_segments = Vec::new();
                    enforce_current_type!(&[TokenType::Identifier], "the name of the module");
                    path_segments.push(self.current.token_content);
                    let mut end_source = self.current.source;
                    while next!() && self.current.token_type == TokenType::NamespaceSeparator {
                        enforce_next!("the name of the module");
                        enforce_current_type!(&[TokenType::Identifier], "the name of the module");
                        path_segments.push(self.current.token_content);
                        end_source = self.current.source;
                    }
                    previous = Some(AstNode::new(
                        AstNodeVariant::Module { path: NamespacePath::new(path_segments) },
                        (&start_source..&end_source).into()
                    ));
                }
                TokenType::KeywordPublic => {
                    let start_source: crate::util::source::SourceRange = self.current.source;
                    enforce_next!("the thing to be public");
                    let mut thing = enforce_expression!(&[], None, "the thing to be public");
                    match thing.node_variant_mut() {
                        AstNodeVariant::Procedure { public, name: _, arguments: _, body: _ } |
                        AstNodeVariant::Variable { public, mutable: _, name: _, value: _ } => if !*public {
                            *public = true;
                            thing.replace_source((&start_source..&thing.source()).into());
                            previous = Some(thing);
                        } else {
                            return Err(Error::new([
                                ErrorSection::Error(ErrorType::MayNotBePublic),
                                ErrorSection::Code(thing.source())
                            ].into()));
                        }
                        _ => {
                            return Err(Error::new([
                                ErrorSection::Error(ErrorType::MayNotBePublic),
                                ErrorSection::Code(thing.source())
                            ].into()));
                        }
                    }
                }
                TokenType::KeywordUse => {
                    let start_source = self.current.source;
                    enforce_next!("the things to use");
                    let mut end_source = self.current.source;
                    fn parse_usage_paths(parser: &mut Parser, strings: &mut StringMap, lexer: &mut Lexer, end_source: &mut SourceRange) -> Result<Vec<NamespacePath>, Error> {
                        macro_rules! next { () => {
                            match parser.next(strings, lexer) {
                                Ok(did) => did,
                                Err(error) => return Err(error)
                            }
                        } }
                        macro_rules! enforce_next { ($expected: expr) => {
                            if !match parser.next(strings, lexer) {
                                Ok(did) => did,
                                Err(error) => return Err(error)
                            } { return Err(Error::new([
                                ErrorSection::Error(ErrorType::UnexpectedEnd($expected)),
                                ErrorSection::Code(parser.current.source)
                            ].into())); }
                        } }
                        macro_rules! enforce_current_type { ($types: expr, $expected: expr) => {
                            if !$types.contains(&parser.current.token_type) { return Err(Error::new([
                                ErrorSection::Error(ErrorType::UnexpectedToken($expected, parser.current.token_content)),
                                ErrorSection::Code(parser.current.source)
                            ].into())); }
                        } }
                        let mut path_segments = Vec::new();
                        enforce_current_type!(&[TokenType::Identifier], "the name of the module");
                        path_segments.push(parser.current.token_content);
                        enforce_next!("the name of the module");
                        while parser.current.token_type == TokenType::NamespaceSeparator {
                            enforce_next!("the name of the module, '(' or '*'");
                            enforce_current_type!(&[TokenType::Identifier, TokenType::ParenOpen, TokenType::Asterisk], "the name of the module, '(' or '*'");
                            match parser.current.token_type {
                                TokenType::Identifier |
                                TokenType::Asterisk => {
                                    path_segments.push(parser.current.token_content);
                                    *end_source = parser.current.source;
                                }
                                TokenType::ParenOpen => {
                                    enforce_next!("the name of the module");
                                    let mut paths = Vec::new();
                                    while parser.current.token_type != TokenType::ParenClose {
                                        match parse_usage_paths(parser, strings, lexer, end_source) {
                                            Ok(mut p) => paths.append(&mut p),
                                            Err(errors) => return Err(errors)
                                        }
                                        enforce_current_type!(&[TokenType::Comma, TokenType::ParenClose], "',' or ')'");
                                        if parser.current.token_type == TokenType::Comma { enforce_next!("the name of the module or ')'"); }
                                    }
                                    enforce_current_type!(&[TokenType::ParenClose], "')'");
                                    *end_source = parser.current.source;
                                    next!();
                                    for path in &mut paths {
                                        let mut new_segments = path_segments.clone();
                                        new_segments.append(&mut path.get_segments().clone());
                                        *path = NamespacePath::new(new_segments);
                                    }
                                    return Ok(paths);
                                }
                                _ => { panic!("impossible") }
                            }
                            if !next!() { break; }
                        }
                        Ok(vec![NamespacePath::new(path_segments)])
                    }
                    previous = Some(AstNode::new(
                        AstNodeVariant::Use {
                            paths: match parse_usage_paths(self, strings, lexer, &mut end_source) {
                                Ok(p) => p,
                                Err(errors) => return Err(errors)
                            }
                        },
                        (&start_source..&end_source).into()
                    ));
                }
                TokenType::KeywordTrue => {
                    previous = Some(AstNode::new(
                        AstNodeVariant::BooleanLiteral { value: true },
                        self.current.source
                    ));
                    next!();
                }
                TokenType::KeywordFalse => {
                    previous = Some(AstNode::new(
                        AstNodeVariant::BooleanLiteral { value: false },
                        self.current.source
                    ));
                    next!();
                }
                TokenType::Hashtag => {
                    let start_source = self.current.source;
                    enforce_next!("the variant's name");
                    enforce_current_type!(&[TokenType::Identifier], "the variant's name");
                    let name = self.current.token_content;
                    enforce_next!("the variant's value");
                    let value = enforce_expression!(&[], HASHTAG_PRECEDENCE, "the variant's value");
                    let value_source = value.source();
                    previous = Some(AstNode::new(
                        AstNodeVariant::Variant {
                            name,
                            value: Box::new(value)
                        },
                        (&start_source..&value_source).into()
                    ));
                }
                _ => return Err(Error::new([
                    ErrorSection::Error(ErrorType::TotallyUnexpectedToken(self.current.token_content)),
                    ErrorSection::Code(self.current.source)
                ].into()))
            }
        }
    }
}