
use crate::compiler::{
    lexer::Lexer, 
    strings::StringMap,
    ast::AstNode,
    tokens::{TokenType, Token}
};


pub struct Parser {
    current: Token,
    reached_end: bool
}

impl Parser {
    pub fn new(strings: &mut StringMap, lexer: &mut Lexer) -> Option<Parser> {
        lexer.next_token(strings)
            .map(|current| Parser {
                current,
                reached_end: false
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

    pub fn parse_block(&mut self, strings: &mut StringMap, lexer: &mut Lexer) -> Result<Vec<AstNode>, ()> {
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

    fn parse_expression_until(&mut self, strings: &mut StringMap, lexer: &mut Lexer, end_at_types: &mut Vec<&[TokenType]>, until: &'static [TokenType]) -> Result<Option<AstNode>, ()> {
        end_at_types.push(until);
        let result = self.parse_expression(strings, lexer, end_at_types);
        end_at_types.pop();
        return result;
    }

    fn parse_expression(&mut self, strings: &mut StringMap, lexer: &mut Lexer, end_at_types: &mut Vec<&[TokenType]>) -> Result<Option<AstNode>, ()> {
        self.reached_end = false;
        let mut previous: Option<AstNode> = None;
        loop {
            for end_at in &*end_at_types {
                if end_at.contains(&self.current.token_type) { return Ok(previous) }
            }
            match self.current.token_type {
                TokenType::Hashtag => {
                    let accessed = if let Some(previous) = previous { previous }
                        else { panic!("The access operator ('#') must have a thing to access on the left, but there is nothing there!"); };
                    if !self.next(strings, lexer) { panic!("The access operator ('#') must have the part to access on the right, but got nothing instead!"); }
                    match self.current.token_type {
                        TokenType::Identifier => {
                            previous = Some(AstNode::ObjectAccess { object: Box::new(accessed), member: self.current.content });
                        }
                        TokenType::BracketOpen => {
                            if !self.next(strings, lexer) { panic!("The access operator ('#') must have the part to access on the right, but got nothing instead!"); }
                            let index = match self.parse_expression_until(strings, lexer, end_at_types, &[TokenType::BracketClose]) {
                                Ok(None) => { panic!("The access operator ('#') must have the part to access on the right, but got nothing instead"); }
                                Ok(Some(node)) => node,
                                Err(error) => return Err(error)
                            };
                            previous = Some(AstNode::ArrayAccess { array: Box::new(accessed), index: Box::new(index) });
                        }
                        _ => { panic!("The access operator ('#') must have the part to access on the right, but got '{}' instead!", strings.get(self.current.content)); }
                    }
                    if !self.next(strings, lexer) { return Ok(previous); }
                }
                TokenType::Pipe => {
                    let piped = if let Some(previous) = previous { previous }
                        else { panic!("The pipe operator ('|') must have a thing to pipe on the left, but there is nothing there!"); };
                    if !self.next(strings, lexer) { panic!("The pipe operator ('|') must have the call to pipe into on the right, but got nothing instead!"); }
                    let into = match self.parse_expression_until(strings, lexer, end_at_types, &[TokenType::Pipe]) {
                        Ok(None) => { panic!("The pipe operator ('|') must have the call to pipe into on the right, but got nothing instead!"); }
                        Ok(Some(node)) => node,
                        Err(error) => return Err(error)
                    };
                    if let AstNode::Call { called, mut arguments } = into {
                        arguments.insert(0, piped);
                        previous = Some(AstNode::Call { called, arguments });
                    } else {
                        previous = Some(AstNode::Call { called: Box::new(into), arguments: vec![piped] });
                    }
                    if self.reached_end { return Ok(previous); }
                }
                TokenType::Equals => {
                    let assigned_to = if let Some(previous) = previous { previous }
                    else { panic!("The assignment operator ('=') must have a thing to assign to on the left, but there is nothing there!"); };
                    if !self.next(strings, lexer) { panic!("The assignment operator ('=') must have the thing to assign on the right, but got nothing instead!"); }
                    let assigned = match self.parse_expression(strings, lexer, end_at_types) {
                        Ok(None) => { panic!("The assignment operator ('=') must have the thing to assign on the right, but got nothing instead!"); }
                        Ok(Some(node)) => node,
                        Err(error) => return Err(error)
                    };
                    previous = Some(AstNode::Assignment { variable: Box::new(assigned_to), value: Box::new(assigned) });
                    if self.reached_end { return Ok(previous); }
                }
                _ => {
                    let new;
                    let mut fetch_next = true;
                    match self.current.token_type {
                        TokenType::Identifier => new = AstNode::VariableAccess { name: self.current.content },
                        TokenType::String => new = AstNode::StringLiteral { value: self.current.content },
                        TokenType::Integer => new = AstNode::IntegerLiteral { value: strings.get(self.current.content).parse().expect("Lexer done messed up lmfao") },
                        TokenType::Fraction => new = AstNode::FractionLiteral { value: strings.get(self.current.content).parse().expect("Lexer done messed up lmfao") },
                        TokenType::ParenOpen => {
                            if !self.next(strings, lexer) { panic!("Parentheses may contain an expression and must be closed, but got nothing instead!"); }
                            new = match self.parse_expression_until(strings, lexer, end_at_types, &[TokenType::ParenClose]) {
                                Ok(None) => AstNode::UnitLiteral,
                                Ok(Some(node)) => node,
                                Err(error) => return Err(error)
                            };
                        }
                        TokenType::BraceOpen => {
                            let mut values = Vec::new();
                            while self.current.token_type != TokenType::BraceClose {
                                if !self.next(strings, lexer) { panic!("Objects must consist of a list of name and value pairs surrounded by braces, but the object is never closed!"); }
                                if self.current.token_type == TokenType::Identifier {}
                                    else if self.current.token_type == TokenType::BraceClose { continue; }
                                    else { panic!("Objects must consist of a list of name and value pairs, but got '{}' instead!", strings.get(self.current.content)); }
                                let member = self.current.content;
                                if !self.next(strings, lexer) { panic!("Objects must consist of a list of name and value pairs, but got nothing instead!"); }
                                if let TokenType::Equals = self.current.token_type {}
                                    else { panic!("Objects must consist of a list of name and value pairs, but got '{}' instead!", strings.get(self.current.content)); }
                                if !self.next(strings, lexer) { panic!("Objects must consist of a list of name and value pairs, but got nothing instead!"); }
                                let value = match self.parse_expression_until(strings, lexer, end_at_types, &[TokenType::Comma, TokenType::BraceClose]) {
                                    Ok(None) => { panic!("Objects must consist of a list of name and value pairs, but got nothing instead!"); }
                                    Ok(Some(node)) => node,
                                    Err(error) => return Err(error)
                                };
                                values.push((member, value));
                            }
                            new = AstNode::Object { values };
                        }
                        TokenType::BracketOpen => {
                            let mut values = Vec::new();
                            while self.current.token_type != TokenType::BracketClose {
                                if !self.next(strings, lexer) { panic!("Arrays must consist of a list of values surrounded by brackets, but the array is never closed!"); }
                                values.push(match self.parse_expression_until(strings, lexer, end_at_types, &[TokenType::Comma, TokenType::BracketClose]) {
                                    Ok(None) => if self.current.token_type != TokenType::BracketClose {
                                        panic!("Arrays must consist of a list of values, but got nothing instead!");
                                    } else { continue },
                                    Ok(Some(node)) => node,
                                    Err(error) => return Err(error)
                                });
                            }
                            new = AstNode::Array { values };
                        }
                        TokenType::KeywordProcedure => {
                            if !self.next(strings, lexer) { panic!("'proc' must be followed by its name, but got nothing instead!"); }
                            if self.current.token_type != TokenType::Identifier { panic!("'proc' must be followed by its name, but got '{}' instead!", strings.get(self.current.content)); }
                            let name = self.current.content;
                            if !self.next(strings, lexer) { panic!("'proc' must be followed by its name and its parameter names, but got nothing instead!"); }
                            let mut arguments = Vec::new();
                            loop {
                                match self.current.token_type {
                                    TokenType::Identifier => arguments.push(self.current.content),
                                    TokenType::BraceOpen => break,
                                    _ => { panic!("'proc' must be followed by its name, its parameter names and a block, but got '{}' instead!", strings.get(self.current.content)); }
                                }
                                if !self.next(strings, lexer) { panic!("'proc' must be followed by its name, its parameter names and a block, but got '{}' instead!", strings.get(self.current.content)); }
                            }
                            if !self.next(strings, lexer) { panic!("'proc' must be followed by its name, its parameter names and and a block, but got nothing instead!"); }
                            let body = match self.parse_block(strings, lexer) {
                                Ok(body) => body,
                                Err(error) => return Err(error)
                            };
                            if self.current.token_type != TokenType::BraceClose { panic!("Blocks must always be closed, but this one was not!"); }
                            new = AstNode::Procedure { name, arguments, body }
                        }
                        TokenType::KeywordFunction => {
                            if !self.next(strings, lexer) { panic!("'func' must be followed by parameter names, but got nothing instead!"); }
                            let mut arguments = Vec::new();
                            loop {
                                match self.current.token_type {
                                    TokenType::Identifier => arguments.push(self.current.content),
                                    TokenType::BraceOpen => break,
                                    _ => { panic!("'func' must be followed by parameter names and a block, but got '{}' instead!", strings.get(self.current.content)); }
                                }
                                if !self.next(strings, lexer) { panic!("'func' must be followed by parameter names and a block, but got '{}' instead!", strings.get(self.current.content)); }
                            }
                            if !self.next(strings, lexer) { panic!("'func' must be followed by parameter names and a block, but got nothing instead!"); }
                            let body = match self.parse_block(strings, lexer) {
                                Ok(body) => body,
                                Err(error) => return Err(error)
                            };
                            if self.current.token_type != TokenType::BraceClose { panic!("Blocks must always be closed, but this one was not!"); }
                            new = AstNode::Function { arguments, body }
                        }
                        TokenType::KeywordVariable |
                        TokenType::KeywordMutable => {
                            let mutable = self.current.token_type == TokenType::KeywordMutable;
                            if mutable {
                                if !self.next(strings, lexer) { panic!("'mut' must be followed by 'var', but got nothing instead!"); }
                                if self.current.token_type != TokenType::KeywordVariable {
                                    panic!("'mut' must be followed by 'var', but got '{}' instead!", strings.get(self.current.content));
                                }
                            }
                            if !self.next(strings, lexer) { panic!("'var' must be followed by the name of the variable, but got nothing instead!"); }
                            if self.current.token_type != TokenType::Identifier { panic!("'var' must be followed by the name of the variable, but got '{}' instead!", strings.get(self.current.content)); }
                            let name = self.current.content;
                            if !self.next(strings, lexer) { panic!("'var' must be followed by the name of the variable and '=', but got nothing instead!"); }
                            if self.current.token_type != TokenType::Equals { panic!("'var' must be followed by the name of the variable and '=', but got '{}' instead!", strings.get(self.current.content)); }
                            self.next(strings, lexer);
                            let value = match self.parse_expression(strings, lexer, end_at_types) {
                                Ok(None) => { panic!("'var' must be followed by the name of the variable, '=' and the value to assign, but got nothing instead!"); }
                                Ok(Some(node)) => node,
                                Err(error) => return Err(error)
                            };
                            fetch_next = false;
                            new = AstNode::Variable { name, mutable, value: Box::new(value) }
                        }
                        TokenType::KeywordCase => {
                            if !self.next(strings, lexer) { panic!("'case' must have a value to base decisions on on the right, but got nothing instead!"); }
                            let value = match self.parse_expression_until(strings, lexer, end_at_types, &[TokenType::Arrow, TokenType::BraceOpen]) {
                                Ok(None) => { panic!("'case' must have a value to base decisions on on the right, but got nothing instead!"); },
                                Ok(Some(node)) => node,
                                Err(error) => return Err(error)
                            };
                            if self.reached_end { panic!("'case' must have a value to base decisions on on the right followed by `->` and a value or a block (conditional) or a block of possible branches (branching), but got nothing instead!"); }
                            match self.current.token_type {
                                TokenType::Arrow => {
                                    if !self.next(strings, lexer) { panic!("Conditional 'case' must have a value to base decisions on on the right followed by `->` and a value or a block, but got nothing instead!"); }
                                    let body = if self.current.token_type == TokenType::BraceOpen {
                                        if !self.next(strings, lexer) { panic!("Conditional 'case' must have a value to base decisions on on the right followed by `->` and a value or a block, but got nothing instead!"); }
                                        let body = match self.parse_block(strings, lexer) {
                                            Ok(body) => body,
                                            Err(error) => return Err(error)
                                        };
                                        if self.reached_end || self.current.token_type != TokenType::BraceClose { panic!("Blocks must always be closed, but this one was not!"); }
                                        body
                                    } else {
                                        fetch_next = false;
                                        vec![match self.parse_expression(strings, lexer, end_at_types) {
                                            Ok(None) => { panic!("Conditional 'case' must have a value to base decisions on on the right followed by `->` and a value or a block, but got nothing instead!"); },
                                            Ok(Some(node)) => node,
                                            Err(error) => return Err(error)
                                        }]
                                    };
                                    new = AstNode::CaseConditon { condition: Box::new(value), body };
                                }
                                TokenType::BraceOpen => {
                                    if !self.next(strings, lexer) { panic!("Branching 'case' must have a value to base decisions on on the right followed by a block of possible branches, but got nothing instead!"); }
                                    let mut branches = Vec::new();
                                    while self.current.token_type == TokenType::Newline && self.next(strings, lexer) {}
                                    loop {
                                        match self.parse_expression(strings, lexer, &mut vec![&[TokenType::Arrow]]) {
                                            Ok(None) => break,
                                            Ok(Some(value)) => {
                                                if self.reached_end || self.current.token_type != TokenType::Arrow { panic!("Branches must be two expressions divided by an arrow ('->'), but there is no arrow here!"); }
                                                if !self.next(strings, lexer) { panic!("Branches must have an expresion or block after the arrow ('->'), but got nothing instead!"); }
                                                let body = if self.current.token_type == TokenType::BraceOpen {
                                                    if !self.next(strings, lexer) { panic!("Branches must have an expresion or block after the arrow ('->'), but got nothing instead!"); }
                                                    let body = match self.parse_block(strings, lexer) {
                                                        Ok(body) => body,
                                                        Err(error) => return Err(error)
                                                    };
                                                    if self.current.token_type != TokenType::BraceClose || !self.next(strings, lexer) { panic!("Blocks must always be closed, but this one was not!"); }
                                                    body
                                                } else {
                                                    vec![match self.parse_expression(strings, lexer, &mut vec![&[TokenType::Newline, TokenType::BraceClose]]) {
                                                        Ok(None) => { panic!("Branches must have an expresion or block after the arrow ('->'), but got nothing instead!"); },
                                                        Ok(Some(node)) => node,
                                                        Err(error) => return Err(error)
                                                    }]
                                                };
                                                branches.push((value, body));
                                                while self.current.token_type == TokenType::Newline && self.next(strings, lexer) {}
                                                if self.current.token_type == TokenType::BraceClose { break }
                                            },
                                            Err(error) => return Err(error)
                                        }
                                    }
                                    if self.current.token_type != TokenType::BraceClose { panic!("Blocks must always be closed, but this one was not!"); }
                                    new = AstNode::CaseBranches { value: Box::new(value), branches };
                                }
                                _ => { panic!("'case' must have a value to base decisions on on the right followed by `->` and a value or a block (conditional) or a block of possible branches (branching), but got '{}' instead!", strings.get(self.current.content)); }
                            }
                        }
                        TokenType::KeywordReturn => {
                            new = AstNode::Return { value: if self.next(strings, lexer) {
                                match self.parse_expression(strings, lexer, end_at_types) {
                                    Ok(None) => None,
                                    Ok(Some(node)) => Some(Box::new(node)),
                                    Err(error) => return Err(error)
                                }
                            } else { None } };
                        }
                        _ => panic!("'{}' is completely unexpected here!", strings.get(self.current.content))
                    }
                    if let Some(prev) = previous {
                        if let AstNode::Call { called, mut arguments } = prev {
                            arguments.push(new);
                            previous = Some(AstNode::Call { called, arguments });
                        } else {
                            previous = Some(AstNode::Call { called: Box::new(prev), arguments: vec![new] });
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