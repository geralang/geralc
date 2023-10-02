
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
                _ => {
                    let new;
                    match self.current.token_type {
                        TokenType::Identifier => new = AstNode::VariableAccess { name: self.current.content },
                        TokenType::String => new = AstNode::StringLiteral { value: self.current.content },
                        TokenType::Integer => new = AstNode::IntegerLiteral { value: strings.get(self.current.content).parse().expect("Lexer done messed up lmfao") },
                        TokenType::Fraction => new = AstNode::FractionLiteral { value: strings.get(self.current.content).parse().expect("Lexer done messed up lmfao") },
                        TokenType::ParenOpen => {
                            if !self.next(strings, lexer) { panic!("Parentheses must contain an expression, but got nothing instead!"); }
                            new = match self.parse_expression_until(strings, lexer, end_at_types, &[TokenType::ParenClose]) {
                                Ok(None) => { panic!("Parentheses must contain an expression, but got nothing instead!"); }
                                Ok(Some(node)) => node,
                                Err(error) => return Err(error)
                            };
                        }
                        TokenType::BraceOpen => {
                            let mut values = Vec::new();
                            while self.current.token_type != TokenType::BraceClose {
                                if !self.next(strings, lexer) { panic!("Objects must consist of a list of name and value pairs surrounded by braces, but the object is never closed!"); }
                                if let TokenType::Identifier = self.current.token_type {}
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
                                    Ok(None) => { panic!("Arrays must consist of a list of values, but got nothing instead!"); }
                                    Ok(Some(node)) => node,
                                    Err(error) => return Err(error)
                                });
                            }
                            new = AstNode::Array { values };
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
                    if !self.next(strings, lexer) { return Ok(previous); }
                }
            }
        }
    }
}