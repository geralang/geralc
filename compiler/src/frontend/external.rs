
use std::collections::HashMap;

use crate::{frontend::{
    tokens::Token,
    lexer::Lexer,
    ast::{AstNode, TypedAstNode},
    modules::{NamespacePath, Module},
    type_checking::Symbol,
    tokens::TokenType,
    types::{Type, TypeMap, TypeGroup}
}, util::source::SourceRange};
use crate::util::{
    strings::{StringMap, StringIdx},
    error::{Error, ErrorSection, ErrorType}
};


pub struct ExternalMappingParser {
    current: Token,
    reached_end: bool   
}

impl ExternalMappingParser {
    pub fn new(strings: &mut StringMap, lexer: &mut Lexer) -> Option<Result<ExternalMappingParser, Error>> {
        lexer.next_token(strings)
            .map(|res| res.map(|token| ExternalMappingParser {
                current: token,
                reached_end: false
            }))
    }

    fn try_next(&mut self, strings: &mut StringMap, lexer: &mut Lexer) -> Result<bool, Error> {
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

    fn expect_next(&mut self, strings: &mut StringMap, lexer: &mut Lexer, expected: &'static str) -> Result<(), Error> {
        if !self.try_next(strings, lexer)? {
            Err(Error::new([
                ErrorSection::Error(ErrorType::UnexpectedEnd(expected)),
                ErrorSection::Code(self.current.source)
            ].into()))
        } else { Ok(()) }
    }

    fn expect_type(&mut self, token_types: &[TokenType], expected: &'static str) -> Result<(), Error> {
        if self.reached_end || !token_types.contains(&self.current.token_type) {
            Err(Error::new([
                ErrorSection::Error(ErrorType::UnexpectedToken(expected, self.current.token_content)),
                ErrorSection::Code(self.current.source)
            ].into()))
        } else { Ok(()) }
    }

    pub fn parse_header(
        &mut self,
        strings: &mut StringMap,
        lexer: &mut Lexer,
        modules: &mut HashMap<NamespacePath, Module<AstNode>>,
        types: &mut TypeMap,
        typed_symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
        external_backings: &mut HashMap<NamespacePath, StringIdx>
    ) -> Result<(), Error> {
        let mut declared_types = HashMap::new();
        while !self.reached_end {
            match strings.get(self.current.token_content) {
                "type" => {
                    self.expect_next(strings, lexer, "the name of the type")?;
                    self.expect_type(&[TokenType::Identifier], "the name of the type")?;
                    let type_name = self.current.token_content;
                    self.expect_next(strings, lexer, "an equals-sign ('=')")?;
                    self.expect_type(&[TokenType::Equals], "an equals-sign ('=')")?;
                    self.expect_next(strings, lexer, "the type to give the name to")?;
                    let type_replacement = self.parse_type(strings, lexer, types, &declared_types)?;
                    declared_types.insert(type_name, type_replacement);
                }
                "proc" => {
                    let source_start = self.current.source;
                    self.expect_next(strings, lexer, "the full path of the procedure")?;
                    let procedure_path = self.parse_path(strings, lexer, "the full path of the procedure")?;
                    self.expect_type(&[TokenType::ParenOpen], "an opening parenthesis ('(')")?;
                    self.expect_next(strings, lexer, "a parameter's type or a closing parenthesis (')')")?;
                    let mut parameters = Vec::new();
                    while self.current.token_type != TokenType::ParenClose {
                        let paramter_type = self.parse_type(strings, lexer, types, &declared_types)?; 
                        self.expect_type(&[TokenType::ParenClose, TokenType::Comma], "a comma (',') or a closing parenthesis (')')")?;
                        if self.current.token_type == TokenType::Comma {
                            self.expect_next(strings, lexer, "a parameter's type or a closing parenthesis (')')")?;
                        }
                        parameters.push(paramter_type);
                    }
                    self.expect_next(strings, lexer, "an arrow ('->') or an equals-sign ('=')")?;
                    self.expect_type(&[TokenType::Arrow, TokenType::Equals], "an arrow ('->') or an equals-sign ('=')")?;
                    let return_type_group = if self.current.token_type == TokenType::Arrow {
                        self.expect_next(strings, lexer, "the return type")?;
                        self.parse_type(strings, lexer, types, &declared_types)?
                    } else {
                        types.insert_group(&[Type::Unit])
                    };
                    self.expect_type(&[TokenType::Equals], "an equals-sign ('=')")?;
                    self.expect_next(strings, lexer, "the name of the external backing procedure")?;
                    self.expect_type(&[TokenType::Identifier], "the name of the external backing procedure")?;
                    let backing = self.current.token_content;
                    let source_end = self.current.source;
                    self.try_next(strings, lexer)?;
                    let procedure_module_path = NamespacePath::new(procedure_path.get_segments()[0..procedure_path.get_segments().len() - 1].into());
                    if procedure_module_path.get_segments().len() == 0 {
                        return Err(Error::new([
                            ErrorSection::Error(ErrorType::NoDefinedModule(
                                "procedure",
                                *procedure_path.get_segments()
                                    .last()
                                    .expect("should have at least one segment")
                            )),
                            ErrorSection::Code((&source_start..&source_end).into())
                        ].into()));
                    }
                    if let Some(module) = modules.get_mut(&procedure_module_path) {
                        module.insert_public(
                            *procedure_path.get_segments()
                                .last()
                                .expect("should have at least one segment")
                        );
                    } else {
                        let mut module = Module::new_raw(procedure_module_path.clone());
                        module.insert_public(
                            *procedure_path.get_segments()
                                .last()
                                .expect("should have at least one segment")
                        );
                        modules.insert(procedure_module_path, module);
                    }
                    let external_str = strings.insert("<external>");
                    typed_symbols.insert(procedure_path.clone(), Symbol::Procedure {
                        public: true,
                        parameter_names: parameters.iter().enumerate().map(|(i, _)| strings.insert(&i.to_string())).collect(),
                        parameter_types: parameters,
                        returns: return_type_group,
                        body: None,
                        source: SourceRange::new(external_str, external_str, 0, 0)
                    });
                    external_backings.insert(procedure_path, backing);
                }
                "var" => {
                    let source_start = self.current.source;
                    self.expect_next(strings, lexer, "the full path of the constant")?;
                    let variable_path = self.parse_path(strings, lexer, "the full path of the constant")?;
                    let variable_type = self.parse_type(strings, lexer, types, &declared_types)?;
                    self.expect_type(&[TokenType::Equals], "an equals-sign ('=')")?;
                    self.expect_next(strings, lexer, "the name of the external backing constant")?;
                    self.expect_type(&[TokenType::Identifier], "the name of the external backing constant")?;
                    let backing = self.current.token_content;
                    let source_end = self.current.source;
                    self.try_next(strings, lexer)?;
                    let variable_module_path = NamespacePath::new(variable_path.get_segments()[0..variable_path.get_segments().len() - 1].into());
                    if variable_module_path.get_segments().len() == 0 {
                        return Err(Error::new([
                            ErrorSection::Error(ErrorType::NoDefinedModule(
                                "constant",
                                *variable_path.get_segments()
                                    .last()
                                    .expect("should have at least one segment")
                            )),
                            ErrorSection::Code((&source_start..&source_end).into())
                        ].into()));
                    }
                    if let Some(module) = modules.get_mut(&variable_module_path) {
                        module.insert_public(
                            *variable_path.get_segments()
                                .last()
                                .expect("should have at least one segment")
                        );
                    } else {
                        let mut module = Module::new_raw(variable_module_path.clone());
                        module.insert_public(
                            *variable_path.get_segments()
                                .last()
                                .expect("should have at least one segment")
                        );
                        modules.insert(variable_module_path, module);
                    }
                    typed_symbols.insert(variable_path.clone(), Symbol::Constant {
                        public: true,
                        value: None,
                        value_types: variable_type
                    });
                    external_backings.insert(variable_path, backing);
                }
                _ => { 
                    return Err(Error::new([
                        ErrorSection::Error(ErrorType::UnexpectedToken("'type', 'proc' or 'var'", self.current.token_content)),
                        ErrorSection::Code(self.current.source)
                    ].into()));
                }
            }
        }
        Ok(())
    }

    fn parse_type(
        &mut self,
        strings: &mut StringMap,
        lexer: &mut Lexer,
        types: &mut TypeMap,
        declared_types: &HashMap<StringIdx, TypeGroup>
    ) -> Result<TypeGroup, Error> {
        match self.current.token_type {
            TokenType::Pipe | TokenType::DoublePipe => {
                let mut arg_types = Vec::new();
                if let TokenType::Pipe = self.current.token_type {
                    self.try_next(strings, lexer)?;
                    loop {
                        let arg_type = self.parse_type(strings, lexer, types, declared_types)?;
                        arg_types.push(arg_type);
                        self.expect_type(&[TokenType::Comma, TokenType::Pipe], "a comma (',') or a pipe ('|')")?;
                        if self.current.token_type == TokenType::Pipe { break; }
                        self.try_next(strings, lexer)?;
                    }
                }
                self.try_next(strings, lexer)?;
                self.expect_type(&[TokenType::Arrow], "an arrow ('->')")?;
                self.try_next(strings, lexer)?;
                let return_type = self.parse_type(strings, lexer, types, declared_types)?;
                let closure_tidx = types.insert_closure(
                    arg_types,
                    return_type
                );
                Ok(types.insert_group(&[
                    Type::Closure(closure_tidx)
                ]))
            }
            TokenType::Identifier | TokenType::KeywordUnit => {
                let name = self.current.clone();
                self.try_next(strings, lexer)?;
                match strings.get(name.token_content) {
                    "unit" => Ok(types.insert_group(&[Type::Unit])),
                    "bool" => Ok(types.insert_group(&[Type::Boolean])),
                    "int" => Ok(types.insert_group(&[Type::Integer])),
                    "float" => Ok(types.insert_group(&[Type::Float])),
                    "str" => Ok(types.insert_group(&[Type::String])),
                    _ => if let Some(replacement) = declared_types.get(&name.token_content) {
                        Ok(*replacement)
                    } else {
                        Err(Error::new([
                            ErrorSection::Error(ErrorType::TypeDoesNotExist(name.token_content)),
                            ErrorSection::Code(name.source)
                        ].into()))
                    }
                }
            }
            TokenType::BraceOpen => {
                self.expect_next(strings, lexer, "the name of a member")?;
                self.expect_type(&[TokenType::Identifier], "the name of a member")?;
                let mut members = Vec::new();
                while self.current.token_type != TokenType::BraceClose {
                    self.expect_type(&[TokenType::Identifier], "the name of a member")?;
                    let member_name = self.current.token_content;
                    self.expect_next(strings, lexer, "an equals-sign ('=')")?;
                    self.expect_type(&[TokenType::Equals], "an equals-sign ('=')")?;
                    self.expect_next(strings, lexer, "the type of the member")?;
                    let member_type = self.parse_type(strings, lexer, types, declared_types)?;
                    self.expect_type(&[TokenType::BraceClose, TokenType::Comma], "a comma (',') or a closing brace ('}')")?;
                    if self.current.token_type == TokenType::Comma {
                        self.expect_next(strings, lexer, "the name of a member or a closing brace ('}')")?;
                    }
                    members.push((member_name, member_type));
                }
                self.try_next(strings, lexer)?;
                let cobject_tidx = types.insert_concrete_object(members);
                Ok(types.insert_group(&[
                    Type::ConcreteObject(cobject_tidx)
                ]))
            }
            TokenType::BracketOpen => {
                self.expect_next(strings, lexer, "the type of the array's elements")?;
                let element_type = self.parse_type(strings, lexer, types, declared_types)?;
                self.expect_type(&[TokenType::BracketClose], "a closing bracket (']')")?;
                self.try_next(strings, lexer)?;
                let array_tidx = types.insert_array(element_type);
                Ok(types.insert_group(&[
                    Type::Array(array_tidx)
                ]))
            }
            _ => {
                Err(Error::new([
                    ErrorSection::Error(ErrorType::UnexpectedToken("the name of a type, an opening brace ('{') or an opening bracket ('[')", self.current.token_content)),
                    ErrorSection::Code(self.current.source)
                ].into()))
            }
        }
    }

    fn parse_path(
        &mut self,
        strings: &mut StringMap,
        lexer: &mut Lexer,
        expected: &'static str
    ) -> Result<NamespacePath, Error> {
        let mut segments = Vec::new();
        self.expect_type(&[TokenType::Identifier], expected)?;
        segments.push(self.current.token_content);
        while self.try_next(strings, lexer)? && self.current.token_type == TokenType::DoubleColon {
            self.expect_next(strings, lexer, expected)?;
            self.expect_type(&[TokenType::Identifier], expected)?;
            segments.push(self.current.token_content);
        }
        Ok(NamespacePath::new(segments))
    }
}