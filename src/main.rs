
#![allow(dead_code)]

mod util;
mod compiler;

use util::{strings::{StringMap, StringIdx}, error::{Error, ErrorSection, ErrorType}};
use compiler::{lexer::Lexer, parser::Parser, ast::{HasAstNodeVariant, AstNode, AstNodeVariant}, grammar::{check_grammar, ScopeType}, modules::{Module, NamespacePath}};

use std::{fs, collections::HashMap};

fn load_file(file_path: StringIdx, strings: &mut StringMap, modules: &mut HashMap<NamespacePath, Module<AstNode>>) -> Result<NamespacePath, Vec<Error>> {
    // read the file
    let file_content = match fs::read_to_string(strings.get(file_path)) {
        Ok(content) => strings.insert(&content),
        Err(error) => return Err(vec![Error::new([
            ErrorSection::Error(ErrorType::FileSystemError(error.to_string())),
            ErrorSection::Info(format!("While trying to read '{}'", strings.get(file_path)))
        ].into())]),
    };
    // parse the file
    let mut lexer = Lexer::new(file_path, file_content, strings);
    let mut parser = match Parser::new(strings, &mut lexer) {
        None => None,
        Some(Err(error)) => { return Err(vec![error]) },
        Some(Ok(parser)) => Some(parser)
    };
    let mut nodes = if let Some(parser) = parser.as_mut() {
        match parser.parse_block(strings, &mut lexer) {
            Ok(tree) => tree,
            Err(error) => return Err(vec![error])
        }
    } else { Vec::new() };
    // check for grammar errors
    let mut grammar_errors = Vec::new();
    check_grammar(&nodes, ScopeType::GlobalStatement, &mut grammar_errors);
    if grammar_errors.len() > 0 { return Err(grammar_errors); }
    // put the file into a module
    let mut module_path = NamespacePath::new(Vec::new());
    if nodes.len() == 0 || match nodes[0].node_variant() {
        AstNodeVariant::Module { path } => { module_path = path.clone(); false }
        _ => true
    } { return Err(vec![Error::new([
        ErrorSection::Error(ErrorType::ModuleDeclarationNotAtTop),
        ErrorSection::Info(format!("In the file '{}'", strings.get(file_path)))
    ].into())]) }
    nodes.remove(0);
    if let Some(module) = modules.get_mut(&module_path) {
        let module_load_errors = module.load(nodes, strings);
        if module_load_errors.len() > 0 { return Err(module_load_errors); }
    } else {
        let mut module = Module::new(module_path.clone());
        let module_load_errors = module.load(nodes, strings);
        if module_load_errors.len() > 0 { return Err(module_load_errors); }
        modules.insert(module_path.clone(), module);
    }
    Ok(module_path)
}

fn main() {
    let mut strings = StringMap::new();
    let mut modules = HashMap::new();
    match load_file(strings.insert("test.gera"), &mut strings, &mut modules) {
        Ok(_) => {}
        Err(errors) => {
            for error in errors { println!("{}\n", error.display(&strings)); }
            return;
        }
    }
    for module in modules.keys() {
        let canonicalization_errors = Module::canonicalize(module, &modules);
        if canonicalization_errors.len() > 0 {
            for error in canonicalization_errors { println!("{}\n", error.display(&strings)); }
            return;
        }
    }
}

