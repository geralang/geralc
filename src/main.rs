
#![allow(dead_code)]

mod util;
mod compiler;

use util::{strings::{StringMap, StringIdx}, error::{Error, ErrorSection, ErrorType}, source::HasSource};
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
    let mut module_path: NamespacePath = NamespacePath::new(Vec::new());
    if nodes.len() == 0 || match nodes[0].node_variant() {
        AstNodeVariant::Module { path } => {
            module_path = path.clone();
            false
        }
        _ => true
    } { return Err(vec![Error::new([
        ErrorSection::Error(ErrorType::ModuleDeclarationNotAtTop),
        ErrorSection::Info(format!("In the file '{}'", strings.get(file_path))),
    ].into())]) }
    let module_declaration = nodes.remove(0);
    if let Some(module) = modules.get(&module_path) {
        return Err(vec![Error::new([
            ErrorSection::Error(ErrorType::ModuleAlreadyDefined(module_path.display(strings))),
            ErrorSection::Code(module_declaration.source()),
            ErrorSection::Info(format!("'{}' is already defined in the file '{}'", module_path.display(strings), strings.get(module.file_name())))
        ].into())]);
    } else {
        modules.insert(
            module_path.clone(),
            match Module::new(module_path.clone(), module_declaration.source().file_name(), nodes, strings) {
                Ok(module) => module,
                Err(errors) => return Err(errors)
            }
        );
    }
    Ok(module_path)
}

fn main() {
    let mut strings = StringMap::new();
    let mut modules = HashMap::new();
    // load files (CLI will do this later on)
    if match load_file(strings.insert("test.gera"), &mut strings, &mut modules) {
        Ok(_) => { false }
        Err(errors) => {
            for error in errors { println!("{}\n", error.display(&strings)); }
            true
        }
    } || match load_file(strings.insert("test2.gera"), &mut strings, &mut modules) {
        Ok(_) => { false }
        Err(errors) => {
            for error in errors { println!("{}\n", error.display(&strings)); }
            true
        }
    } { return; }
    // canonicalize modules
    let module_paths = modules.keys().map(|p| p.clone()).collect::<Vec<NamespacePath>>();
    for module_path in module_paths {
        let mut module = modules.remove(&module_path).expect("key must be valid");
        let canonicalization_errors = module.canonicalize(&modules, &mut strings);
        modules.insert(module_path, module);
        if canonicalization_errors.len() > 0 {
            for error in canonicalization_errors { println!("{}\n", error.display(&strings)); }
            return;
        }
    }
    // debug
    println!("{:?}", modules);
}

