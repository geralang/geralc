
#![allow(dead_code)]

mod util;
mod cli;
mod frontend;
mod backend;

use util::{strings::{StringMap, StringIdx}, error::{Error, ErrorSection, ErrorType}, source::HasSource};
use frontend::{
    lexer::Lexer,
    parser::Parser,
    ast::{HasAstNodeVariant, AstNode, AstNodeVariant, TypedAstNode},
    grammar_checking::{check_grammar, ScopeType},
    modules::{Module, NamespacePath},
    type_checking::{type_check_modules, Symbol}, external::ExternalMappingParser
};
use cli::{CliArgs, CliArg};
use backend::interpreter::Interpreter;

use std::{fs, env, collections::HashMap};


fn main() {
    if cfg!(target_os = "windows") {
        use windows::Win32::System::Console;
        unsafe {
            let output = Console::GetStdHandle(Console::STD_OUTPUT_HANDLE)
                .expect("Failed to get console handle");
            let mut console_mode: Console::CONSOLE_MODE = Default::default();
            Console::GetConsoleMode(output, &mut console_mode)
                .expect("Failed to get console mode");
            console_mode |= Console::ENABLE_VIRTUAL_TERMINAL_PROCESSING;
            Console::SetConsoleMode(output, console_mode)
                .expect("Failed to set console mode");
        }
    }
    let mut strings = StringMap::new();
    // parse cli args
    const CLI_ARG_OUTPUT: CliArg = CliArg::required("output", 1);
    let args = match CliArgs::parse(&[
        //CLI_ARG_OUTPUT
    ], &env::args().collect::<Vec<String>>()[1..]) {
        Ok(args) => args,
        Err(error) => {
            println!("{}", error.display(&strings));
            return;
        }
    };
    // read all files 
    let mut modules = HashMap::new();
    let mut typed_symbols = HashMap::new();
    let mut external_backings = HashMap::new();
    let mut errored = false;
    for file_name in args.free_values() {
        match load_file(strings.insert(file_name), &mut strings, &mut modules, &mut typed_symbols, &mut external_backings) {
            Ok(_) => {}
            Err(errors) => {
                for error in errors { println!("{}", error.display(&strings)); }
                errored = true;
            }
        }
    }
    if errored { return; }
    // canonicalize modules
    let module_paths = modules.keys().map(|p| p.clone()).collect::<Vec<NamespacePath>>();
    for module_path in module_paths {
        let mut module = modules.remove(&module_path).expect("key must be valid");
        let canonicalization_errors = module.canonicalize(&modules, &mut strings);
        modules.insert(module_path, module);
        if canonicalization_errors.len() > 0 {
            for error in canonicalization_errors { println!("{}", error.display(&strings)); }
            return;
        }
    }
    // type check
    match type_check_modules(modules, &strings, &mut typed_symbols) {
        Ok(typed_ast) => typed_ast,
        Err(errors) => {
            for error in errors { println!("{}", error.display(&strings)); }
            return;
        }
    };
    // evaluate constants
    let mut interpreter = Interpreter::new();
    for (symbol_path, symbol_value) in &typed_symbols {
        if let Symbol::Constant { value, value_types: _ } = symbol_value {
            let value = if let Some(value) = interpreter.get_constant_value(symbol_path) {
                Some(value.clone())
            } else if let Some(value) = value {
                Some(match interpreter.evaluate_node(value, &typed_symbols, &strings) {
                    Ok(value) => value,
                    Err(error) => {
                        println!("{}", error.display(&strings));
                        return;
                    }
                })
            } else {
                None
            };
            println!("{} = {:?}", symbol_path.display(&strings), value);
        }
    }
}

fn load_file(
    file_path: StringIdx,
    strings: &mut StringMap,
    modules: &mut HashMap<NamespacePath, Module<AstNode>>,
    typed_symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    external_backings: &mut HashMap<NamespacePath, StringIdx>
) -> Result<(), Vec<Error>> {
    // read the file
    let file_content = match fs::read_to_string(strings.get(file_path)) {
        Ok(content) => strings.insert(&content),
        Err(error) => return Err(vec![Error::new([
            ErrorSection::Error(ErrorType::FileSystemError(error.to_string())),
            ErrorSection::Info(format!("While trying to read '{}'", strings.get(file_path)))
        ].into())]),
    };
    if strings.get(file_path).ends_with(".gera") {
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
        if let Some(module) = modules.get_mut(&module_path) {
            if !module.is_raw() {
                return Err(vec![Error::new([
                    ErrorSection::Error(ErrorType::ModuleAlreadyDefined(module_path.display(strings))),
                    ErrorSection::Code(module_declaration.source()),
                    ErrorSection::Info(format!("'{}' is already defined in the file '{}'", module_path.display(strings), strings.get(module.file_name())))
                ].into())]);
            }
            module.bake(module_declaration.source().file_name(), nodes, strings)?;
        } else {
            modules.insert(
                module_path.clone(),
                Module::new(module_path.clone(), module_declaration.source().file_name(), nodes, strings)?
            );
        }
        Ok(())
    } else if strings.get(file_path).ends_with(".gem") {
        // parse the file
        let mut lexer = Lexer::new(file_path, file_content, strings);
        match ExternalMappingParser::new(strings, &mut lexer) {
            None => Ok(()),
            Some(Err(error)) => Err(vec![error]),
            Some(Ok(mut parser)) => if let Err(error) = 
                    parser.parse_header(strings, &mut lexer, modules, typed_symbols, external_backings) {
                Err(vec![error])
            } else {
                Ok(())
            }
        }
    } else {
        Err(vec![Error::new([
            ErrorSection::Error(ErrorType::InvalidFileExtension(String::from(strings.get(file_path)))),
            ErrorSection::Help(String::from("Valid extensions are:\n'.gera' - Gera source file\n'.gem' - Gera external mappings file"))
        ].into())])
    }
    
}