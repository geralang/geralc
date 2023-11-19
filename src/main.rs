
#![allow(dead_code)]

mod cli;
mod builtin;
mod util;
mod frontend;
mod backend;

use util::{strings::{StringMap, StringIdx}, error::{Error, ErrorSection, ErrorType}, source::HasSource};
use cli::{CliArgs, CliArg};
use builtin::load_builtins;
use frontend::{
    lexer::Lexer,
    parser::Parser,
    ast::{HasAstNodeVariant, AstNode, AstNodeVariant, TypedAstNode},
    grammar_checking::{check_grammar, ScopeType},
    modules::{Module, NamespacePath},
    type_checking::{type_check_modules, Symbol}, external::ExternalMappingParser,
    types::TypeScope
};
use backend::{
    lowering::lower_typed_ast,
    target::CompileTarget,
    c::generate_c,
    javascript::generate_javascript,
    symbols::generate_symbols
};

use std::{fs, env, collections::HashMap};

fn handle_error(error: &Error, strings: &StringMap) -> ! {
    println!("{}", error.display(strings));
    std::process::exit(1);
}

fn handle_errors(errors: &[Error], strings: &StringMap) -> ! {
    for error in errors {
        println!("{}", error.display(strings));
    }
    std::process::exit(1);
}

fn main() {
    #[cfg(target_os = "windows")] {
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
    const CLI_ARG_MAIN: CliArg = CliArg::optional("m", "specifies the path of the main procedure", 1);
    const CLI_ARG_TARGET: CliArg = CliArg::required("t", "specifies the target format", 1);
    const CLI_ARG_OUTPUT: CliArg = CliArg::required("o", "specifies the output file", 1);
    let args = match CliArgs::parse(&[
        CLI_ARG_MAIN,
        CLI_ARG_TARGET,
        CLI_ARG_OUTPUT
    ], &env::args().collect::<Vec<String>>()[1..]) {
        Ok(args) => args,
        Err(error) => handle_error(&error, &strings)
    };
    let target_str = args.values(CLI_ARG_TARGET)
        .expect("is required")
        .last()
        .expect("is required to have one value")
        .clone();
    let targets: HashMap<String, CompileTarget> = HashMap::from([
        ("c".into(), CompileTarget::IrConsumer(generate_c)),
        ("js".into(), CompileTarget::IrConsumer(generate_javascript)),
        ("symbols".into(), CompileTarget::TypedAstConsumer(generate_symbols))
    ]);
    let selected_target = targets.get(&target_str).unwrap_or_else(|| handle_error(&Error::new([
        ErrorSection::Error(ErrorType::InvalidCompileTarget(target_str.clone())),
        ErrorSection::Help(format!("List of available targets: {}", targets.iter().map(|t| format!("\n- {}", t.0)).collect::<Vec<String>>().join("")))
    ].into()), &strings));
    let output_file = args.values(CLI_ARG_OUTPUT)
        .expect("is required")
        .last()
        .expect("is required to have one value");
    // load builtins
    let mut modules = HashMap::new();
    let mut type_scope = TypeScope::new();
    let mut typed_symbols = HashMap::new();
    let mut external_backings = HashMap::new();
    load_builtins(&mut strings, &mut modules, &mut type_scope, &mut typed_symbols, &mut external_backings);
    // read all files
    let mut file_read_errors = Vec::new();
    for file_name in args.free_values() {
        match read_file(strings.insert(file_name), &mut strings, &mut modules, &mut type_scope, &mut typed_symbols, &mut external_backings) {
            Ok(_) => {}
            Err(mut errors) => file_read_errors.append(&mut errors)
        }
    }
    if file_read_errors.len() > 0 { handle_errors(&file_read_errors, &strings) }
    //println!("parsing done");
    // canonicalize modules
    let module_paths = modules.keys().map(|p| p.clone()).collect::<Vec<NamespacePath>>();
    for module_path in module_paths {
        let mut module = modules.remove(&module_path).expect("key must be valid");
        let canonicalization_errors = module.canonicalize(&modules, &mut strings);
        modules.insert(module_path, module);
        if canonicalization_errors.len() > 0 { handle_errors(&canonicalization_errors, &strings); }
    }
    //println!("canonicalization done");
    // if target consumes AST, pass it the typed AST and write the result
    if let CompileTarget::AstConsumer(generator) = selected_target {
        write_file(output_file, (generator)(type_scope, modules, external_backings, &mut strings), &strings);
        return;
    }
    // type check
    match type_check_modules(modules, &strings, &mut type_scope, &mut typed_symbols) {
        Ok(_) => {}
        Err(errors) => handle_errors(&errors, &strings)
    };
    //println!("type checking done");
    // if target consumes typed AST, pass it the typed AST and write the result
    if let CompileTarget::TypedAstConsumer(generator) = selected_target {
        write_file(output_file, (generator)(type_scope, typed_symbols, external_backings, &mut strings), &strings);
        return;
    }
    // find main procedure
    let main_procedure_path = NamespacePath::new(
        args.values(CLI_ARG_MAIN)
            .unwrap_or_else(|| handle_error(&Error::new([
                ErrorSection::Error(ErrorType::NoMainProcedureDefined(target_str))
            ].into()), &strings))
            .last()
            .expect("is required to have one value")
            .split("::")
            .map(|e| strings.insert(e))
            .collect::<Vec<StringIdx>>()
    );
    let main_procedure = if let Some(symbol)
        = typed_symbols
            .get(&main_procedure_path)
            .map(|s| {
                if let Symbol::Procedure { 
                    parameter_names,
                    parameter_types,
                    body, ..
                } = s {
                    if parameter_names.len() == 0
                    && parameter_types.len() == 0
                    && body.is_some() { Some(s) }
                    else { None }
                } else { None }
            })
            .flatten() {
        symbol
    } else { handle_error(&Error::new([
        ErrorSection::Error(ErrorType::InvalidMainProcedure(main_procedure_path.display(&strings))),
        ErrorSection::Help(String::from("The main procedure needs to be a procedure without any arguments."))
    ].into()), &strings) };
    // lower typed AST
    let (ir_symbols, ir_types) = match lower_typed_ast(
         &mut strings, &mut type_scope, &typed_symbols, &external_backings,
         (&main_procedure_path, main_procedure)
    ) {
        Ok(ir) => ir,
        Err(error) => {
            println!("{}", error.display(&strings));
            std::process::exit(1);
        }
    };
    let ir_type_deduplication = ir_types.deduplicate();
    println!("{}", ir_types.display(&strings));
    println!("{:?}", ir_type_deduplication);
    //println!("lowering done");
    // if target consumes IR, pass it the IR and write the result
    if let CompileTarget::IrConsumer(generator) = selected_target {
        write_file(output_file, (generator)(ir_symbols, ir_types, ir_type_deduplication, main_procedure_path, &mut strings), &strings);
        return;
    }
    // done!
}

pub fn read_file(
    file_path: StringIdx,
    strings: &mut StringMap,
    modules: &mut HashMap<NamespacePath, Module<AstNode>>,
    type_scope: &mut TypeScope,
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
    // process the file
    process_file(file_path, file_content, strings, modules, type_scope, typed_symbols, external_backings)
}

pub fn process_file(
    file_path: StringIdx,
    file_content: StringIdx,
    strings: &mut StringMap,
    modules: &mut HashMap<NamespacePath, Module<AstNode>>,
    type_scope: &mut TypeScope,
    typed_symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    external_backings: &mut HashMap<NamespacePath, StringIdx>
) -> Result<(), Vec<Error>> {
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
                    parser.parse_header(strings, &mut lexer, modules, type_scope, typed_symbols, external_backings) {
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

pub fn write_file(path: &String, content: String, strings: &StringMap) {
    if let Err(error) = fs::write(path, content) {
        handle_error(&Error::new([
            ErrorSection::Error(ErrorType::FileSystemError(error.to_string())),
            ErrorSection::Info(format!("While trying to write to '{}'", path))
        ].into()), strings);
    }
}