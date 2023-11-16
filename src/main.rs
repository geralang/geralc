
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
    wasm::generate_wasm
};

use std::{fs, env, collections::HashMap};

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
    const CLI_ARG_MAIN: CliArg = CliArg::required("m", "specifies the path of the main procedure", 1);
    const CLI_ARG_TARGET: CliArg = CliArg::required("t", "specifies the target format", 1);
    const CLI_ARG_OUTPUT: CliArg = CliArg::required("o", "specifies the output file", 1);
    let args = match CliArgs::parse(&[
        CLI_ARG_MAIN,
        CLI_ARG_TARGET,
        CLI_ARG_OUTPUT
    ], &env::args().collect::<Vec<String>>()[1..]) {
        Ok(args) => args,
        Err(error) => {
            println!("{}", error.display(&strings));
            std::process::exit(1);
        }
    };
    // load builtins
    let mut modules = HashMap::new();
    let mut type_scope= TypeScope::new();
    let mut typed_symbols = HashMap::new();
    let mut external_backings = HashMap::new();
    load_builtins(&mut strings, &mut modules, &mut type_scope, &mut typed_symbols, &mut external_backings);
    // read all files
    let mut errored = false;
    for file_name in args.free_values() {
        match read_file(strings.insert(file_name), &mut strings, &mut modules, &mut type_scope, &mut typed_symbols, &mut external_backings) {
            Ok(_) => {}
            Err(errors) => {
                for error in errors { println!("{}", error.display(&strings)); }
                errored = true;
            }
        }
    }
    if errored { std::process::exit(1); }
    //println!("parsing done");
    // canonicalize modules
    let module_paths = modules.keys().map(|p| p.clone()).collect::<Vec<NamespacePath>>();
    for module_path in module_paths {
        let mut module = modules.remove(&module_path).expect("key must be valid");
        let canonicalization_errors = module.canonicalize(&modules, &mut strings);
        modules.insert(module_path, module);
        if canonicalization_errors.len() > 0 {
            for error in canonicalization_errors { println!("{}", error.display(&strings)); }
            std::process::exit(1);
        }
    }
    //println!("canonicalization done");
    // type check
    match type_check_modules(modules, &strings, &mut type_scope, &mut typed_symbols) {
        Ok(_) => {}
        Err(errors) => {
            for error in errors { println!("{}", error.display(&strings)); }
            std::process::exit(1);
        }
    };
    //println!("type checking done");
    // find main procedure
    let main_procedure_path = NamespacePath::new(
        args.values(CLI_ARG_MAIN)
            .expect("is required")
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
    } else {
        println!("{}", Error::new([
            ErrorSection::Error(ErrorType::InvalidMainProcedure(main_procedure_path.display(&strings))),
            ErrorSection::Help(String::from("The main procedure needs to be a procedure without any arguments."))
        ].into()).display(&strings));
        std::process::exit(1);
    };
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
    println!("{:?}", ir_type_deduplication);
    //println!("lowering done");
    // generate file content based on format
    let targets: HashMap<String, CompileTarget> = HashMap::from([
        ("c".into(), CompileTarget(generate_c)),
        ("wasm".into(), CompileTarget(generate_wasm))
    ]);
    let selected_target = args.values(CLI_ARG_TARGET)
        .expect("is required")
        .last()
        .expect("is required to have one value");
    let output = if let Some(target) = targets.get(selected_target) {
        (target.0)(ir_symbols, ir_types, ir_type_deduplication, main_procedure_path, &mut strings)
    } else {
        println!("{}", Error::new([
            ErrorSection::Error(ErrorType::InvalidCompileTarget(selected_target.clone())),
            ErrorSection::Help(format!("List of available targets: {}", targets.iter().map(|t| format!("\n- {}", t.0)).collect::<Vec<String>>().join("")))
        ].into()).display(&strings));
        std::process::exit(1);
    };
    //println!("emitting done");
    // write to output file
    let output_file_name = args.values(CLI_ARG_OUTPUT)
        .expect("is required")
        .last()
        .expect("is required to have one value");
    if let Err(error) = fs::write(output_file_name, output) {
        println!("{}", Error::new([
            ErrorSection::Error(ErrorType::FileSystemError(error.to_string())),
            ErrorSection::Info(format!("While trying to write to '{}'", output_file_name))
        ].into()).display(&strings));
        std::process::exit(1);
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