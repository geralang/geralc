
pub mod backend;
pub mod frontend;
pub mod util;
pub mod builtin;

use util::{
    strings::{StringMap, StringIdx},
    error::{Error, ErrorSection, ErrorType},
    source::HasSource
};
use builtin::load_builtins;
use frontend::{
    lexer::Lexer,
    parser::Parser,
    ast::{HasAstNodeVariant, AstNode, AstNodeVariant, TypedAstNode},
    grammar_checking::{check_grammar, ScopeType},
    modules::{Module, NamespacePath},
    type_checking::{type_check_modules, Symbol}, external::ExternalMappingParser,
    types::TypeScope, target_macro::process_target_blocks
};
use backend::{
    lowering::lower_typed_ast,
    target::CompileTarget,
    c::generate_c,
    javascript::generate_javascript,
    symbols::generate_symbols
};

use std::collections::HashMap;

pub fn compile(
    strings: &mut StringMap,
    files: HashMap<StringIdx, StringIdx>,
    target_str: &str,
    main_proc: Option<String>
) -> Result<String, Vec<Error>> {
    let targets: HashMap<String, CompileTarget> = HashMap::from([
        ("c".into(), CompileTarget::IrConsumer(generate_c)),
        ("js".into(), CompileTarget::IrConsumer(generate_javascript)),
        ("symbols".into(), CompileTarget::TypedAstConsumer(generate_symbols))
    ]);
    let selected_target = targets.get(target_str).map(|t| Ok(t)).unwrap_or_else(|| Err(vec![Error::new([
        ErrorSection::Error(ErrorType::InvalidCompileTarget(target_str.to_string())),
        ErrorSection::Help(format!("List of available targets: {}", targets.iter().map(|t| format!("\n- {}", t.0)).collect::<Vec<String>>().join("")))
    ].into())]))?;
    // load builtins
    let mut modules = HashMap::new();
    let mut type_scope = TypeScope::new();
    let mut typed_symbols = HashMap::new();
    let mut external_backings = HashMap::new();
    load_builtins(&target_str, strings, &mut modules, &mut type_scope, &mut typed_symbols, &mut external_backings);
    // process all files
    let mut file_process_errors = Vec::new();
    for (file_name, file_content) in files {
        process_file(
            file_name, file_content, target_str,
            strings, &mut modules, &mut type_scope, &mut typed_symbols, &mut external_backings
        ).unwrap_or_else(|mut errors| file_process_errors.append(&mut errors));
    }
    if file_process_errors.len() > 0 { return Err(file_process_errors); }
    //println!("parsing done");
    // canonicalize modules
    let module_paths = modules.keys().map(|p| p.clone()).collect::<Vec<NamespacePath>>();
    for module_path in module_paths {
        let mut module = modules.remove(&module_path).expect("key must be valid");
        let canonicalization_errors = module.canonicalize(&modules, strings);
        modules.insert(module_path, module);
        if canonicalization_errors.len() > 0 { return Err(canonicalization_errors) }
    }
    //println!("canonicalization done");
    // if target consumes AST, pass it the typed AST and return the result
    if let CompileTarget::AstConsumer(generator) = selected_target {
        return Ok((generator)(type_scope, modules, external_backings, strings));
    }
    // type check
    type_check_modules(modules, &strings, &mut type_scope, &mut typed_symbols)?;
    //println!("type checking done");
    // if target consumes typed AST, pass it the typed AST and return the result
    if let CompileTarget::TypedAstConsumer(generator) = selected_target {
        return Ok((generator)(type_scope, typed_symbols, external_backings, strings));
    }
    // find main procedure
    let main_proc = main_proc.map(|p| Ok(p)).unwrap_or_else(|| Err(vec![Error::new([
            ErrorSection::Error(ErrorType::NoMainProcedureDefined(target_str.to_string()))
        ].into())]))?;
    let main_procedure_path = NamespacePath::new(
        main_proc.split("::").map(|e| strings.insert(e)).collect::<Vec<StringIdx>>()
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
    } else { return Err(vec![Error::new([
        ErrorSection::Error(ErrorType::InvalidMainProcedure(main_procedure_path.display(&strings))),
        ErrorSection::Help(String::from("The main procedure needs to be a procedure without any arguments."))
    ].into())]); };
    // lower typed AST
    let ir_symbols = lower_typed_ast(
        strings, &mut type_scope, &typed_symbols, &external_backings,
        (&main_procedure_path, main_procedure)
    ).map_err(|e| vec![e])?;
    //println!("lowering done");
    // if target consumes IR, pass it the IR and return the result
    if let CompileTarget::IrConsumer(generator) = selected_target {
        return Ok((generator)(ir_symbols, type_scope, main_procedure_path, strings));
    }
    // done!
    return Ok(String::new())
}

pub fn process_file(
    file_path: StringIdx,
    file_content: StringIdx,
    target_str: &str,
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
        // expand target macrocs
        process_target_blocks(&mut nodes, target_str, strings);
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