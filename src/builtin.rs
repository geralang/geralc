use std::collections::HashMap;

use crate::process_file;
use crate::util::strings::{StringMap, StringIdx};
use crate::frontend::{
    modules::{NamespacePath, Module},
    ast::{AstNode, TypedAstNode},
    types::{TypeScope, VarTypeIdx, PossibleTypes, Type},
    type_checking::Symbol
};

fn register_foreign_builtin(
    procedure_path: NamespacePath,
    parameter_types: Vec<VarTypeIdx>,
    return_type: VarTypeIdx,
    modules: &mut HashMap<NamespacePath, Module<AstNode>>,
    typed_symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
) {
    let procedure_module_path = NamespacePath::new(procedure_path.get_segments()[0..procedure_path.get_segments().len() - 1].into());
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
    typed_symbols.insert(procedure_path.clone(), Symbol::Procedure {
        parameter_names: Vec::new(),
        parameter_types: parameter_types,
        returns: return_type,
        body: None
    });
}

fn register_type(possible_types: &PossibleTypes, type_scope: &mut TypeScope) -> VarTypeIdx {
    let group = type_scope.register_variable();
    type_scope.limit_possible_types(&PossibleTypes::OfGroup(group), possible_types)
        .expect("should be a valid type");
    group
}

fn path_from(segments: &[&'static str], strings: &mut StringMap) -> NamespacePath {
    NamespacePath::new(segments.iter().map(|s| strings.insert(s)).collect())
}

pub fn load_builtins(
    strings: &mut StringMap,
    modules: &mut HashMap<NamespacePath, Module<AstNode>>,
    type_scope: &mut TypeScope,
    typed_symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    external_backings: &mut HashMap<NamespacePath, StringIdx>
) {
    load_foreign_builtins(strings, modules, type_scope, typed_symbols);
    load_native_builtins(strings, modules, type_scope, typed_symbols, external_backings);
}

fn load_foreign_builtins(
    strings: &mut StringMap,
    modules: &mut HashMap<NamespacePath, Module<AstNode>>,
    type_scope: &mut TypeScope,
    typed_symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>
) {
    {
        let t = register_type(
            &PossibleTypes::OneOf(vec![
                Type::Object(HashMap::new(), false),
                Type::Array(PossibleTypes::Any),
                Type::String
            ]),
            type_scope
        );
        register_foreign_builtin(
            path_from(&["core", "addr_eq"], strings),
            vec![
                register_type(&PossibleTypes::OfGroup(t), type_scope),
                register_type(&PossibleTypes::OfGroup(t), type_scope),
            ],
            register_type(&PossibleTypes::OneOf(vec![Type::Boolean]), type_scope),
            modules, typed_symbols
        );
    }
    {
        let t = register_type(
            &PossibleTypes::OneOf(vec![
                Type::Variants(HashMap::new(), false)
            ]),
            type_scope
        );
        register_foreign_builtin(
            path_from(&["core", "tag_eq"], strings),
            vec![
                register_type(&PossibleTypes::OfGroup(t), type_scope),
                register_type(&PossibleTypes::OfGroup(t), type_scope),
            ],
            register_type(&PossibleTypes::OneOf(vec![Type::Boolean]), type_scope),
            modules, typed_symbols
        );
    }
    register_foreign_builtin(
        path_from(&["core", "length"], strings),
        vec![
            register_type(&PossibleTypes::OneOf(vec![
                Type::String,
                Type::Array(PossibleTypes::Any)
            ]), type_scope),
        ],
        register_type(&PossibleTypes::OneOf(vec![Type::Integer]), type_scope),
        modules, typed_symbols
    );
    register_foreign_builtin(
        path_from(&["core", "exhaust"], strings),
        vec![
            register_type(&PossibleTypes::OneOf(vec![
                Type::Closure(
                    vec![],
                    register_type(&PossibleTypes::OneOf(vec![
                        Type::Variants([
                            (strings.insert("end"), PossibleTypes::Any),
                            (strings.insert("next"), PossibleTypes::Any)
                        ].into(), true)
                    ]), type_scope),
                    None
                )
            ]), type_scope)
        ],
        register_type(&PossibleTypes::OneOf(vec![Type::Unit]), type_scope),
        modules, typed_symbols
    );
}

fn load_native_builtins(
    strings: &mut StringMap,
    modules: &mut HashMap<NamespacePath, Module<AstNode>>,
    type_scope: &mut TypeScope,
    typed_symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    external_backings: &mut HashMap<NamespacePath, StringIdx>
) {
    let src = strings.insert("
mod core
    
pub proc range(start, end) {
    mut var i = start
    return || {
        case i >= end -> return #end unit
        var c = i
        i = i + 1
        return #next c
    }
}
    ");
    let file = strings.insert("<builtin>/core.gera");
    if let Err(errors) = process_file(file, src, strings, modules, type_scope, typed_symbols, external_backings) {
        for error in errors {
            println!("{}", error.display(strings));
        }
        panic!("builtin file is invalid");
    }
}