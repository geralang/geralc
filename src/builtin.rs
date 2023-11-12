use std::collections::HashMap;

use crate::process_file;
use crate::util::{
    strings::{StringMap, StringIdx},
    source::SourceRange
};
use crate::frontend::{
    modules::{NamespacePath, Module},
    ast::{AstNode, TypedAstNode},
    types::{TypeScope, VarTypeIdx, Type},
    type_checking::Symbol
};

fn register_foreign_builtin(
    procedure_path: NamespacePath,
    parameter_names: &[&'static str],
    parameter_types: Vec<VarTypeIdx>,
    return_type: VarTypeIdx,
    strings: &mut StringMap,
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
    let builtin_str = strings.insert("<builtin>");
    typed_symbols.insert(procedure_path.clone(), Symbol::Procedure {
        parameter_names: parameter_names.iter().map(|p| strings.insert(p)).collect(),
        parameter_types: parameter_types,
        returns: return_type,
        body: None,
        source: SourceRange::new(builtin_str, builtin_str, 0, 0)
    });
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
        let array_element_type = type_scope.register_variable();
        let t = type_scope.register_with_types(Some(vec![
            Type::Object(HashMap::new(), false),
            Type::Array(array_element_type),
            Type::String
        ]));
        register_foreign_builtin(
            path_from(&["core", "addr_eq"], strings),
            &["a", "b"],
            vec![t, t],
            type_scope.register_with_types(Some(vec![Type::Boolean])),
            strings, modules, typed_symbols
        );
    }
    {
        let t = type_scope.register_with_types(Some(vec![
            Type::Variants(HashMap::new(), false)
        ]));
        register_foreign_builtin(
            path_from(&["core", "tag_eq"], strings),
            &["a", "b"],
            vec![t, t],
            type_scope.register_with_types(Some(vec![Type::Boolean])),
            strings, modules, typed_symbols
        );
    }
    {
        let array_element_type = type_scope.register_variable();
        register_foreign_builtin(
            path_from(&["core", "length"], strings),
            &["thing"],
            vec![
                type_scope.register_with_types(Some(vec![
                    Type::String,
                    Type::Array(array_element_type)
                ])),
            ],
            type_scope.register_with_types(Some(vec![Type::Integer])),
            strings, modules, typed_symbols
        );
    }
    {
        let array_element_types = type_scope.register_variable();
        register_foreign_builtin(
            path_from(&["core", "array"], strings),
            &["value", "size"],
            vec![
                array_element_types,
                type_scope.register_with_types(Some(vec![Type::Integer]))
            ],
            type_scope.register_with_types(Some(vec![
                Type::Array(array_element_types)
            ])),
            strings, modules, typed_symbols
        );
    }
    { 
        let end = type_scope.register_variable();
        let next = type_scope.register_variable();
        let exhausted_clore_return_types = type_scope.register_with_types(Some(vec![
            Type::Variants([
                (strings.insert("end"), end),
                (strings.insert("next"), next)
            ].into(), true)
        ]));
        register_foreign_builtin(
            path_from(&["core", "exhaust"], strings),
            &["iter"],
            vec![
                type_scope.register_with_types(Some(vec![
                    Type::Closure(
                        vec![],
                        exhausted_clore_return_types,
                        None
                    )
                ]))
            ],
            type_scope.register_with_types(Some(vec![Type::Unit])),
            strings, modules, typed_symbols
        );
    }
    {
        register_foreign_builtin(
            path_from(&["core", "panic"], strings),
            &["message"],
            vec![
                type_scope.register_with_types(Some(vec![Type::String]))
            ],
            type_scope.register_with_types(Some(vec![Type::Panic])),
            strings, modules, typed_symbols
        );
    }
    {
        register_foreign_builtin(
            path_from(&["core", "as_str"], strings),
            &["thing"],
            vec![
                type_scope.register_variable()
            ],
            type_scope.register_with_types(Some(vec![Type::String])),
            strings, modules, typed_symbols
        );
    }
    {
        register_foreign_builtin(
            path_from(&["core", "as_int"], strings),
            &["number"],
            vec![
                type_scope.register_with_types(Some(vec![Type::Integer, Type::Float]))
            ],
            type_scope.register_with_types(Some(vec![Type::Integer])),
            strings, modules, typed_symbols
        );
    }
    {
        register_foreign_builtin(
            path_from(&["core", "as_flt"], strings),
            &["number"],
            vec![
                type_scope.register_with_types(Some(vec![Type::Integer, Type::Float]))
            ],
            type_scope.register_with_types(Some(vec![Type::Float])),
            strings, modules, typed_symbols
        );
    }
    {
        let s = type_scope.register_with_types(Some(vec![Type::String]));
        let i = type_scope.register_with_types(Some(vec![Type::Integer]));
        register_foreign_builtin(
            path_from(&["core", "substring"], strings),
            &["source", "start", "end"],
            vec![s, i, i],
            s,
            strings, modules, typed_symbols
        );
    }
    {
        let s = type_scope.register_with_types(Some(vec![Type::String]));
        register_foreign_builtin(
            path_from(&["core", "concat"], strings),
            &["string_a", "string_b"],
            vec![s, s],
            s,
            strings, modules, typed_symbols
        );
    }
    {
        let i = type_scope.register_with_types(Some(vec![Type::Integer]));
        let u = type_scope.register_with_types(Some(vec![Type::Unit]));
        register_foreign_builtin(
            path_from(&["core", "parse_int"], strings),
            &["source"],
            vec![
                type_scope.register_with_types(Some(vec![Type::String]))
            ],
            type_scope.register_with_types(Some(vec![Type::Variants([
                (strings.insert("some"), i),
                (strings.insert("none"), u)
            ].into(), true)])),
            strings, modules, typed_symbols
        );
    }
    {
        let f = type_scope.register_with_types(Some(vec![Type::Float]));
        let u = type_scope.register_with_types(Some(vec![Type::Unit]));
        register_foreign_builtin(
            path_from(&["core", "parse_flt"], strings),
            &["source"],
            vec![
                type_scope.register_with_types(Some(vec![Type::String]))
            ],
            type_scope.register_with_types(Some(vec![Type::Variants([
                (strings.insert("some"), f),
                (strings.insert("none"), u)
            ].into(), true)])),
            strings, modules, typed_symbols
        );
    }
    {
        register_foreign_builtin(
            path_from(&["core", "string"], strings),
            &["repeated", "times"],
            vec![
                type_scope.register_with_types(Some(vec![Type::String])),
                type_scope.register_with_types(Some(vec![Type::Integer]))
            ],
            type_scope.register_with_types(Some(vec![Type::String])),
            strings, modules, typed_symbols
        );
    }
}

fn load_native_builtins(
    strings: &mut StringMap,
    modules: &mut HashMap<NamespacePath, Module<AstNode>>,
    type_scope: &mut TypeScope,
    typed_symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    external_backings: &mut HashMap<NamespacePath, StringIdx>
) {
    let src = strings.insert(include_str!("core.gera"));
    let file = strings.insert("<builtin>/core.gera");
    if let Err(errors) = process_file(file, src, strings, modules, type_scope, typed_symbols, external_backings) {
        for error in errors {
            println!("{}", error.display(strings));
        }
        panic!("builtin file is invalid");
    }
}