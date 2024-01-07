use std::collections::HashMap;

use crate::process_file;
use crate::util::{
    strings::{StringMap, StringIdx},
    source::SourceRange
};
use crate::frontend::{
    modules::{NamespacePath, Module},
    ast::{AstNode, TypedAstNode},
    types::{TypeScope, TypeGroup, Type},
    type_checking::Symbol
};

fn register_foreign_builtin(
    procedure_path: NamespacePath,
    parameter_names: &[&'static str],
    parameter_types: Vec<TypeGroup>,
    return_type: TypeGroup,
    type_scope: TypeScope,
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
        public: true,
        parameter_names: parameter_names.iter().map(|p| strings.insert(p)).collect(),
        parameter_types: parameter_types,
        returns: return_type,
        body: None,
        source: SourceRange::new(builtin_str, builtin_str, 0, 0),
        type_scope
    });
}

fn path_from(segments: &[&'static str], strings: &mut StringMap) -> NamespacePath {
    NamespacePath::new(segments.iter().map(|s| strings.insert(s)).collect())
}

pub fn load_builtins(
    target_str: &str,
    strings: &mut StringMap,
    modules: &mut HashMap<NamespacePath, Module<AstNode>>,
    global_type_scope: &mut TypeScope,
    typed_symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    external_backings: &mut HashMap<NamespacePath, StringIdx>
) {
    load_foreign_builtins(strings, modules, typed_symbols);
    load_native_builtins(target_str, strings, modules, global_type_scope, typed_symbols, external_backings);
}

fn load_foreign_builtins(
    strings: &mut StringMap,
    modules: &mut HashMap<NamespacePath, Module<AstNode>>,
    typed_symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>
) {
    {
        let mut type_scope = TypeScope::new();
        let array_element_type = type_scope.insert_group(&[Type::Any]);
        let object_tidx = type_scope.insert_object(HashMap::new(), false);
        let array_tidx = type_scope.insert_array(array_element_type);
        let t = type_scope.insert_group(&[
            Type::Object(object_tidx),
            Type::Array(array_tidx)
        ]);
        register_foreign_builtin(
            path_from(&["core", "addr_eq"], strings),
            &["a", "b"],
            vec![t, t],
            type_scope.insert_group(&[Type::Boolean]),
            type_scope, strings, modules, typed_symbols
        );
    }
    {
        let mut type_scope = TypeScope::new();
        let var_tidx = type_scope.insert_variants(HashMap::new(), false);
        let t = type_scope.insert_group(&[
            Type::Variants(var_tidx)
        ]);
        register_foreign_builtin(
            path_from(&["core", "tag_eq"], strings),
            &["a", "b"],
            vec![t, t],
            type_scope.insert_group(&[Type::Boolean]),
            type_scope, strings, modules, typed_symbols
        );
    }
    {
        let mut type_scope = TypeScope::new();
        let array_element_type = type_scope.insert_group(&[Type::Any]);
        let array_tidx = type_scope.insert_array(array_element_type);
        register_foreign_builtin(
            path_from(&["core", "length"], strings),
            &["thing"],
            vec![
                type_scope.insert_group(&[
                    Type::String,
                    Type::Array(array_tidx)
                ]),
            ],
            type_scope.insert_group(&[Type::Integer]),
            type_scope, strings, modules, typed_symbols
        );
    }
    {
        let mut type_scope = TypeScope::new();
        let array_element_types = type_scope.insert_group(&[Type::Any]);
        let array_tidx = type_scope.insert_array(array_element_types);
        register_foreign_builtin(
            path_from(&["core", "array"], strings),
            &["value", "size"],
            vec![
                array_element_types,
                type_scope.insert_group(&[Type::Integer])
            ],
            type_scope.insert_group(&[
                Type::Array(array_tidx)
            ]),
            type_scope, strings, modules, typed_symbols
        );
    }
    { 
        let mut type_scope = TypeScope::new();
        let end = type_scope.insert_group(&[Type::Any]);
        let next = type_scope.insert_group(&[Type::Any]);
        let var_tidx = type_scope.insert_variants(
            [
                (strings.insert("end"), end),
                (strings.insert("next"), next)
            ].into(),
            true
        );
        let exhausted_clore_return_types = type_scope.insert_group(&[
            Type::Variants(var_tidx)
        ]);
        let closure_tidx = type_scope.insert_closure(
            vec![],
            exhausted_clore_return_types,
            None
        );
        register_foreign_builtin(
            path_from(&["core", "exhaust"], strings),
            &["iter"],
            vec![
                type_scope.insert_group(&[
                    Type::Closure(closure_tidx)
                ])
            ],
            type_scope.insert_group(&[Type::Unit]),
            type_scope, strings, modules, typed_symbols
        );
    }
    {
        let mut type_scope = TypeScope::new();
        register_foreign_builtin(
            path_from(&["core", "panic"], strings),
            &["message"],
            vec![
                type_scope.insert_group(&[Type::String])
            ],
            type_scope.insert_group(&[Type::Any]),
            type_scope, strings, modules, typed_symbols
        );
    }
    {
        let mut type_scope = TypeScope::new();
        register_foreign_builtin(
            path_from(&["core", "as_str"], strings),
            &["thing"],
            vec![
                type_scope.insert_group(&[Type::Any])
            ],
            type_scope.insert_group(&[Type::String]),
            type_scope, strings, modules, typed_symbols
        );
    }
    {
        let mut type_scope = TypeScope::new();
        register_foreign_builtin(
            path_from(&["core", "as_int"], strings),
            &["number"],
            vec![
                type_scope.insert_group(&[Type::Integer, Type::Float])
            ],
            type_scope.insert_group(&[Type::Integer]),
            type_scope, strings, modules, typed_symbols
        );
    }
    {
        let mut type_scope = TypeScope::new();
        register_foreign_builtin(
            path_from(&["core", "as_flt"], strings),
            &["number"],
            vec![
                type_scope.insert_group(&[Type::Integer, Type::Float])
            ],
            type_scope.insert_group(&[Type::Float]),
            type_scope, strings, modules, typed_symbols
        );
    }
    {
        let mut type_scope = TypeScope::new();
        let s = type_scope.insert_group(&[Type::String]);
        let i = type_scope.insert_group(&[Type::Integer]);
        register_foreign_builtin(
            path_from(&["core", "substring"], strings),
            &["source", "start", "end"],
            vec![s, i, i],
            s,
            type_scope, strings, modules, typed_symbols
        );
    }
    {
        let mut type_scope = TypeScope::new();
        let s = type_scope.insert_group(&[Type::String]);
        register_foreign_builtin(
            path_from(&["core", "concat"], strings),
            &["string_a", "string_b"],
            vec![s, s],
            s,
            type_scope, strings, modules, typed_symbols
        );
    }
    {
        let mut type_scope = TypeScope::new();
        let i = type_scope.insert_group(&[Type::Integer]);
        let u = type_scope.insert_group(&[Type::Unit]);
        let var_tidx = type_scope.insert_variants(
            [
                (strings.insert("some"), i),
                (strings.insert("none"), u)
            ].into(),
            true
        );
        register_foreign_builtin(
            path_from(&["core", "parse_int"], strings),
            &["source"],
            vec![
                type_scope.insert_group(&[Type::String])
            ],
            type_scope.insert_group(&[Type::Variants(var_tidx)]),
            type_scope, strings, modules, typed_symbols
        );
    }
    {
        let mut type_scope = TypeScope::new();
        let f = type_scope.insert_group(&[Type::Float]);
        let u = type_scope.insert_group(&[Type::Unit]);
        let var_tidx = type_scope.insert_variants(
            [
                (strings.insert("some"), f),
                (strings.insert("none"), u)
            ].into(),
            true
        );
        register_foreign_builtin(
            path_from(&["core", "parse_flt"], strings),
            &["source"],
            vec![
                type_scope.insert_group(&[Type::String])
            ],
            type_scope.insert_group(&[Type::Variants(var_tidx)]),
            type_scope, strings, modules, typed_symbols
        );
    }
    {
        let mut type_scope = TypeScope::new();
        register_foreign_builtin(
            path_from(&["core", "string"], strings),
            &["repeated", "times"],
            vec![
                type_scope.insert_group(&[Type::String]),
                type_scope.insert_group(&[Type::Integer])
            ],
            type_scope.insert_group(&[Type::String]),
            type_scope, strings, modules, typed_symbols
        );
    }
    {
        let mut type_scope = TypeScope::new();
        register_foreign_builtin(
            path_from(&["core", "hash"], strings),
            &["value"],
            vec![
                type_scope.insert_group(&[Type::Any]),

            ],
            type_scope.insert_group(&[Type::Integer]),
            type_scope, strings, modules, typed_symbols
        );
    }
}

fn load_native_builtins(
    target_str: &str,
    strings: &mut StringMap,
    modules: &mut HashMap<NamespacePath, Module<AstNode>>,
    global_type_scope: &mut TypeScope,
    typed_symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    external_backings: &mut HashMap<NamespacePath, StringIdx>
) {
    let src = strings.insert(include_str!("core.gera"));
    let file = strings.insert("<builtin>/core.gera");
    if let Err(errors) = process_file(file, src, target_str, strings, modules, global_type_scope, typed_symbols, external_backings) {
        for error in errors {
            println!("{}", error.display(strings, false));
        }
        panic!("builtin file is invalid");
    }
}