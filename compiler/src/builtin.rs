use std::collections::HashMap;

use crate::process_file;
use crate::util::{
    strings::{StringMap, StringIdx},
    source::SourceRange
};
use crate::frontend::{
    modules::{NamespacePath, Module},
    ast::{AstNode, TypedAstNode},
    types::{TypeMap, TypeGroup, Type},
    type_checking::Symbol
};

fn register_foreign_builtin(
    procedure_path: NamespacePath,
    parameter_names: &[&'static str],
    parameter_types: Vec<TypeGroup>,
    return_type: TypeGroup,
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
        source: SourceRange::new(builtin_str, builtin_str, 0, 0)
    });
}

fn path_from(segments: &[&'static str], strings: &mut StringMap) -> NamespacePath {
    NamespacePath::new(segments.iter().map(|s| strings.insert(s)).collect())
}

pub fn load_builtins(
    target_str: &str,
    strings: &mut StringMap,
    modules: &mut HashMap<NamespacePath, Module<AstNode>>,
    types: &mut TypeMap,
    typed_symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    external_backings: &mut HashMap<NamespacePath, StringIdx>
) {
    load_foreign_builtins(strings, modules, types, typed_symbols);
    load_native_builtins(target_str, strings, modules, types, typed_symbols, external_backings);
}

fn load_foreign_builtins(
    strings: &mut StringMap,
    modules: &mut HashMap<NamespacePath, Module<AstNode>>,
    types: &mut TypeMap,
    typed_symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>
) {
    {
        let array_element_type = types.insert_group(&[Type::Any]);
        let object_tidx = types.insert_object(HashMap::new(), false);
        let array_tidx = types.insert_array(array_element_type);
        let t = types.insert_group(&[
            Type::Object(object_tidx),
            Type::Array(array_tidx)
        ]);
        register_foreign_builtin(
            path_from(&["core", "addr_eq"], strings),
            &["a", "b"],
            vec![t, t],
            types.insert_group(&[Type::Boolean]),
            strings, modules, typed_symbols
        );
    }
    {
        let var_tidx = types.insert_variants(HashMap::new(), false);
        let t = types.insert_group(&[
            Type::Variants(var_tidx)
        ]);
        register_foreign_builtin(
            path_from(&["core", "tag_eq"], strings),
            &["a", "b"],
            vec![t, t],
            types.insert_group(&[Type::Boolean]),
            strings, modules, typed_symbols
        );
    }
    {
        let array_element_type = types.insert_group(&[Type::Any]);
        let array_tidx = types.insert_array(array_element_type);
        register_foreign_builtin(
            path_from(&["core", "length"], strings),
            &["thing"],
            vec![
                types.insert_group(&[
                    Type::String,
                    Type::Array(array_tidx)
                ]),
            ],
            types.insert_group(&[Type::Integer]),
            strings, modules, typed_symbols
        );
    }
    {
        let array_element_types = types.insert_group(&[Type::Any]);
        let array_tidx = types.insert_array(array_element_types);
        register_foreign_builtin(
            path_from(&["core", "array"], strings),
            &["value", "size"],
            vec![
                array_element_types,
                types.insert_group(&[Type::Integer])
            ],
            types.insert_group(&[
                Type::Array(array_tidx)
            ]),
            strings, modules, typed_symbols
        );
    }
    { 
        let end = types.insert_group(&[Type::Any]);
        let next = types.insert_group(&[Type::Any]);
        let var_tidx = types.insert_variants(
            [
                (strings.insert("end"), end),
                (strings.insert("next"), next)
            ].into(),
            true
        );
        let exhausted_closure_return_types = types.insert_group(&[
            Type::Variants(var_tidx)
        ]);
        let closure_tidx = types.insert_closure(
            vec![],
            exhausted_closure_return_types
        );
        register_foreign_builtin(
            path_from(&["core", "exhaust"], strings),
            &["iter"],
            vec![
                types.insert_group(&[
                    Type::Closure(closure_tidx)
                ])
            ],
            types.insert_group(&[Type::Unit]),
            strings, modules, typed_symbols
        );
    }
    {
        register_foreign_builtin(
            path_from(&["core", "panic"], strings),
            &["message"],
            vec![
                types.insert_group(&[Type::String])
            ],
            types.insert_group(&[Type::Any]),
            strings, modules, typed_symbols
        );
    }
    {
        register_foreign_builtin(
            path_from(&["core", "as_str"], strings),
            &["thing"],
            vec![
                types.insert_group(&[Type::Any])
            ],
            types.insert_group(&[Type::String]),
            strings, modules, typed_symbols
        );
    }
    {
        register_foreign_builtin(
            path_from(&["core", "as_int"], strings),
            &["number"],
            vec![
                types.insert_group(&[Type::Integer, Type::Float])
            ],
            types.insert_group(&[Type::Integer]),
            strings, modules, typed_symbols
        );
    }
    {
        register_foreign_builtin(
            path_from(&["core", "as_flt"], strings),
            &["number"],
            vec![
                types.insert_group(&[Type::Integer, Type::Float])
            ],
            types.insert_group(&[Type::Float]),
            strings, modules, typed_symbols
        );
    }
    {
        let s = types.insert_group(&[Type::String]);
        let i = types.insert_group(&[Type::Integer]);
        register_foreign_builtin(
            path_from(&["core", "substring"], strings),
            &["source", "start", "end"],
            vec![s, i, i],
            s,
            strings, modules, typed_symbols
        );
    }
    {
        let s = types.insert_group(&[Type::String]);
        register_foreign_builtin(
            path_from(&["core", "concat"], strings),
            &["string_a", "string_b"],
            vec![s, s],
            s,
            strings, modules, typed_symbols
        );
    }
    {
        let i = types.insert_group(&[Type::Integer]);
        let u = types.insert_group(&[Type::Unit]);
        let var_tidx = types.insert_variants(
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
                types.insert_group(&[Type::String])
            ],
            types.insert_group(&[Type::Variants(var_tidx)]),
            strings, modules, typed_symbols
        );
    }
    {
        let f = types.insert_group(&[Type::Float]);
        let u = types.insert_group(&[Type::Unit]);
        let var_tidx = types.insert_variants(
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
                types.insert_group(&[Type::String])
            ],
            types.insert_group(&[Type::Variants(var_tidx)]),
            strings, modules, typed_symbols
        );
    }
    {
        register_foreign_builtin(
            path_from(&["core", "string"], strings),
            &["repeated", "times"],
            vec![
                types.insert_group(&[Type::String]),
                types.insert_group(&[Type::Integer])
            ],
            types.insert_group(&[Type::String]),
            strings, modules, typed_symbols
        );
    }
    {
        register_foreign_builtin(
            path_from(&["core", "hash"], strings),
            &["value"],
            vec![
                types.insert_group(&[Type::Any]),

            ],
            types.insert_group(&[Type::Integer]),
            strings, modules, typed_symbols
        );
    }
}

fn load_native_builtins(
    target_str: &str,
    strings: &mut StringMap,
    modules: &mut HashMap<NamespacePath, Module<AstNode>>,
    types: &mut TypeMap,
    typed_symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    external_backings: &mut HashMap<NamespacePath, StringIdx>
) {
    let src = strings.insert(include_str!("core.gera"));
    let file = strings.insert("<builtin>/core.gera");
    if let Err(errors) = process_file(file, src, target_str, strings, modules, types, typed_symbols, external_backings) {
        for error in errors {
            println!("{}", error.display(strings, false));
        }
        panic!("builtin file is invalid");
    }
}