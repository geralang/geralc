
use std::collections::HashMap;

use json::JsonValue;

use crate::frontend::{
    types::{TypeScope, Type},
    type_checking::Symbol,
    ast::TypedAstNode,
    modules::NamespacePath
};
use crate::util::strings::{StringMap, StringIdx};

pub fn generate_symbols(
    type_scope: TypeScope,
    typed_symbols: HashMap<NamespacePath, Symbol<TypedAstNode>>, 
    external_backings: HashMap<NamespacePath, StringIdx>,
    strings: &mut StringMap
) -> String {
    let mut result = json::object! {
        types: JsonValue::Array((0..type_scope.get_internal_group_count()).map(|g|
            serialize_group_types(
                type_scope.get_group_types_from_internal_index(g), &type_scope, strings
            )
        ).collect()),
        modules: json::object! {},
        procedures: json::object! {},
        constants: json::object! {}
    };
    serialize_modules(
        &[], &type_scope, &typed_symbols, &external_backings, strings, &mut result
    );
    return json::stringify(result)
}

fn serialize_group_types(
    types: &Option<Vec<Type>>,
    type_scope: &TypeScope,
    strings: &StringMap
) -> JsonValue {
    if let Some(possible_types) = types {
        json::object! {
            "any": false,
            "types": JsonValue::Array(
                possible_types.iter().map(|t| serialize_type(t, type_scope, strings)).collect()
            )
        }
    } else {
        json::object! {
            "any": true
        }
    }
}

fn serialize_type(
    t: &Type,
    type_scope: &TypeScope,
    strings: &StringMap
) -> JsonValue {
    match t {
        Type::Unit => json::object! { "type": "unit" },
        Type::Boolean => json::object! { "type": "boolean" },
        Type::Integer => json::object! { "type": "integer" },
        Type::Float => json::object! { "type": "float" },
        Type::String => json::object! { "type": "string" },
        Type::Panic => json::object! { "type": "panic" },
        Type::Array(element_types) => json::object! { 
            "type": "array", 
            "element_types": JsonValue::Number(
                type_scope.get_group_internal_index(*element_types).into()
            )
        },
        Type::Object(member_types, fixed) => {
            let mut member_stypes = json::object! {};
            for (mn, mt) in member_types {
                member_stypes[strings.get(*mn)] = JsonValue::Number(
                    type_scope.get_group_internal_index(*mt).into()
                );
            }
            json::object! {
                "type": "object",
                "member_types": member_stypes,
                "fixed": JsonValue::Boolean(*fixed)
            }
        }
        Type::ConcreteObject(member_types) => json::object! {
            "type": "concrete_object",
            "member_types": JsonValue::Array(member_types.iter().map(|(mn, mt)| json::object! {
                "name": strings.get(*mn),
                "type": serialize_type(mt, type_scope, strings)
            }).collect())
        },
        Type::Closure(parameter_types, return_types, _) => json::object! {
            "type": "closure",
            "parameter_types": JsonValue::Array(parameter_types.iter().map(|t|
                JsonValue::Number(type_scope.get_group_internal_index(*t).into())
            ).collect()),
            "return_types": JsonValue::Number(
                type_scope.get_group_internal_index(*return_types).into()
            )
        },
        Type::Variants(variant_types, fixed) => {
            let mut variant_stypes = json::object! {};
            for (vn, vt) in variant_types {
                variant_stypes[strings.get(*vn)] = JsonValue::Number(
                    type_scope.get_group_internal_index(*vt).into()
                );
            }
            json::object! {
                "type": "variants",
                "variant_types": variant_stypes,
                "fixed": JsonValue::Boolean(*fixed)
            }
        }
    }
}

fn serialize_modules(
    element_of: &[StringIdx],
    type_scope: &TypeScope,
    typed_symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>, 
    external_backings: &HashMap<NamespacePath, StringIdx>,
    strings: &mut StringMap,
    into: &mut JsonValue
) {
    for (symbol_path, symbol) in typed_symbols {
        if symbol_path.get_segments().len() - 1 < element_of.len() { continue; } // path is not even long enough
        if symbol_path.get_segments()[..element_of.len()] != *element_of { continue; } // path does not match
        let element_name = symbol_path.get_segments()[element_of.len()];
        if symbol_path.get_segments().len() - 1 > element_of.len() { // symbol is in submodule of this module
            let mut submodule_path = Vec::from(element_of);
            submodule_path.push(element_name);
            let mut module = json::object! {
                "modules": json::object! {},
                "procedures": json::object! {},
                "constants": json::object! {}
            };
            serialize_modules(
                &submodule_path, type_scope, typed_symbols, external_backings, strings, &mut module
            );
            into["modules"][strings.get(element_name)] = module;
            continue;
        }
        // symbol is in this module
        let element_name = strings.get(element_name);
        match symbol {
            Symbol::Procedure { public, parameter_names, parameter_types, returns, body, source: _ } => {
                let is_external = body.is_none() && external_backings.contains_key(symbol_path);
                let mut procedure = json::object! {
                    "public": *public,
                    "external": is_external,
                    "name": element_name,
                    "parameters": JsonValue::Array((0..parameter_names.len()).map(|p| json::object! {
                        "name": strings.get(parameter_names[p]),
                        "type": JsonValue::Number(
                            type_scope.get_group_internal_index(parameter_types[p]).into()
                        )
                    }).collect()),
                    "return_types": JsonValue::Number(
                        type_scope.get_group_internal_index(*returns).into()
                    )
                };
                if is_external {
                    procedure["backing"] = JsonValue::String(strings.get(
                        *external_backings.get(symbol_path).expect("checked above")
                    ).into());
                }
                into["procedures"][element_name] = procedure;
            }
            Symbol::Constant { public, value: _, value_types } => 
                into["constants"][element_name] = json::object! {
                    "public": *public,
                    "name": element_name,
                    "types": JsonValue::Number(
                        type_scope.get_group_internal_index(*value_types).into()
                    )
                }
        }
    }
}