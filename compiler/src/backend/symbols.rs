
use std::collections::HashMap;

use serde_json::json;

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
    let mut result = json!({
        "types": serde_json::Value::Array((0..type_scope.get_internal_group_count()).map(|g|
            serialize_group_types(
                type_scope.get_group_types_from_internal_index(g), &type_scope, strings
            )
        ).collect()),
        "modules": json!({}),
        "procedures": json!({}),
        "constants": json!({})
    });
    serialize_modules(
        &[], &type_scope, &typed_symbols, &external_backings, strings, &mut result
    );
    return result.to_string()
}

fn serialize_group_types(
    types: &Option<Vec<Type>>,
    type_scope: &TypeScope,
    strings: &StringMap
) -> serde_json::Value {
    if let Some(possible_types) = types {
        json!({
            "any": false,
            "types": serde_json::Value::Array(
                possible_types.iter().map(|t| serialize_type(t, type_scope, strings)).collect()
            )
        })
    } else {
        json!({
            "any": true
        })
    }
}

fn serialize_type(
    t: &Type,
    type_scope: &TypeScope,
    strings: &StringMap
) -> serde_json::Value {
    match t {
        Type::Unit => json!({ "type": "unit" }),
        Type::Boolean => json!({ "type": "boolean" }),
        Type::Integer => json!({ "type": "integer" }),
        Type::Float => json!({ "type": "float" }),
        Type::String => json!({ "type": "string" }),
        Type::Panic => json!({ "type": "panic" }),
        Type::Array(element_types) => json!({ 
            "type": "array", 
            "element_types": serde_json::Value::Number(
                type_scope.get_group_internal_index(*element_types).into()
            )
        }),
        Type::Object(member_types, fixed) => {
            let mut member_stypes = json!({});
            for (mn, mt) in member_types {
                member_stypes[strings.get(*mn)] = serde_json::Value::Number(
                    type_scope.get_group_internal_index(*mt).into()
                );
            }
            json!({
                "type": "object",
                "member_types": member_stypes,
                "fixed": *fixed
            })
        }
        Type::ConcreteObject(member_types) => json!({
            "type": "concrete_object",
            "member_types": serde_json::Value::Array(member_types.iter().map(|(mn, mt)| json!({
                "name": strings.get(*mn),
                "type": serialize_type(mt, type_scope, strings)
            })).collect())
        }),
        Type::Closure(parameter_types, return_types, _) => json!({
            "type": "closure",
            "parameter_types": serde_json::Value::Array(parameter_types.iter().map(|t|
                serde_json::Value::Number(type_scope.get_group_internal_index(*t).into())
            ).collect()),
            "return_types": serde_json::Value::Number(
                type_scope.get_group_internal_index(*return_types).into()
            )
        }),
        Type::Variants(variant_types, fixed) => {
            let mut variant_stypes = json!({});
            for (vn, vt) in variant_types {
                variant_stypes[strings.get(*vn)] = serde_json::Value::Number(
                    type_scope.get_group_internal_index(*vt).into()
                );
            }
            json!({
                "type": "variants",
                "variant_types": variant_stypes,
                "fixed": *fixed
            })
        }
    }
}

fn serialize_modules(
    element_of: &[StringIdx],
    type_scope: &TypeScope,
    typed_symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>, 
    external_backings: &HashMap<NamespacePath, StringIdx>,
    strings: &mut StringMap,
    into: &mut serde_json::Value
) {
    for (symbol_path, symbol) in typed_symbols {
        if symbol_path.get_segments().len() - 1 < element_of.len() { continue; } // path is not even long enough
        if symbol_path.get_segments()[..element_of.len()] != *element_of { continue; } // path does not match
        let element_name = symbol_path.get_segments()[element_of.len()];
        if symbol_path.get_segments().len() - 1 > element_of.len() { // symbol is in submodule of this module
            let mut submodule_path = Vec::from(element_of);
            submodule_path.push(element_name);
            let mut module = json!({
                "modules": json!({}),
                "procedures": json!({}),
                "constants": json!({})
            });
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
                let mut procedure = json!({
                    "public": *public,
                    "external": is_external,
                    "name": element_name,
                    "parameters": serde_json::Value::Array((0..parameter_names.len()).map(|p| json!({
                        "name": strings.get(parameter_names[p]),
                        "type": serde_json::Value::Number(
                            type_scope.get_group_internal_index(parameter_types[p]).into()
                        )
                    })).collect()),
                    "return_types": serde_json::Value::Number(
                        type_scope.get_group_internal_index(*returns).into()
                    )
                });
                if is_external {
                    procedure["backing"] = serde_json::Value::String(strings.get(
                        *external_backings.get(symbol_path).expect("checked above")
                    ).into());
                }
                into["procedures"][element_name] = procedure;
            }
            Symbol::Constant { public, value: _, value_types } => 
                into["constants"][element_name] = json!({
                    "public": *public,
                    "name": element_name,
                    "types": serde_json::Value::Number(
                        type_scope.get_group_internal_index(*value_types).into()
                    )
                })
        }
    }
}