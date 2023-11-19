
use std::collections::HashMap;

use crate::backend::{
    ir::{IrSymbol, IrTypeBank, IrType, IrInstruction, IrVariable, IrTypeBankMapping},
    interpreter::Value
};
use crate::frontend::modules::NamespacePath;
use crate::util::strings::{StringMap, StringIdx};

pub fn generate_javascript(
    symbols: Vec<IrSymbol>,
    types: IrTypeBank,
    type_dedup: IrTypeBankMapping,
    main_procedure_path: NamespacePath,
    strings: &mut StringMap
) -> String {
    let mut output = String::new();
    let externals = collect_externals(&symbols);
    output.push_str("(function() {\n");
    output.push_str("\"use strict\";\n");
    emit_core_library(&mut output);
    output.push_str("\n");
    emit_constants(&symbols, strings, &mut output);
    output.push_str("\n");
    emit_procedure_impls(&symbols, &types, &type_dedup, strings, &externals, &mut output);
    output.push_str("\n");
    emit_main_function(&main_procedure_path, strings, &mut output);
    output.push_str("\n})();");
    output
}

fn collect_externals(symbols: &Vec<IrSymbol>) -> HashMap<NamespacePath, StringIdx> {
    let mut external = HashMap::new();
    for symbol in symbols {
        match symbol {
            IrSymbol::Procedure { .. } => {}
            IrSymbol::BuiltInProcedure { .. } => {}
            IrSymbol::Variable { .. } => {}
            IrSymbol::ExternalProcedure { path, backing, .. } |
            IrSymbol::ExternalVariable { path, backing, .. } => {
                external.insert(path.clone(), *backing);
            }
        }
    }
    return external;
}

fn emit_core_library(output: &mut String) {
    output.push_str(include_str!("./gen_libs/core.js"));
    output.push_str("\n");
}

fn emit_constants(
    symbols: &Vec<IrSymbol>,
    strings: &mut StringMap,
    output: &mut String
) {
    for symbol in symbols {
        match symbol {
            IrSymbol::Procedure { .. } |
            IrSymbol::ExternalProcedure { .. } |
            IrSymbol::BuiltInProcedure { .. } => {}
            IrSymbol::Variable { path, value_type: _, value } => {
                output.push_str("const ");
                emit_path(path, strings, output);
                output.push_str(" = ");
                emit_value(value, output);
                output.push_str(";\n");
            }
            IrSymbol::ExternalVariable { .. } => {}
        }
    }
}

fn emit_variable(variable: IrVariable, output: &mut String) {
    output.push_str("local");
    output.push_str(&variable.index.to_string());
}

fn get_builtin_bodies(strings: &mut StringMap) -> HashMap<NamespacePath, fn(&Vec<IrType>, IrType, &IrTypeBank, &IrTypeBankMapping, &mut StringMap) -> String> {
    fn path_from(segments: &[&'static str], strings: &mut StringMap) -> NamespacePath {
        NamespacePath::new(segments.iter().map(|s| strings.insert(s)).collect())
    }
    let mut builtins: HashMap<NamespacePath, fn(&Vec<IrType>, IrType, &IrTypeBank, &IrTypeBankMapping, &mut StringMap) -> String> = HashMap::new();
    builtins.insert(path_from(&["core", "addr_eq"], strings), |_, _, _, _, _| {
        String::from(r#"
return param0 === param1;
"#)
    });
    builtins.insert(path_from(&["core", "tag_eq"], strings), |_, _, _, _, _| {
        String::from(r#"
return param0.tag === param1.tag;
"#)
    });
    builtins.insert(path_from(&["core", "length"], strings), |_, _, _, _, _| {
        String::from(r#"
return BigInt(param0.length);
"#)
    });
    builtins.insert(path_from(&["core", "array"], strings), |_, _, _, _, _| {
        String::from(r#"
return new Array(Number(param1)).fill(param0);
"#)
    });
    builtins.insert(path_from(&["core", "exhaust"], strings), |_, _, _, _, strings| {
        format!("
while(param0.call().tag == {}) {{}}
", strings.insert("next").0)
    });
    builtins.insert(path_from(&["core", "panic"], strings), |_, _, _, _, _| {
        String::from(r#"
throw param0;
"#)
    });
    builtins.insert(path_from(&["core", "as_str"], strings), |_, _, _, _, _| {
        String::from(r#"
return param0.toString();
"#)
    });
    builtins.insert(path_from(&["core", "as_int"], strings), |_, _, _, _, _| {
        String::from(r#"
return BigInt(Math.floor(param0));
"#)
    });
    builtins.insert(path_from(&["core", "as_flt"], strings), |_, _, _, _, _| {
        String::from(r#"
return Number(param0);
"#)
    });
    builtins.insert(path_from(&["core", "substring"], strings), |_, _, _, _, _| {
        String::from(r#"
let start_idx = param1;
if(param1 < 0) { start_idx = param0.length + param1; }
if(param1 > param0.length) {
    throw `the start index ${param1} is out of bounds for a string of length ${param0.length}`;
}
let end_idx = param2;
if(param2 < 0) { end_idx = param0.length + param2; }
if(param2 > param0.length) {
    throw `the end index ${param2} is out of bounds for a string of length ${param0.length}`;
}
if(start_idx > end_idx) {
    throw `the start index ${param1} is larger than the end index ${param2} (length of string is ${param0.length})`;
}
return param0.substring(Number(param0), Number(param1));
"#)
    });
    builtins.insert(path_from(&["core", "concat"], strings), |_, _, _, _, _| {
        String::from(r#"
return param0 + param1;
"#)
    });
    builtins.insert(path_from(&["core", "parse_flt"], strings), |_, _, _, _, strings| {
        format!("
const r = parseFloat(param0);
if(isNaN(r)) {{ return {{ tag: {}, value: undefined }}; }}
return {{ tag: {}, value: r }};
", strings.insert("none").0, strings.insert("some").0)
    });
    builtins.insert(path_from(&["core", "parse_int"], strings), |_, _, _, _, strings| {
        format!("
const r = parseInt(param0);
if(isNaN(r)) {{ return {{ tag: {}, value: undefined }}; }}
return {{ tag: {}, value: BigInt(r) }};
", strings.insert("none").0, strings.insert("some").0)
    });
    builtins.insert(path_from(&["core", "string"], strings), |_, _, _, _, _| {
        String::from(r#"
return param0.repeat(Number(param1));
"#)
    });
    builtins.insert(path_from(&["core", "hash"], strings), |_, _, _, _, _| {
        String::from(r#"
return hash(param0);
"#)
    });
    return builtins;
}

fn emit_procedure_impls(
    symbols: &Vec<IrSymbol>,
    types: &IrTypeBank,
    type_dedup: &IrTypeBankMapping,
    strings: &mut StringMap,
    external: &HashMap<NamespacePath, StringIdx>,
    output: &mut String
) {
    let builtin_bodies = get_builtin_bodies(strings);
    for symbol in symbols {
        match symbol {
            IrSymbol::Procedure { path, variant, parameter_types, return_type: _, variables, body, source: _ } => {
                output.push_str("function ");
                emit_procedure_name(path, *variant, strings, output);
                output.push_str("(");
                let mut had_param = false;
                for p in 0..parameter_types.len() {
                    if had_param { output.push_str(", "); }
                    had_param = true;
                    output.push_str("param");
                    output.push_str(&p.to_string());
                }
                output.push_str(") {\n");
                for variable_idx in 0..variables.len() {
                    output.push_str("    let ");
                    emit_variable(IrVariable { index: variable_idx, version: 0 }, output);
                    output.push_str(";\n");
                }
                let mut body_str = String::new();
                emit_block(
                    body, variables, types, type_dedup, external, symbols, strings, &mut body_str
                );
                indent(&body_str, output);
                output.push_str("}\n");
            }
            IrSymbol::BuiltInProcedure { path, variant, parameter_types, return_type } => {
                output.push_str("function ");
                emit_procedure_name(path, *variant, strings, output);
                output.push_str("(");
                let mut had_param = false;
                for p in 0..parameter_types.len() {
                    if had_param { output.push_str(", "); }
                    had_param = true;
                    output.push_str("param");
                    output.push_str(&p.to_string());
                }
                output.push_str(") {\n");
                let body_str = (builtin_bodies
                    .get(path)
                    .expect("builtin should have implementation"))
                    (parameter_types, *return_type, types, type_dedup, strings);
                indent(&body_str, output);
                output.push_str("}\n");
            }
            _ => {}
        }
    }
}

fn emit_main_function(
    main_procedure_path: &NamespacePath,
    strings: &StringMap,
    output: &mut String
) {
    emit_procedure_name(main_procedure_path, 0, strings, output);
    output.push_str("();\n");
}

fn emit_path(path: &NamespacePath, strings: &StringMap,output: &mut String) {
    output.push_str(
        &path.get_segments()
            .iter()
            .map(|s| strings.get(*s)
            .replace("_", "__"))
            .collect::<Vec<String>>()
            .join("_")
    );
}

fn emit_procedure_name(
    path: &NamespacePath,
    variant: usize,
    strings: &StringMap,
    output: &mut String
) {
    emit_path(path, strings, output);
    output.push_str("_");
    output.push_str(&variant.to_string());
}

fn emit_string_literal(value: &str, output: &mut String) {
    output.push('"');
    output.push_str(
        &value.replace("\\", "\\\\")
            .replace("\n", "\\n")
            .replace("\r", "\\r")
            .replace("\"", "\\\"")
    );
    output.push('"');
}

fn emit_value(
    value: &Value, output: &mut String
) {
    match value {
        Value::Unit => output.push_str("undefined"),
        Value::Boolean(b) => output.push_str(if *b { "true" } else { "false" }),
        Value::Integer(i) => {
            output.push_str(&i.to_string());
            output.push_str("n");
        }
        Value::Float(f) => {
            if *f == f64::INFINITY { output.push_str("Infinity"); }
            else if *f == f64::NEG_INFINITY { output.push_str("-Infinity"); }
            else if f.is_nan() { output.push_str("NaN"); }
            else { output.push_str(&format!("{:.}", *f)); }
        }
        Value::String(s) => emit_string_literal(std::rc::Rc::as_ref(s), output),
        Value::Array(_) => panic!("constant arrays are forbidden!"),
        Value::Object(_) => panic!("constant objects are forbidden!"),
        Value::Closure(_, _, _, _, _) => panic!("constant closures are forbidden!"),
        Value::Variant(variant_name, variant_value) => {
            output.push_str("{ tag = ");
            output.push_str(&variant_name.0.to_string());
            output.push_str(", value = ");
            emit_value(&*variant_value, output);
            output.push_str("}");
        }
    }
}

fn indent(indent: &str, output: &mut String) {
    for line in indent.lines() {
        if line.len() == 0 { continue; }
        output.push_str("    ");
        output.push_str(line);
        output.push_str("\n");
    }
}

fn emit_block(
    instructions: &Vec<IrInstruction>,
    variable_types: &Vec<IrType>,
    types: &IrTypeBank,
    type_dedup: &IrTypeBankMapping,
    external: &HashMap<NamespacePath, StringIdx>,
    symbols: &Vec<IrSymbol>,
    strings: &StringMap,
    output: &mut String
) {
    output.push_str("{\n");
    for instruction in instructions {
        let mut o = String::new();
        emit_instruction(
            instruction, variable_types, types, type_dedup, external, symbols, strings, &mut o
        );
        indent(&o, output);
    }
    output.push_str("}");
}

fn emit_copied(
    copied: &str,
    copied_type: IrType,
    types: &IrTypeBank,
    type_dedup: &IrTypeBankMapping,
    output: &mut String
) {
    match copied_type.direct(types).apply_mapping(type_dedup) {
        IrType::Unit |
        IrType::Boolean |
        IrType::Integer |
        IrType::Float |
        IrType::String |
        IrType::Array(_) |
        IrType::Object(_) |
        IrType::ConcreteObject(_) |
        IrType::Closure(_) => {
            output.push_str(copied);
        }
        IrType::Variants(_) => {
            output.push_str("{ tag: ");
            output.push_str(copied);
            output.push_str(".tag, value: ");
            output.push_str(copied);
            output.push_str(".value }");
        }
        IrType::Indirect(_) => panic!("should be direct!")
    }
}

fn emit_copied_variable(
    copied: IrVariable,
    copied_type: IrType,
    types: &IrTypeBank,
    type_dedup: &IrTypeBankMapping,
    output: &mut String
) {
    let mut copied_str = String::new();
    emit_variable(copied, &mut copied_str);
    emit_copied(&copied_str, copied_type, types, type_dedup, output);
}

fn emit_instruction(
    instruction: &IrInstruction,
    variable_types: &Vec<IrType>,
    types: &IrTypeBank,
    type_dedup: &IrTypeBankMapping,
    external: &HashMap<NamespacePath, StringIdx>,
    symbols: &Vec<IrSymbol>,
    strings: &StringMap,
    output: &mut String
) {
    match instruction {
        IrInstruction::LoadUnit { .. } => {}
        IrInstruction::LoadBoolean { value, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            output.push_str(if *value { "true" } else { "false" });
            output.push_str(";\n");
        }
        IrInstruction::LoadInteger { value, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            output.push_str(&value.to_string());
            output.push_str("n;\n");
        }
        IrInstruction::LoadFloat { value, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            output.push_str(&format!("{:.}", *value));
            output.push_str(";\n");
        }
        IrInstruction::LoadString { value, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_string_literal(strings.get(*value), output);
            output.push_str(";\n");
        }
        IrInstruction::LoadObject { member_values, into } => {
            emit_variable(*into, output);
            output.push_str(" = { ");
            let mut had_member = false;
            for (member_name, member_value) in member_values {
                if had_member { output.push_str(", "); }
                had_member = true;
                output.push_str(strings.get(*member_name));
                output.push_str(": ");
                emit_copied_variable(
                    *member_value, variable_types[member_value.index], types, type_dedup, output
                );
            }
            output.push_str(" };\n");
        }
        IrInstruction::LoadArray { element_values, into } => {
            emit_variable(*into, output);
            output.push_str(" = [ ");
            let mut had_member = false;
            for element_value in element_values {
                if had_member { output.push_str(", "); }
                had_member = true;
                emit_copied_variable(
                    *element_value, variable_types[element_value.index], types, type_dedup, output
                );
            }
            output.push_str(" ];\n");
        }
        IrInstruction::LoadVariant { name, v, into } => {
            emit_variable(*into, output);
            output.push_str(" = { tag: ");
            output.push_str(&name.0.to_string());
            output.push_str(", value: ");
            emit_copied_variable(*v, variable_types[v.index], types, type_dedup, output);
            output.push_str(" };\n");
        }
        IrInstruction::LoadGlobalVariable { path, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            let path_matches = |s: &&IrSymbol| match s {
                IrSymbol::Procedure { path: p, .. } |
                IrSymbol::ExternalProcedure { path: p, .. } |
                IrSymbol::BuiltInProcedure { path: p, .. } |
                IrSymbol::Variable { path: p, .. } |
                IrSymbol::ExternalVariable { path: p, .. } => *path == *p
            };
            match symbols.iter().find(path_matches).expect("should exist") {
                IrSymbol::Procedure { .. } |
                IrSymbol::BuiltInProcedure { .. } |
                IrSymbol::ExternalProcedure { .. } => {
                    let (expected_parameter_types, expected_return_type) =
                        if let IrType::Closure(closure_idx) = variable_types[into.index].direct(types).apply_mapping(type_dedup) {
                            types.get_closure(closure_idx)
                        } else { panic!("should be a closure"); };
                    let mut found_variant = 0;
                    for symbol in symbols.iter().filter(path_matches) {
                        match symbol {
                            IrSymbol::Procedure { variant, parameter_types, return_type, .. } |
                            IrSymbol::BuiltInProcedure { variant, parameter_types, return_type, .. } => {
                                let mut params_match = true;
                                for param_idx in 0..parameter_types.len() {
                                    if !parameter_types[param_idx].eq(
                                        &expected_parameter_types[param_idx], types,
                                        &mut HashMap::new()
                                    ) {
                                        params_match = false;
                                        break;
                                    }
                                }
                                if !params_match { continue; }
                                if !return_type.eq(expected_return_type, types, &mut HashMap::new()) {
                                    continue;
                                }
                                found_variant = *variant;
                                break;
                            }
                            IrSymbol::ExternalProcedure { .. } => {
                                break;
                            }
                            IrSymbol::Variable { .. } |
                            IrSymbol::ExternalVariable { .. } => panic!("impossible")
                        }
                    }
                    emit_variable(*into, output);
                    output.push_str(" = ");
                    if let Some(backing) = external.get(path) {
                        output.push_str(strings.get(*backing));
                    } else {
                        emit_procedure_name(path, found_variant, strings, output);
                    }
                    output.push_str(";\n");
                }
                IrSymbol::Variable { .. } |
                IrSymbol::ExternalVariable { .. } => {
                    emit_variable(*into, output);
                    output.push_str(" = ");
                    if let Some(backing) = external.get(path) {
                        output.push_str(strings.get(*backing));
                    } else {
                        emit_path(path, strings, output);
                    }
                    output.push_str(";\n");
                }
            }
        }
        IrInstruction::LoadParameter { index, into } => {
            emit_variable(*into, output);
            output.push_str(" = param");
            output.push_str(&index.to_string());
            output.push_str(";\n");
        }
        IrInstruction::LoadClosure {
            parameter_types, return_type: _, captured, variables, body, into, source: _
        } => {
            emit_variable(*into, output);
            output.push_str(" = {\n");
            output.push_str("    captures: { ");
            let mut had_capture = false;
            for (capture_name, capture_value) in captured {
                if had_capture { output.push_str(", "); }
                had_capture = true;
                output.push_str(strings.get(*capture_name));
                output.push_str(": ");
                emit_copied_variable(
                    *capture_value, variable_types[capture_value.index], types, type_dedup, output
                );
            }
            output.push_str(" },\n");
            let mut body_str = String::new();
            body_str.push_str("call: function(");
            let mut had_param = false;
            for p in 0..parameter_types.len() {
                if had_param { body_str.push_str(", "); }
                had_param = true;
                body_str.push_str("param");
                body_str.push_str(&p.to_string());
            }
            body_str.push_str(") {\n");
            for variable_idx in 0..variables.len() {
                body_str.push_str("    let ");
                emit_variable(IrVariable { index: variable_idx, version: 0 }, &mut body_str);
                body_str.push_str(";\n");
            }
            let mut body_body_str = String::new();
            emit_block(
                body, variables, types, type_dedup, external, symbols, strings, &mut body_body_str
            );
            indent(&body_body_str, &mut body_str);
            body_str.push_str("}\n");
            indent(&body_str, output);
            output.push_str("};\n");
        }
        IrInstruction::GetObjectMember { accessed, member, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            let mut accessed_str = String::new();
            emit_variable(*accessed, &mut accessed_str);
            accessed_str.push_str(".");
            accessed_str.push_str(strings.get(*member));
            emit_copied(&accessed_str, variable_types[into.index], types, type_dedup, output);
            output.push_str(";\n");
        }
        IrInstruction::SetObjectMember { value, accessed, member } => {
            emit_variable(*accessed, output);
            output.push_str(".");
            output.push_str(strings.get(*member));
            output.push_str(" = ");
            emit_copied_variable(*value, variable_types[value.index], types, type_dedup, output);
            output.push_str(";\n");
        }
        IrInstruction::GetArrayElement { accessed, index, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            let mut accessed_str = String::new();
            emit_variable(*accessed, &mut accessed_str);
            accessed_str.push_str("[");
            emit_variable(*index, &mut accessed_str);
            accessed_str.push_str("]");
            emit_copied(&accessed_str, variable_types[into.index], types, type_dedup, output);
            output.push_str(";\n");
        }
        IrInstruction::SetArrayElement { value, accessed, index } => {
            emit_variable(*accessed, output);
            output.push_str("[");
            emit_variable(*index, output);
            output.push_str("] = ");
            emit_copied_variable(*value, variable_types[value.index], types, type_dedup, output);
            output.push_str(";\n");
        }
        IrInstruction::GetClosureCapture { name, into } => {
            emit_variable(*into, output);
            output.push_str(" = this.captures.");
            output.push_str(strings.get(*name));
            output.push_str(";\n");
        }
        IrInstruction::SetClosureCapture { value, name } => {
            output.push_str("this.captures.");
            output.push_str(strings.get(*name));
            output.push_str(" = ");
            emit_copied_variable(*value, variable_types[value.index], types, type_dedup, output);
            output.push_str(";\n");
        }
        IrInstruction::Move { from, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_copied_variable(*from, variable_types[from.index], types, type_dedup, output);
            output.push_str(";\n");
        }
        IrInstruction::Add { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            if let IrType::Integer = variable_types[a.index].direct(types).apply_mapping(type_dedup) {
                output.push_str("BigInt.asIntN(64, ");
                emit_variable(*a, output);
                output.push_str(" + ");
                emit_variable(*b, output);
                output.push_str(")");
            } else {
                emit_variable(*a, output);
                output.push_str(" + ");
                emit_variable(*b, output);
            }
            output.push_str(";\n");
        }
        IrInstruction::Subtract { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            if let IrType::Integer = variable_types[a.index].direct(types).apply_mapping(type_dedup) {
                output.push_str("BigInt.asIntN(64, ");
                emit_variable(*a, output);
                output.push_str(" - ");
                emit_variable(*b, output);
                output.push_str(")");
            } else {
                emit_variable(*a, output);
                output.push_str(" - ");
                emit_variable(*b, output);
            }
            output.push_str(";\n");
        }
        IrInstruction::Multiply { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            if let IrType::Integer = variable_types[a.index].direct(types).apply_mapping(type_dedup) {
                output.push_str("BigInt.asIntN(64, ");
                emit_variable(*a, output);
                output.push_str(" * ");
                emit_variable(*b, output);
                output.push_str(")");
            } else {
                emit_variable(*a, output);
                output.push_str(" * ");
                emit_variable(*b, output);
            }
            output.push_str(";\n");
        }
        IrInstruction::Divide { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            if let IrType::Integer = variable_types[a.index].direct(types).apply_mapping(type_dedup) {
                output.push_str("BigInt.asIntN(64, ");
                emit_variable(*a, output);
                output.push_str(" / ");
                emit_variable(*b, output);
                output.push_str(")");
            } else {
                emit_variable(*a, output);
                output.push_str(" / ");
                emit_variable(*b, output);
            }
            output.push_str(";\n");
        }
        IrInstruction::Modulo { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            if let IrType::Integer = variable_types[a.index].direct(types).apply_mapping(type_dedup) {
                output.push_str("BigInt.asIntN(64, ");
                emit_variable(*a, output);
                output.push_str(" % ");
                emit_variable(*b, output);
                output.push_str(")");
            } else {
                emit_variable(*a, output);
                output.push_str(" % ");
                emit_variable(*b, output);
            }
            output.push_str(";\n");
        }
        IrInstruction::Negate { x, into } => {
            emit_variable(*into, output);
            output.push_str(" = -");
            emit_variable(*x, output);
            output.push_str(";\n");
        }
        IrInstruction::LessThan { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_variable(*a, output);
            output.push_str(" < ");
            emit_variable(*b, output);
            output.push_str(";\n");
        }
        IrInstruction::LessThanEquals { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_variable(*a, output);
            output.push_str(" <= ");
            emit_variable(*b, output);
            output.push_str(";\n");
        }
        IrInstruction::GreaterThan { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_variable(*a, output);
            output.push_str(" > ");
            emit_variable(*b, output);
            output.push_str(";\n");
        }
        IrInstruction::GreaterThanEquals { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_variable(*a, output);
            output.push_str(" >= ");
            emit_variable(*b, output);
            output.push_str(";\n");
        }
        IrInstruction::Equals { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = eq(");
            emit_variable(*a, output);
            output.push_str(", ");
            emit_variable(*b, output);
            output.push_str(");\n");
        }
        IrInstruction::NotEquals { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = !eq(");
            emit_variable(*a, output);
            output.push_str(", ");
            emit_variable(*b, output);
            output.push_str(");\n");
        }
        IrInstruction::Not { x, into } => {
            emit_variable(*into, output);
            output.push_str(" = !");
            emit_variable(*x, output);
            output.push_str(";\n");
        }
        IrInstruction::BranchOnValue { value, branches, else_branch } => {
            let mut value_str = String::new();
            emit_variable(*value, &mut value_str);
            let mut had_branch = false;
            for (branch_value, branch_body) in branches {
                if had_branch { output.push_str(" else "); }
                had_branch = true;
                output.push_str("if(eq(");
                output.push_str(&value_str);
                output.push_str(", ");
                emit_value(branch_value, output);
                output.push_str(")) ");
                emit_block(
                    branch_body, variable_types, types, type_dedup, external, symbols, strings,
                    output
                );
            }
            if else_branch.len() > 0 {
                if had_branch { output.push_str(" else "); }
                emit_block(
                    else_branch, variable_types, types, type_dedup, external, symbols, strings, output
                );
            }
            output.push_str("\n");
        }
        IrInstruction::BranchOnVariant { value, branches, else_branch } => {
            let mut matched = String::new();
            emit_variable(*value, &mut matched);
            output.push_str("switch(");
            output.push_str(&matched);
            output.push_str(".tag) {\n");
            for (branch_variant, branch_variable, branch_body) in branches {
                let mut branch_str = String::new();
                branch_str.push_str("case ");
                branch_str.push_str(&branch_variant.0.to_string());
                branch_str.push_str(": ");
                if let Some(branch_variable) = branch_variable {
                    emit_variable(*branch_variable, &mut branch_str);
                    branch_str.push_str(" = ");
                    branch_str.push_str(&matched);
                    branch_str.push_str(".value; ");
                }
                emit_block(
                    branch_body, variable_types, types, type_dedup, external, symbols, strings,
                    &mut branch_str
                );
                branch_str.push_str("\n");
                indent(&branch_str, output);
            }
            if else_branch.len() > 0 {
                let mut else_branch_str = String::new();
                else_branch_str.push_str("default: ");
                emit_block(
                    else_branch, variable_types, types, type_dedup, external, symbols, strings,
                    &mut else_branch_str
                );
                else_branch_str.push_str("\n");
                indent(&else_branch_str, output);
            }
            output.push_str("}\n");
        }
        IrInstruction::Call { path, variant, arguments, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            if let Some(backing) = external.get(path) {
                output.push_str(strings.get(*backing));
            } else {
                emit_procedure_name(path, *variant, strings, output);
            }
            output.push_str("(");
            let mut had_param = false;
            for argument_idx in 0..arguments.len() {
                if had_param { output.push_str(", "); }
                had_param = true;
                emit_copied_variable(
                    arguments[argument_idx], variable_types[arguments[argument_idx].index], types,
                    type_dedup, output
                );
            }
            output.push_str(");\n");
        }
        IrInstruction::CallClosure { called, arguments, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_variable(*called, output);
            output.push_str(".call(");
            let mut had_param = false;
            for argument_idx in 0..arguments.len() {
                if had_param { output.push_str(", "); }
                had_param = true;
                emit_copied_variable(
                    arguments[argument_idx], variable_types[arguments[argument_idx].index], types,
                    type_dedup, output
                );
            }
            output.push_str(");\n");
        }
        IrInstruction::Return { value } => {
            output.push_str("return ");
            emit_variable(*value, output);
            output.push_str(";\n");
        }
        IrInstruction::Phi { .. } => {}
    }
}

