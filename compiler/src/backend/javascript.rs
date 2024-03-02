
use std::collections::HashMap;

use crate::backend::{
    ir::{IrSymbol, IrInstruction, IrVariable},
    constants::{ConstantValue, ConstantPool, ConstantPoolValue}
};
use crate::frontend::{
    modules::NamespacePath,
    types::{TypeGroup, TypeMap, Type}
};
use crate::util::strings::{StringMap, StringIdx};

pub fn generate_javascript(
    symbols: Vec<IrSymbol>,
    mut types: TypeMap,
    mut constants: ConstantPool,
    main_procedure_path: NamespacePath,
    strings: &mut StringMap,
    max_call_depth: usize
) -> String {
    types.replace_any_with_unit();
    let mut output = String::new();
    let externals = collect_externals(&symbols);
    output.push_str("\n//
// Generated from Gera source code by gerap.
// See: https://github.com/geralang/gerac
//\n");
    output.push_str("\n");
    output.push_str("(function() {\n");
    output.push_str("\"use strict\";\n");
    output.push_str("const GERA_MAX_CALL_DEPTH = ");
    output.push_str(&max_call_depth.to_string());
    output.push_str(";\n");
    emit_core_library(&mut output);
    output.push_str("\n");
    let mut constant_deps = String::new();
    emit_static_variables(&symbols, &mut constants, strings, &mut constant_deps);
    constant_deps.push_str("\n");
    emit_procedure_impls(&symbols, &mut types, &mut constants, strings, &externals, &mut constant_deps);
    output.push_str("\n");
    emit_constant_declarations(&mut types, &mut constants, &externals, &symbols, strings, &mut output);
    output.push_str("\n");
    output.push_str(&constant_deps);
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
    output.push_str(include_str!("./core/core.js"));
    output.push_str("\n");
}

fn emit_static_variables(
    symbols: &Vec<IrSymbol>,
    constants: &ConstantPool,
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
                emit_value(&value, constants, output);
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

fn get_builtin_bodies(strings: &mut StringMap) -> HashMap<NamespacePath, fn(&Vec<TypeGroup>, TypeGroup, &mut TypeMap, &mut StringMap) -> String> {
    fn path_from(segments: &[&'static str], strings: &mut StringMap) -> NamespacePath {
        NamespacePath::new(segments.iter().map(|s| strings.insert(s)).collect())
    }
    let mut builtins: HashMap<NamespacePath, fn(&Vec<TypeGroup>, TypeGroup, &mut TypeMap, &mut StringMap) -> String> = HashMap::new();
    builtins.insert(path_from(&["core", "addr_eq"], strings), |_, _, _, _| {
        String::from(r#"
return param0 === param1;
"#)
    });
    builtins.insert(path_from(&["core", "tag_eq"], strings), |_, _, _, _| {
        String::from(r#"
return param0.tag === param1.tag;
"#)
    });
    builtins.insert(path_from(&["core", "length"], strings), |param_types, _, types, _| {
        match types.group_concrete(param_types[0]) {
            Type::String => String::from(r#"
return BigInt(gera___strlen(param0));
"#),
            Type::Array(_) => String::from(r#"
return BigInt(param0.length);
"#),
            _ => panic!("should only be array or string!")
        }        
    });
    builtins.insert(path_from(&["core", "array"], strings), |_, _, _, _| {
        String::from(r#"
return new Array(Number(param1)).fill(param0);
"#)
    });
    builtins.insert(path_from(&["core", "exhaust"], strings), |_, _, _, strings| {
        format!("
while(param0.call().tag == {}) {{}}
", strings.insert("next").0)
    });
    builtins.insert(path_from(&["core", "panic"], strings), |_, _, _, _| {
        String::from(r#"
gera___panic(param0);
"#)
    });
    builtins.insert(path_from(&["core", "as_str"], strings), |param_types, _, types, strings| {
        match types.group_concrete(param_types[0]) {
            Type::Unit | Type::Any => String::from(r#"
return "<unit>";
"#),
            Type::Boolean |
            Type::Integer |
            Type::Float => String::from(r#"
return param0.toString();
"#),
            Type::String => String::from(r#"
return param0;
"#),
            Type::Array(_) => String::from(r#"
return "<array>";
"#),
            Type::Object(_) |
            Type::ConcreteObject(_) => String::from(r#"
return "<object>";
"#),
            Type::Variants(variant_idx) => {
                let variant_types = &types.variants(variant_idx).0;
                let mut result = String::from("switch(param0.tag) {\n");
                for (variant_name, _) in variant_types {
                    result.push_str("    case ");
                    result.push_str(&variant_name.0.to_string());
                    result.push_str(": return \"#");
                    result.push_str(strings.get(*variant_name));
                    result.push_str(" <...>\";\n");
                }
                result.push_str("}\n");
                result
            }
            Type::Closure(_) => String::from(r#"
return "<closure>";
"#)
        }
    });
    builtins.insert(path_from(&["core", "as_int"], strings), |_, _, _, _| {
        String::from(r#"
return BigInt(Math.floor(param0));
"#)
    });
    builtins.insert(path_from(&["core", "as_flt"], strings), |_, _, _, _| {
        String::from(r#"
return Number(param0);
"#)
    });
    builtins.insert(path_from(&["core", "substring"], strings), |_, _, _, _| {
        String::from(r#"
let start_idx = param1;
if(param1 < 0) { start_idx = BigInt(param0.length) + param1; }
if(start_idx > param0.length) {
    gera___panic(`the start index ${param1} is out of bounds for a string of length ${param0.length}`);
}
let end_idx = param2;
if(param2 < 0) { end_idx = BigInt(param0.length) + param2; }
if(end_idx > param0.length) {
    gera___panic(`the end index ${param2} is out of bounds for a string of length ${param0.length}`);
}
if(start_idx > end_idx) {
    gera___panic(`the start index ${param1} is larger than the end index ${param2} (length of string is ${param0.length})`);
}
return gera___substring(param0, Number(start_idx), Number(end_idx));
"#)
    });
    builtins.insert(path_from(&["core", "concat"], strings), |_, _, _, _| {
        String::from(r#"
return param0 + param1;
"#)
    });
    builtins.insert(path_from(&["core", "string"], strings), |_, _, _, _| {
        String::from(r#"
return param0.repeat(Number(param1));
"#)
    });
    builtins.insert(path_from(&["core", "hash"], strings), |_, _, _, _| {
        String::from(r#"
return gera___hash(param0);
"#)
    });
    return builtins;
}

fn emit_procedure_impls(
    symbols: &Vec<IrSymbol>,
    types: &mut TypeMap,
    constants: &mut ConstantPool,
    strings: &mut StringMap,
    external: &HashMap<NamespacePath, StringIdx>,
    output: &mut String
) {
    let builtin_bodies = get_builtin_bodies(strings);
    for symbol in symbols {
        match symbol {
            IrSymbol::Procedure { path, variant, parameter_types, return_type: _, variables, body } => {
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
                    body, variables, types, constants, external, symbols, strings, &mut body_str
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
                    (parameter_types, *return_type, types, strings);
                indent(&body_str, output);
                output.push_str("}\n");
            }
            _ => {}
        }
    }
}

fn emit_constant_name(idx: usize, output: &mut String) {
    output.push_str("geraconstant");
    output.push_str(&idx.to_string());
}

fn emit_constant_declarations(
    types: &mut TypeMap,
    constants: &mut ConstantPool,
    external: &HashMap<NamespacePath, StringIdx>,
    symbols: &Vec<IrSymbol>,
    strings: &StringMap,
    output: &mut String
) {
    for vi in 0..constants.get_value_count() {
        match constants.get_value(vi).clone() {
            ConstantPoolValue::String(v) => {
                output.push_str("const ");
                emit_constant_name(vi, output);
                output.push_str(" = ");
                emit_string_literal(&v, output);
                output.push_str(";\n");
            }
            ConstantPoolValue::Array(values, _) => {
                output.push_str("const ");
                emit_constant_name(vi, output);
                output.push_str(" = [\n");
                for value in values.iter() {
                    output.push_str("    ");
                    emit_value(value, constants, output);
                    output.push_str(",\n");
                }
                output.push_str("];\n");
            }
            ConstantPoolValue::Object(members) => {
                output.push_str("const ");
                emit_constant_name(vi, output);
                output.push_str(" = {\n");
                for (member_name, (member_value, _)) in &members {
                    output.push_str("    ");
                    output.push_str(strings.get(*member_name));
                    output.push_str(": ");
                    emit_value(member_value, constants, output);
                    output.push_str(",\n");
                }
                output.push_str("};\n");
            }
            ConstantPoolValue::Closure(closure) => {
                let closure = closure.clone();
                output.push_str("const ");
                emit_constant_name(vi, output);
                output.push_str(" = {\n");
                output.push_str("    captures: { ");
                let mut had_capture = false;
                for (capture_name, capture_value) in &closure.captured {
                    if had_capture { output.push_str(", "); }
                    had_capture = true;
                    output.push_str(strings.get(*capture_name));
                    output.push_str(": ");
                    emit_value(capture_value, constants, output);
                }
                output.push_str(" },\n");
                let mut body_str = String::new();
                body_str.push_str("call: function(");
                let mut had_param = false;
                for p in 0..closure.parameter_types.len() {
                    if had_param { body_str.push_str(", "); }
                    had_param = true;
                    body_str.push_str("param");
                    body_str.push_str(&p.to_string());
                }
                body_str.push_str(") {\n");
                for variable_idx in 0..closure.variables.len() {
                    body_str.push_str("    let ");
                    emit_variable(IrVariable { index: variable_idx, version: 0 }, &mut body_str);
                    body_str.push_str(";\n");
                }
                let mut body_body_str = String::new();
                emit_block(
                    &closure.body, &closure.variables, types, constants, external, symbols, strings,
                    &mut body_body_str
                );
                indent(&body_body_str, &mut body_str);
                body_str.push_str("}\n");
                indent(&body_str, output);
                output.push_str("};\n");
            }
            ConstantPoolValue::Variant(_, _, _) => {}
        }
    }
}

fn emit_main_function(
    main_procedure_path: &NamespacePath,
    strings: &StringMap,
    output: &mut String
) {
    output.push_str("gera___stack.push(");
    emit_string_literal(&main_procedure_path.display(strings), output);
    output.push_str(", \"<entry point>\", 0);\n");
    emit_procedure_name(main_procedure_path, 0, strings, output);
    output.push_str("();\n");
}

fn emit_path(path: &NamespacePath, strings: &StringMap, output: &mut String) {
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
    value: &ConstantValue, constants: &ConstantPool, output: &mut String
) {
    match value {
        ConstantValue::Unit => output.push_str("undefined"),
        ConstantValue::Boolean(b) => output.push_str(if *b { "true" } else { "false" }),
        ConstantValue::Integer(i) => {
            output.push_str(&i.to_string());
            output.push_str("n");
        }
        ConstantValue::Float(f) => {
            if *f == f64::INFINITY { output.push_str("Infinity"); }
            else if *f == f64::NEG_INFINITY { output.push_str("-Infinity"); }
            else if f.is_nan() { output.push_str("NaN"); }
            else { output.push_str(&format!("{:.}", *f)); }
        }
        ConstantValue::String(s) => emit_constant_name(s.into(), output),
        ConstantValue::Array(a) => emit_constant_name(a.into(), output),
        ConstantValue::Object(o) => emit_constant_name(o.into(), output),
        ConstantValue::Closure(c) => emit_constant_name(c.into(), output),
        ConstantValue::Variant(v) => {
            let (variant_name, variant_value, _) = constants.get_variant(*v);
            output.push_str("{ tag: ");
            output.push_str(&variant_name.0.to_string());
            output.push_str(", value: ");
            emit_value(&variant_value, constants, output);
            output.push_str(" }");
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
    variable_types: &Vec<TypeGroup>,
    types: &mut TypeMap,
    constants: &ConstantPool,
    external: &HashMap<NamespacePath, StringIdx>,
    symbols: &Vec<IrSymbol>,
    strings: &StringMap,
    output: &mut String
) {
    output.push_str("{\n");
    for instruction in instructions {
        let mut o = String::new();
        emit_instruction(
            instruction, variable_types, types, constants, external, symbols, strings, &mut o
        );
        indent(&o, output);
    }
    output.push_str("}");
}

fn emit_copied(
    copied: &str,
    copied_type: TypeGroup,
    types: &mut TypeMap,
    output: &mut String
) {
    match types.group_concrete(copied_type) {
        Type::Any |
        Type::Unit |
        Type::Boolean |
        Type::Integer |
        Type::Float |
        Type::String |
        Type::Array(_) |
        Type::Object(_) |
        Type::ConcreteObject(_) |
        Type::Closure(_) => {
            output.push_str(copied);
        }
        Type::Variants(_) => {
            output.push_str("{ tag: ");
            output.push_str(copied);
            output.push_str(".tag, value: ");
            output.push_str(copied);
            output.push_str(".value }");
        }
    }
}

fn emit_copied_variable(
    copied: IrVariable,
    copied_type: TypeGroup,
    types: &mut TypeMap,
    output: &mut String
) {
    let mut copied_str = String::new();
    emit_variable(copied, &mut copied_str);
    emit_copied(&copied_str, copied_type, types, output);
}

fn emit_instruction(
    instruction: &IrInstruction,
    variable_types: &Vec<TypeGroup>,
    types: &mut TypeMap,
    constants: &ConstantPool,
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
            output.push_str(" = {\n");
            for (member_name, member_value) in member_values {
                output.push_str("    ");
                output.push_str(strings.get(*member_name));
                output.push_str(": ");
                emit_copied_variable(
                    *member_value, variable_types[member_value.index], types, output
                );
                output.push_str(",\n");
            }
            output.push_str("};\n");
        }
        IrInstruction::LoadArray { element_values, into } => {
            emit_variable(*into, output);
            output.push_str(" = [\n");
            for element_value in element_values {
                output.push_str("    ");
                emit_copied_variable(
                    *element_value, variable_types[element_value.index], types, output
                );
                output.push_str(",\n");
            }
            output.push_str("];\n");
        }
        IrInstruction::LoadVariant { name, v, into } => {
            emit_variable(*into, output);
            output.push_str(" = { tag: ");
            output.push_str(&name.0.to_string());
            output.push_str(", value: ");
            emit_copied_variable(*v, variable_types[v.index], types, output);
            output.push_str(" };\n");
        }
        IrInstruction::LoadGlobalVariable { path, into } => {
            if let Type::Unit = types.group_concrete(variable_types[into.index]) { return; }
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
                    panic!("Should've been converted to 'IrInstruction::LoadProcedure'!");
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
            parameter_types, return_type: _, captured, variables, body, into
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
                    *capture_value, variable_types[capture_value.index], types, output
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
                body, variables, types, constants, external, symbols, strings, &mut body_body_str
            );
            indent(&body_body_str, &mut body_str);
            body_str.push_str("}\n");
            indent(&body_str, output);
            output.push_str("};\n");
        }
        IrInstruction::LoadValue { value, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_value(&value, constants, output);
            output.push_str(";\n");
        }
        IrInstruction::GetObjectMember { accessed, member, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            let mut accessed_str = String::new();
            emit_variable(*accessed, &mut accessed_str);
            accessed_str.push_str(".");
            accessed_str.push_str(strings.get(*member));
            emit_copied(&accessed_str, variable_types[into.index], types, output);
            output.push_str(";\n");
        }
        IrInstruction::SetObjectMember { value, accessed, member } => {
            emit_variable(*accessed, output);
            output.push_str(".");
            output.push_str(strings.get(*member));
            output.push_str(" = ");
            emit_copied_variable(*value, variable_types[value.index], types, output);
            output.push_str(";\n");
        }
        IrInstruction::GetArrayElement { accessed, index, into, source } => {
            let mut index_str = String::new();
            emit_variable(*index, &mut index_str);
            let mut accessed_str = String::new();
            emit_variable(*accessed, &mut accessed_str);
            output.push_str(&index_str);
            output.push_str(" = gera___verify_index(");
            output.push_str(&index_str);
            output.push_str(", ");
            output.push_str(&accessed_str);
            output.push_str(".length, ");
            emit_string_literal(strings.get(source.file_name()), output);
            output.push_str(", ");
            let source_line = strings.get(source.file_content()).chars()
                .take(source.start_position()).collect::<String>()
                .lines().collect::<Vec<&str>>().len();
            output.push_str(&source_line.to_string());
            output.push_str(");\n");
            accessed_str.push_str("[");
            accessed_str.push_str(&index_str);
            accessed_str.push_str("]");
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_copied(&accessed_str, variable_types[into.index], types, output);
            output.push_str(";\n");
        }
        IrInstruction::SetArrayElement { value, accessed, index, source } => {
            let mut index_str = String::new();
            emit_variable(*index, &mut index_str);
            let mut accessed_str = String::new();
            emit_variable(*accessed, &mut accessed_str);
            output.push_str(&index_str);
            output.push_str(" = gera___verify_index(");
            output.push_str(&index_str);
            output.push_str(", ");
            output.push_str(&accessed_str);
            output.push_str(".length, ");
            emit_string_literal(strings.get(source.file_name()), output);
            output.push_str(", ");
            let source_line = strings.get(source.file_content()).chars()
                .take(source.start_position()).collect::<String>()
                .lines().collect::<Vec<&str>>().len();
            output.push_str(&source_line.to_string());
            output.push_str(");\n");
            output.push_str(&accessed_str);
            output.push_str("[");
            output.push_str(&index_str);
            output.push_str("] = ");
            emit_copied_variable(*value, variable_types[value.index], types, output);
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
            emit_copied_variable(*value, variable_types[value.index], types, output);
            output.push_str(";\n");
        }
        IrInstruction::Move { from, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_copied_variable(*from, variable_types[from.index], types, output);
            output.push_str(";\n");
        }
        IrInstruction::Add { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            if let Type::Integer = types.group_concrete(variable_types[a.index]) {
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
            if let Type::Integer = types.group_concrete(variable_types[a.index]) {
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
            if let Type::Integer = types.group_concrete(variable_types[a.index]) {
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
        IrInstruction::Divide { a, b, into, source } => {    
            let mut divisor = String::new();
            emit_variable(*b, &mut divisor);
            if let Type::Integer = types.group_concrete(variable_types[a.index]) {
                output.push_str("gera___verify_integer_divisor(");
                output.push_str(&divisor);
                output.push_str(", ");
                emit_string_literal(strings.get(source.file_name()), output);
                output.push_str(", ");
                let source_line = strings.get(source.file_content()).chars()
                    .take(source.start_position()).collect::<String>()
                    .lines().collect::<Vec<&str>>().len();
                output.push_str(&source_line.to_string());
                output.push_str(");\n");
                emit_variable(*into, output);
                output.push_str(" = ");
                output.push_str("BigInt.asIntN(64, ");
                emit_variable(*a, output);
                output.push_str(" / ");
                emit_variable(*b, output);
                output.push_str(")");
            } else {
                emit_variable(*into, output);
                output.push_str(" = ");
                emit_variable(*a, output);
                output.push_str(" / ");
                emit_variable(*b, output);
            }
            output.push_str(";\n");
        }
        IrInstruction::Modulo { a, b, into, source } => {
            let mut divisor = String::new();
            emit_variable(*b, &mut divisor);
            if let Type::Integer = types.group_concrete(variable_types[a.index]) {
                output.push_str("gera___verify_integer_divisor(");
                output.push_str(&divisor);
                output.push_str(", ");
                emit_string_literal(strings.get(source.file_name()), output);
                output.push_str(", ");
                let source_line = strings.get(source.file_content()).chars()
                    .take(source.start_position()).collect::<String>()
                    .lines().collect::<Vec<&str>>().len();
                output.push_str(&source_line.to_string());
                output.push_str(");\n");
                emit_variable(*into, output);
                output.push_str(" = ");
                output.push_str("BigInt.asIntN(64, ");
                emit_variable(*a, output);
                output.push_str(" % ");
                emit_variable(*b, output);
                output.push_str(")");
            } else {
                emit_variable(*into, output);
                output.push_str(" = ");
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
            output.push_str(" = gera___eq(");
            emit_variable(*a, output);
            output.push_str(", ");
            emit_variable(*b, output);
            output.push_str(");\n");
        }
        IrInstruction::NotEquals { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = !gera___eq(");
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
            output.push_str("switch(");
            emit_variable(*value, output);
            output.push_str(") {\n");
            for (branch_value, branch_body) in branches {
                output.push_str("    case ");
                emit_value(&branch_value, constants, output);
                output.push_str(":\n");
                let mut branch = String::new();
                for instruction in branch_body {
                    emit_instruction(
                        instruction, variable_types, types, constants, external, symbols, strings,
                        &mut branch
                    );
                }
                let mut branch_indented = String::new();
                indent(&branch, &mut branch_indented);
                indent(&branch_indented, output);
                output.push_str("        break;\n");
            }
            if else_branch.len() > 0 {
                output.push_str("    default:\n");
                let mut else_branch_str = String::new();
                for instruction in else_branch {
                    emit_instruction(
                        instruction, variable_types, types, constants, external, symbols, strings,
                        &mut else_branch_str
                    );
                }
                let mut else_branch_indented = String::new();
                indent(&else_branch_str, &mut else_branch_indented);
                indent(&else_branch_indented, output);
            }
            output.push_str("}\n");
        }
        IrInstruction::BranchOnVariant { value, branches, else_branch } => {
            let mut matched = String::new();
            emit_variable(*value, &mut matched);
            output.push_str("switch(");
            output.push_str(&matched);
            output.push_str(".tag) {\n");
            for (branch_variant, branch_variable, branch_body) in branches {
                output.push_str("    case ");
                output.push_str(&branch_variant.0.to_string());
                output.push_str(":\n");
                let mut branch = String::new();
                if let Some(branch_variable) = branch_variable {
                    emit_variable(*branch_variable, &mut branch);
                    branch.push_str(" = ");
                    branch.push_str(&matched);
                    branch.push_str(".value;\n");
                }
                for instruction in branch_body {
                    emit_instruction(
                        instruction, variable_types, types, constants, external, symbols, strings,
                        &mut branch
                    );
                }
                let mut branch_indented = String::new();
                indent(&branch, &mut branch_indented);
                indent(&branch_indented, output);
                output.push_str("        break;\n");
            }
            if else_branch.len() > 0 {
                output.push_str("    default:\n");
                let mut else_branch_str = String::new();
                for instruction in else_branch {
                    emit_instruction(
                        instruction, variable_types, types, constants, external, symbols, strings,
                        &mut else_branch_str
                    );
                }
                let mut else_branch_indented = String::new();
                indent(&else_branch_str, &mut else_branch_indented);
                indent(&else_branch_indented, output);
            }
            output.push_str("}\n");
        }
        IrInstruction::Call { path, variant, arguments, into, source } => {
            output.push_str("gera___stack.push(");
            emit_string_literal(&path.display(strings), output);
            output.push_str(", ");
            emit_string_literal(strings.get(source.file_name()), output);
            output.push_str(", ");
            let source_line = strings.get(source.file_content()).chars()
                .take(source.start_position()).collect::<String>()
                .lines().collect::<Vec<&str>>().len();
            output.push_str(&source_line.to_string());
            output.push_str(");\n");
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
                    output
                );
            }
            output.push_str(");\n");
            output.push_str("gera___stack.pop();\n");
        }
        IrInstruction::CallClosure { called, arguments, into, source } => {
            output.push_str("gera___stack.push(\"<closure>\", ");
            emit_string_literal(strings.get(source.file_name()), output);
            output.push_str(", ");
            let source_line = strings.get(source.file_content()).chars()
                .take(source.start_position()).collect::<String>()
                .lines().collect::<Vec<&str>>().len();
            output.push_str(&source_line.to_string());
            output.push_str(");\n");
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
                    output
                );
            }
            output.push_str(");\n");
            output.push_str("gera___stack.pop();\n");
        }
        IrInstruction::Return { value } => {
            output.push_str("return ");
            emit_variable(*value, output);
            output.push_str(";\n");
        }
        IrInstruction::Phi { .. } => {}
    }
}

