
use std::collections::HashMap;

use crate::backend::{
    ir::{IrSymbol, IrTypeBank, IrType, IrInstruction, IrVariable},
    interpreter::Value
};
use crate::frontend::modules::NamespacePath;
use crate::util::strings::{StringMap, StringIdx};

pub fn generate_c(
    symbols: Vec<IrSymbol>,
    types: IrTypeBank,
    main_procedure_path: NamespacePath,
    strings: &mut StringMap
) -> String {
    let mut output = String::new();
    let mut external = HashMap::new();
    emit_core_library(&mut output);
    output.push_str("\n");
    emit_type_declarations(&types, &mut output);
    output.push_str("\n");
    emit_procedure_pointers(&types, &mut output);
    output.push_str("\n");
    emit_type_members(&types, strings, &mut output);
    output.push_str("\n");
    emit_conversion_functions(&types, &mut output);
    output.push_str("\n");
    emit_symbol_declarations(&symbols, &types, strings, &mut external, &mut output);
    output.push('\n');
    emit_procedure_impls(&symbols, &types, strings, &mut external, &mut output);
    output.push('\n');
    emit_main_function(&main_procedure_path, strings, &mut output);
    return output;
}

fn emit_core_library(
    output: &mut String
) {
    let core_library: Box<[u8]> = include_bytes!("./gen_libs/core.c").clone().into();
    output.push_str(&String::from_utf8(core_library.into()).expect("should be valid"));
    output.push_str("\n");
}

fn emit_object_name(index: usize, output: &mut String) {
    output.push_str("GeraObject");
    output.push_str(&index.to_string());
} 

fn emit_concrete_object_name(index: usize, output: &mut String) {
    output.push_str("GeraConcreteObject");
    output.push_str(&index.to_string());
} 

fn emit_variants_name(index: usize, output: &mut String) {
    output.push_str("GeraVariants");
    output.push_str(&index.to_string());
} 

fn emit_procedure_ptr_name(index: usize, output: &mut String) {
    output.push_str("GeraProcPtr");
    output.push_str(&index.to_string());
} 

fn emit_type_declarations(
    types: &IrTypeBank,
    output: &mut String
) {
    // arrays are not needed
    for object_idx in 0..types.get_all_objects().len() {
        output.push_str("typedef struct ");
        emit_object_name(object_idx, output);
        emit_object_name(object_idx, output);
        output.push_str(";\n");
    }
    for concrete_object_idx in 0..types.get_all_concrete_objects().len() {
        output.push_str("typedef struct ");
        emit_concrete_object_name(concrete_object_idx, output);
        emit_concrete_object_name(concrete_object_idx, output);
        output.push_str(";\n");
    }
    for variants_idx in 0..types.get_all_variants().len() {
        output.push_str("typedef struct ");
        emit_variants_name(variants_idx, output);
        output.push_str(" ");
        emit_variants_name(variants_idx, output);
        output.push_str(";\n");
    }
    // procedure pointers are done later
    // indirects are not needed
}

fn emit_procedure_pointers(
    types: &IrTypeBank,
    output: &mut String
) {
    for proc_ptr_idx in 0..types.get_all_procedure_ptrs().len() {
        output.push_str("typedef ");
        let proc_type = &types.get_all_procedure_ptrs()[proc_ptr_idx];
        emit_type(proc_type.1, types, output);
        output.push_str("(*");
        emit_procedure_ptr_name(proc_ptr_idx, output);
        output.push_str(")(");
        for arg_idx in 0..proc_type.0.len() {
            if arg_idx >= 1 { output.push_str(", "); }
            emit_type(proc_type.0[arg_idx], types, output);
        }
        output.push_str(");\n");
    }
}

fn emit_type_members(
    types: &IrTypeBank,
    strings: &StringMap,
    output: &mut String
) {
    for object_idx in 0..types.get_all_objects().len() {
        output.push_str("typedef struct ");
        emit_object_name(object_idx, output);
        output.push_str(" {");
        for (member_name, member_type) in &types.get_all_objects()[object_idx] {
            output.push_str("\n    ");
            emit_type(*member_type, types, output);
            output.push_str("* ");
            output.push_str(strings.get(*member_name));
            output.push_str(";");
        }
        output.push_str("\n} ");
        emit_object_name(object_idx, output);
        output.push_str(";\n");
    }
    for concrete_object_idx in 0..types.get_all_concrete_objects().len() {
        output.push_str("typedef struct ");
        emit_concrete_object_name(concrete_object_idx, output);
        output.push_str(" {");
        for (member_name, member_type) in &types.get_all_concrete_objects()[concrete_object_idx] {
            output.push_str("\n    ");
            emit_type(*member_type, types, output);
            output.push_str(" ");
            output.push_str(strings.get(*member_name));
            output.push_str(";");
        }
        output.push_str("\n} ");
        emit_concrete_object_name(concrete_object_idx, output);
        output.push_str(";\n");
    }
    for variants_idx in 0..types.get_all_variants().len() {
        let has_values = types.get_all_variants()[variants_idx].iter().filter(|(_, vt)|
                if let IrType::Unit = vt.direct(types) { false } else { true }
            ).next().is_some();
        if has_values {
            output.push_str("typedef union ");
            emit_variants_name(variants_idx, output);
            output.push_str("Value {");
            for (variant_name, variant_type) in &types.get_all_variants()[variants_idx] {
                if let IrType::Unit = variant_type.direct(types) { continue; }
                output.push_str("\n    ");
                emit_type(*variant_type, types, output);
                output.push_str(" ");
                output.push_str(strings.get(*variant_name));
                output.push_str(";");
            }
            output.push_str("\n} ");
            emit_variants_name(variants_idx, output);
            output.push_str("Value;\n");
        }
        output.push_str("typedef struct ");
        emit_variants_name(variants_idx, output);
        output.push_str(" {\n    unsigned long long int tag;");
        if has_values {
            output.push_str("\n    ");
            emit_variants_name(variants_idx, output);
            output.push_str("Value value;");
        }
        output.push_str("\n} ");
        emit_variants_name(variants_idx, output);
        output.push_str(";\n");
    }
}

fn emit_variant_conversion_name(
    from: usize,
    to: usize,
    output: &mut String
) {
    output.push_str("geravariant");
    output.push_str(&from.to_string());
    output.push_str("into");
    output.push_str(&to.to_string());
}

fn emit_conversion_functions(
    types: &IrTypeBank,
    output: &mut String
) {
    fn variant_is_subset(major: usize, minor: usize, types: &IrTypeBank) -> bool {
        if major == minor { return false; }
        for (variant_name, variant_type) in &types.get_all_variants()[minor] {
            if let Some(major_variant_type) = types.get_all_variants()[major].get(variant_name) {
                if !variant_type.eq(major_variant_type, types) { return false; }
            } else { return false; }
        }
        return true;
    }
    for to_variants_idx in 0..types.get_all_variants().len() {
        let to_has_values = types.get_all_variants()[to_variants_idx].iter().filter(|(_, vt)|
                if let IrType::Unit = vt.direct(types) { false } else { true }
            ).next().is_some();
        for from_variants_idx in 0..types.get_all_variants().len() {
            let from_has_values = types.get_all_variants()[from_variants_idx].iter().filter(|(_, vt)|
                if let IrType::Unit = vt.direct(types) { false } else { true }
            ).next().is_some();
            if !variant_is_subset(to_variants_idx, from_variants_idx, types) { continue; }
            emit_variants_name(to_variants_idx, output);
            output.push_str(" ");
            emit_variant_conversion_name(from_variants_idx, to_variants_idx, output);
            output.push_str("(");
            emit_variants_name(from_variants_idx, output);
            output.push_str(" from) {\n    ");
            emit_variants_name(to_variants_idx, output);
            output.push_str(" to;");
            if to_has_values && from_has_values {
                output.push_str("\n    for(unsigned long long int b = 0; b < sizeof(");
                emit_variants_name(from_variants_idx, output);
                output.push_str("Value); b += 1) {\n        ((char*) &to.value)[b] = ((char*) &from.value)[b];\n    }");
            }
            output.push_str("\n    to.tag = from.tag;\n    return to;\n}\n");
        }
    }
}

fn emit_symbol_declarations(
    symbols: &Vec<IrSymbol>,
    types: &IrTypeBank,
    strings: &StringMap,
    external: &mut HashMap<NamespacePath, StringIdx>,
    output: &mut String
) {
    for symbol in symbols {
        match symbol {
            IrSymbol::Procedure { path, variant, parameter_types, return_type, .. } => {
                emit_type(*return_type, types, output);
                output.push_str(" ");
                emit_procedure_name(path, *variant, strings, output);
                output.push_str("(");
                for p in 0..parameter_types.len() {
                    if p > 0 { output.push_str(", "); }
                    emit_type(parameter_types[p], types, output);
                    output.push_str(" param");
                    output.push_str(&p.to_string());
                }
                output.push_str(");\n");
            }
            IrSymbol::ExternalProcedure { path, backing, parameter_types, return_type } => {
                external.insert(path.clone(), *backing);
                output.push_str("extern ");
                emit_type(*return_type, types, output);
                output.push_str(" ");
                output.push_str(strings.get(*backing));
                output.push_str("(");
                for p in 0..parameter_types.len() {
                    if p > 0 { output.push_str(", "); }
                    emit_type(parameter_types[p], types, output);
                    output.push_str(" param");
                    output.push_str(&p.to_string());
                }
                output.push_str(");\n");
            }
            IrSymbol::Variable { path, value_type, value } => {
                if let IrType::Unit = value_type.direct(types) { continue; }
                output.push_str("const ");
                emit_type(*value_type, types, output);
                output.push_str(" ");
                emit_path(path, strings, output);
                output.push_str(" = ");
                emit_value(value, output);
                output.push_str(";\n");
            }
            IrSymbol::ExternalVariable { path, backing, value_type } => {
                if let IrType::Unit = value_type.direct(types) { continue; }
                external.insert(path.clone(), *backing);
                output.push_str("extern const ");
                emit_type(*value_type, types, output);
                output.push_str(" ");
                output.push_str(strings.get(*backing));
                output.push_str(";\n");
            }
        }
    }
}

fn emit_procedure_impls(
    symbols: &Vec<IrSymbol>,
    types: &IrTypeBank,
    strings: &StringMap,
    external: &mut HashMap<NamespacePath, StringIdx>,
    output: &mut String
) {
    for symbol in symbols {
        if let IrSymbol::Procedure { path, variant, parameter_types, return_type, variables, body }
            = symbol {
            emit_type(*return_type, types, output);
            output.push_str(" ");
            emit_procedure_name(path, *variant, strings, output);
            output.push_str("(");
            for p in 0..parameter_types.len() {
                if p > 0 { output.push_str(", "); }
                emit_type(parameter_types[p], types, output);
                output.push_str(" param");
                output.push_str(&p.to_string());
            }
            output.push_str(") {");
            for variable_idx in 0..variables.len() {
                if let IrType::Unit = variables[variable_idx].direct(types) { continue; }
                output.push_str("\n    ");
                emit_type(variables[variable_idx], types, output);
                output.push_str(" ");
                emit_variable(IrVariable { index: variable_idx, version: 0 }, output);
                output.push_str(";");
            }
            let mut body_str = String::new();
            emit_block(body, variables, types, external, symbols, strings, &mut body_str);
            indent(&body_str, output, true);
            output.push_str("\n}\n");
        }
    }
}

fn emit_main_function(
    main_procedure_path: &NamespacePath,
    strings: &StringMap,
    output: &mut String
) {
    output.push_str("int main(void) {\n    ");
    emit_procedure_name(main_procedure_path, 0, strings, output);
    output.push_str("();\n    return 0;\n}\n");
}

fn emit_type(
    t: IrType,
    types: &IrTypeBank,
    output: &mut String
) {
    match t {
        IrType::Unit => output.push_str("void"),
        IrType::Boolean => output.push_str("char"),
        IrType::Integer => output.push_str("long long int"),
        IrType::Float => output.push_str("double"),
        IrType::String => output.push_str("char*"),
        IrType::Array(a) => {
            emit_type(*types.get_array(a), types, output);
            output.push_str("*");
        }
        IrType::Object(o) => emit_object_name(o.0, output),
        IrType::ConcreteObject(o) => emit_concrete_object_name(o.0, output),
        IrType::Variants(v) => emit_variants_name(v.0, output),
        IrType::ProcedurePtr(p) => emit_procedure_ptr_name(p.0, output),
        IrType::Indirect(i) => emit_type(*types.get_indirect(i), types, output)
    }
}

fn emit_path(
    path: &NamespacePath,
    strings: &StringMap,
    output: &mut String
) {
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

fn emit_string_literal(
    value: &str,
    output: &mut String
) {
    output.push('"');
    output.push_str(
        &value.replace("\n", "\\n")
            .replace("\r", "\\r")
            .replace("\"", "\\\"")
    );
    output.push('"');
}

fn emit_value(
    value: &Value,
    output: &mut String
) {
    match value {
        Value::Unit => panic!("should not have to emit unit value!"),
        Value::Boolean(b) => output.push_str(if *b { "1" } else { "0" }),
        Value::Integer(i) => output.push_str(&i.to_string()),
        Value::Float(f) => output.push_str(&format!("{:.}", *f)),
        Value::String(s) => emit_string_literal(std::rc::Rc::as_ref(s), output),
        Value::Array(_) => todo!("emit Value::Array"),
        Value::Object(_) => todo!("emit Value::Object"),
        Value::Closure(_, _, _) => todo!("emit Value::Closure"),
        Value::Variant(_, _) => todo!("emit Value::Variant"),
    }
}

fn indent(
    indent: &str,
    output: &mut String,
    mut indent_start: bool
) {
    for line in indent.lines() {
        if indent_start { output.push_str("\n    "); }
        if !indent_start { indent_start = true; }
        output.push_str(line);
    }
}

fn emit_block(
    instructions: &Vec<IrInstruction>,
    variable_types: &Vec<IrType>,
    types: &IrTypeBank,
    external: &HashMap<NamespacePath, StringIdx>,
    symbols: &Vec<IrSymbol>,
    strings: &StringMap,
    output: &mut String
) {
    output.push_str("{");
    for instruction in instructions {
        let mut o = String::new();
        emit_instruction(instruction, variable_types, types, external, symbols, strings, &mut o);
        indent(&o, output, true);
    }
    output.push_str("\n}");
}

fn emit_variable(
    variable: IrVariable,
    output: &mut String
) {
    output.push_str("local");
    output.push_str(&variable.index.to_string());
}

fn emit_variable_poly(
    variable: &str,
    from_type: IrType,
    to_type: IrType,
    types: &IrTypeBank,
    output: &mut String
) {
    if from_type.eq(&to_type, types) {
        output.push_str(variable);
        return;
    }
    if let IrType::Variants(to_variants_idx) = to_type.direct(types) {
        let from_variants_idx = if let IrType::Variants(v) = from_type.direct(types) { v }
            else { panic!("should be a variant"); };
        emit_variant_conversion_name(from_variants_idx.0, to_variants_idx.0, output);
        output.push_str("(");
        output.push_str(variable);
        output.push_str(")");
        return;
    }
    todo!("implicit conversions between objects and concrete objects");
}

fn emit_equality(
    a: &str,
    b: &str,
    compared_types: IrType,
    types: &IrTypeBank,
    output: &mut String
) {
    match compared_types.direct(types) {
        IrType::Unit => {
            output.push_str("1");
        }
        IrType::String => {
            output.push_str("gera___string_eq(");
            output.push_str(a);
            output.push_str(", ");
            output.push_str(b);
            output.push_str(")");
        }
        IrType::Array(_) => todo!("emit array comparison"),
        IrType::Object(_) => todo!("emit object comparison"),
        IrType::ConcreteObject(_) => todo!("emit concrete object comparison"),
        IrType::Variants(_) => todo!("emit variant comparison"),
        IrType::Indirect(_) => panic!("should be direct"),
        _ => {
            output.push_str(a);
            output.push_str(" == ");
            output.push_str(b);
        }
    }
}

fn emit_instruction(
    instruction: &IrInstruction,
    variable_types: &Vec<IrType>,
    types: &IrTypeBank,
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
            output.push_str(if *value { "1" } else { "0" });
            output.push_str(";\n");
        }
        IrInstruction::LoadInteger { value, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            output.push_str(&value.to_string());
            output.push_str(";\n");
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
        IrInstruction::LoadObject { member_values, into } => todo!("emit object literals"),
        IrInstruction::LoadArray { element_values, into } => todo!("emit array literals"),
        IrInstruction::LoadVariant { name, v, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            let variant_idx = if let IrType::Variants(v)
                = variable_types[into.index].direct(types) { v.0 }
                else { panic!("should be a variant"); };
            output.push_str("(");
            emit_variants_name(variant_idx, output);
            output.push_str(") {\n    .tag = ");
            output.push_str(&name.0.to_string());
            if let IrType::Unit = variable_types[v.index].direct(types) {} else {
                output.push_str(",\n    .value = { .");
                output.push_str(strings.get(*name));
                output.push_str(" = ");
                emit_variable(*v, output);
                output.push_str(" }");
            }
            output.push_str("\n};\n");
        }
        IrInstruction::LoadGlobalVariable { path, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            emit_variable(*into, output);
            output.push_str(" = ");
            if let Some(backing) = external.get(path) {
                output.push_str(strings.get(*backing));
            } else {
                emit_path(path, strings, output);
            }
            output.push_str(";\n");
        }
        IrInstruction::LoadProcedurePtr { path, variant, into } => {
            emit_variable(*into, output);
            output.push_str(" = &");
            emit_procedure_name(path, *variant, strings, output);
            output.push_str(";\n");
        }
        IrInstruction::LoadParameter { index, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            emit_variable(*into, output);
            output.push_str(" = param");
            output.push_str(&index.to_string());
            output.push_str(";\n");
        }
        IrInstruction::GetObjectMember { accessed, member, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_variable(*accessed, output);
            output.push_str("->");
            output.push_str(strings.get(*member));
            output.push_str(";\n");
        }
        IrInstruction::SetObjectMember { value, accessed, member } => {
            if let IrType::Unit = variable_types[value.index].direct(types) { return; }
            emit_variable(*accessed, output);
            output.push_str("->");
            output.push_str(strings.get(*member));
            output.push_str(" = ");
            emit_variable(*value, output);
            output.push_str(";\n");
        }
        IrInstruction::GetArrayElement { accessed, index, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_variable(*accessed, output);
            output.push_str("[");
            emit_variable(*index, output);
            output.push_str("];\n");
        }
        IrInstruction::SetArrayElement { value, accessed, index } => {
            if let IrType::Unit = variable_types[value.index].direct(types) { return; }
            emit_variable(*accessed, output);
            output.push_str("[");
            emit_variable(*index, output);
            output.push_str("] = ");
            emit_variable(*value, output);
            output.push_str(";\n");
        }
        IrInstruction::Move { from, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_variable(*from, output);
            output.push_str(";\n");
        }
        IrInstruction::Add { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_variable(*a, output);
            output.push_str(" + ");
            emit_variable(*b, output);
            output.push_str(";\n");
        }
        IrInstruction::Subtract { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_variable(*a, output);
            output.push_str(" - ");
            emit_variable(*b, output);
            output.push_str(";\n");
        }
        IrInstruction::Multiply { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_variable(*a, output);
            output.push_str(" * ");
            emit_variable(*b, output);
            output.push_str(";\n");
        }
        IrInstruction::Divide { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_variable(*a, output);
            output.push_str(" / ");
            emit_variable(*b, output);
            output.push_str(";\n");
        }
        IrInstruction::Modulo { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            if let IrType::Float = variable_types[a.index].direct(types) {
                output.push_str("gera___float_mod(");
                emit_variable(*a, output);
                output.push_str(", ");
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
            output.push_str(" = ");
            let compared_types = variable_types[a.index].direct(types);
            let mut a_str = String::new();
            emit_variable(*a, &mut a_str);
            let mut b_str = String::new();
            emit_variable(*b, &mut b_str);
            emit_equality(&a_str, &b_str, compared_types, types, output);
            output.push_str(";\n");
        }
        IrInstruction::NotEquals { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = !(");
            let compared_types = variable_types[a.index].direct(types);
            let mut a_str = String::new();
            emit_variable(*a, &mut a_str);
            let mut b_str = String::new();
            emit_variable(*b, &mut b_str);
            emit_equality(&a_str, &b_str, compared_types, types, output);
            output.push_str(");\n");
        }
        IrInstruction::Not { x, into } => {
            emit_variable(*into, output);
            output.push_str(" = !");
            emit_variable(*x, output);
            output.push_str(";\n");
        }
        IrInstruction::BranchOnValue { value, branches, else_branch } => {
            output.push_str("{");
            for branch_idx in 0..branches.len() {
                if let IrType::Unit = variable_types[value.index].direct(types) { continue; }
                output.push_str("\n    ");
                emit_type(variable_types[value.index], types, output);
                output.push_str(" branchval");
                output.push_str(&branch_idx.to_string());
                output.push_str(" = ");
                emit_value(&branches[branch_idx].0, output);
                output.push_str(";");
            }
            output.push_str("\n    ");
            let mut v_str = String::new();
            emit_variable(*value, &mut v_str);
            for branch_idx in 0..branches.len() {
                output.push_str("if(");
                let mut b_str = String::from("branchval");
                b_str.push_str(&branch_idx.to_string());
                emit_equality(&v_str, &b_str, variable_types[value.index], types, output);
                output.push_str(") ");
                let mut branch_str = String::new();
                emit_block(
                    &branches[branch_idx].1, variable_types, types, external, symbols, strings,
                    &mut branch_str
                );
                indent(&branch_str, output, false);
                output.push_str(" else ");
            }
            let mut else_branch_str = String::new();
            emit_block(else_branch, variable_types, types, external, symbols, strings, &mut else_branch_str);
            indent(&else_branch_str, output, false);
            output.push_str("\n}\n");
        }
        IrInstruction::BranchOnVariant { value, branches, else_branch } => {
            output.push_str("switch(");
            emit_variable(*value, output);
            output.push_str(".tag) {");
            let variant_idx = if let IrType::Variants(v)
                = variable_types[value.index].direct(types) { v.0 }
                else { panic!("should be a variant"); };
            for (branch_variant, branch_variable, branch_body) in branches {
                output.push_str("\n    case ");
                output.push_str(&branch_variant.0.to_string());
                output.push_str(":\n");
                if let IrType::Unit = variable_types[branch_variable.index].direct(types) {} else {
                    output.push_str("        ");
                    emit_variable(*branch_variable, output);
                    output.push_str(" = ");
                    emit_variable(*value, output);
                    output.push_str(".value.");
                    output.push_str(strings.get(*branch_variant));
                    output.push_str(";\n");
                }
                for instruction in branch_body {
                    output.push_str("        ");
                    emit_instruction(
                        instruction, variable_types, types, external, symbols, strings, output
                    );
                }
                output.push_str("        break;");
            }
            if else_branch.len() > 0 {
                output.push_str("\n    default:\n");
                for instruction in else_branch {
                    output.push_str("        ");
                    emit_instruction(
                        instruction, variable_types, types, external, symbols, strings, output
                    );
                }
            }
            output.push_str("\n}\n");
        }
        IrInstruction::Call { path, variant, arguments, into } => {
            // this is cursed and I hate it
            let (parameter_types, return_type) = match symbols.into_iter().filter(|s| match *s {
                IrSymbol::Procedure { path: p, variant: v, .. } => *path == *p && *variant == *v,
                IrSymbol::ExternalProcedure { path: p, .. } => *path == *p,
                _ => false
            }).next().expect("should exist") {
                IrSymbol::Procedure { parameter_types, return_type, .. } |
                IrSymbol::ExternalProcedure { parameter_types, return_type, .. } => (
                    parameter_types, return_type
                ),
                _ => panic!("should be a procedure")
            };
            // end of cursed part 
            let returns_value = if let IrType::Unit = return_type.direct(types) { false }
                else { true };
            let mut value = String::new();
            if let Some(backing) = external.get(path) {
                value.push_str(strings.get(*backing));
            } else {
                emit_procedure_name(path, *variant, strings, &mut value);
            }
            value.push_str("(");
            for argument_idx in 0..arguments.len() {
                if argument_idx >= 1 { value.push_str(", "); }
                let mut variable = String::new();
                emit_variable(arguments[argument_idx], &mut variable);
                emit_variable_poly(
                    &variable,
                    variable_types[arguments[argument_idx].index],
                    parameter_types[argument_idx],
                    types, &mut value
                );
            }
            value.push_str(")");
            if returns_value {
                emit_variable(*into, output);
                output.push_str(" = ");
                emit_variable_poly(
                    &value,
                    *return_type,
                    variable_types[into.index],
                    types, output
                );
            } else {
                output.push_str(&value);
            }
            output.push_str(";\n");
        }
        IrInstruction::CallPtr { called, arguments, into } => {
            let (parameter_types, return_type) = if let IrType::ProcedurePtr(p)
                    = variable_types[called.index].direct(types) {
                types.get_procedure_ptr(p)
            } else { panic!("should be a procedure pointer"); };
            let returns_value = if let IrType::Unit = return_type.direct(types) { false }
                else { true };
            let mut value = String::new();
            value.push_str("(");
            emit_variable(*called, &mut value);
            value.push_str(")(");
            for argument_idx in 0..arguments.len() {
                if argument_idx >= 1 { value.push_str(", "); }
                let mut variable = String::new();
                emit_variable(arguments[argument_idx], &mut variable);
                emit_variable_poly(
                    &variable,
                    variable_types[arguments[argument_idx].index],
                    parameter_types[argument_idx],
                    types, &mut value
                );
            }
            value.push_str(")");
            if returns_value {
                emit_variable(*into, output);
                output.push_str(" = ");
                emit_variable_poly(
                    &value,
                    *return_type,
                    variable_types[into.index],
                    types, output
                );
            } else {
                output.push_str(&value);
            }
            output.push_str(";\n");
        }
        IrInstruction::Return { value } => {
            output.push_str("return ");
            emit_variable(*value, output);
            output.push_str(";\n");
        }
        IrInstruction::Phi { .. } => {}
    }
}