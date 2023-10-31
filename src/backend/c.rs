
use std::collections::{HashMap, HashSet};

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
    emit_free_handler_functions(&types, strings, &mut output);
    output.push_str("\n");
    emit_symbol_declarations(&symbols, &types, strings, &mut external, &mut output);
    output.push('\n');
    emit_procedure_impls(&symbols, &types, strings, &mut external, &mut output);
    output.push('\n');
    emit_main_function(&main_procedure_path, strings, &mut output);
    return output;
}

fn emit_core_library(output: &mut String) {
    let gera_header: Box<[u8]> = include_bytes!("./gen_libs/gera.h").clone().into();
    output.push_str(&String::from_utf8(gera_header.into()).expect("should be valid"));
    output.push_str("\n");
    let core_library: Box<[u8]> = include_bytes!("./gen_libs/core.c").clone().into();
    output.push_str(&String::from_utf8(core_library.into()).expect("should be valid"));
    output.push_str("\n");
}

fn emit_object_name(index: usize, output: &mut String) {
    output.push_str("GeraObject");
    output.push_str(&index.to_string());
} 

fn emit_object_alloc_name(index: usize, output: &mut String) {
    output.push_str("GeraObjectAllocation");
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

fn emit_type_declarations(types: &IrTypeBank, output: &mut String) {
    // arrays are not needed
    for object_idx in 0..types.get_all_objects().len() {
        output.push_str("typedef struct ");
        emit_object_name(object_idx, output);
        output.push_str(" ");
        emit_object_name(object_idx, output);
        output.push_str(";\n");
        output.push_str("typedef struct ");
        emit_object_alloc_name(object_idx, output);
        output.push_str(" ");
        emit_object_alloc_name(object_idx, output);
        output.push_str(";\n");
    }
    for concrete_object_idx in 0..types.get_all_concrete_objects().len() {
        output.push_str("typedef struct ");
        emit_concrete_object_name(concrete_object_idx, output);
        output.push_str(" ");
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

fn emit_procedure_pointers(types: &IrTypeBank, output: &mut String) {
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

fn emit_type_members(types: &IrTypeBank, strings: &StringMap, output: &mut String) {
    for object_idx in 0..types.get_all_objects().len() {
        output.push_str("typedef struct ");
        emit_object_name(object_idx, output);
        output.push_str(" {\n    GeraAllocation* allocation;");
        for (member_name, member_type) in &types.get_all_objects()[object_idx] {
            output.push_str("\n    ");
            emit_type(*member_type, types, output);
            output.push_str("* member");
            output.push_str(&member_name.0.to_string());
            output.push_str(";");
        }
        output.push_str("\n} ");
        emit_object_name(object_idx, output);
        output.push_str(";\n");
        output.push_str("typedef struct ");
        emit_object_alloc_name(object_idx, output);
        output.push_str(" {");
        for (member_name, member_type) in &types.get_all_objects()[object_idx] {
            output.push_str("\n    ");
            emit_type(*member_type, types, output);
            output.push_str(" member");
            output.push_str(&member_name.0.to_string());
            output.push_str(";");
        }
        output.push_str("\n} ");
        emit_object_alloc_name(object_idx, output);
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

fn emit_variant_conversion_name(from: usize, to: usize, output: &mut String) {
    output.push_str("geravariant");
    output.push_str(&from.to_string());
    output.push_str("into");
    output.push_str(&to.to_string());
}

fn emit_object_conversion_name(from: usize, to: usize, output: &mut String) {
    output.push_str("geraobject");
    output.push_str(&from.to_string());
    output.push_str("into");
    output.push_str(&to.to_string());
}

fn emit_conversion_functions(types: &IrTypeBank, output: &mut String) {
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
    fn object_is_subset(major: usize, minor: usize, types: &IrTypeBank) -> bool {
        if major == minor { return false; }
        for (member_name, member_type) in &types.get_all_objects()[minor] {
            if let Some(major_member_type) = types.get_all_objects()[major].get(member_name) {
                if !member_type.eq(major_member_type, types) { return false; }
            } else { return false; }
        }
        return true;
    }
    for to_object_idx in 0..types.get_all_objects().len() {
        let to_has_values = types.get_all_objects()[to_object_idx].iter().filter(|(_, mt)|
                if let IrType::Unit = mt.direct(types) { false } else { true }
            ).next().is_some();
        for from_object_idx in 0..types.get_all_objects().len() {
            let from_has_values = types.get_all_objects()[from_object_idx].iter().filter(|(_, mt)|
                if let IrType::Unit = mt.direct(types) { false } else { true }
            ).next().is_some();
            if !object_is_subset(from_object_idx, to_object_idx, types) { continue; }
            if !to_has_values || !from_has_values { continue; }
            emit_object_name(to_object_idx, output);
            output.push_str(" ");
            emit_object_conversion_name(from_object_idx, to_object_idx, output);
            output.push_str("(");
            emit_object_name(from_object_idx, output);
            output.push_str(" from) {\n    return (");
            emit_object_name(to_object_idx, output);
            output.push_str(") {");
            for (member_name, _) in &types.get_all_objects()[to_object_idx] {
                output.push_str("\n        .member");
                output.push_str(&member_name.0.to_string());
                output.push_str(" = from.member");
                output.push_str(&member_name.0.to_string());
                output.push_str(",");
            }
            output.push_str("\n    };\n}\n");
        }
    }
}

fn emit_rc_incr(
    variable: &str,
    incr_type: IrType,
    types: &IrTypeBank,
    strings: &StringMap,
    output: &mut String
) {
    match incr_type {
        IrType::Unit => {}
        IrType::Boolean => {}
        IrType::Integer => {}
        IrType::Float => {}
        IrType::String => {}
        IrType::Array(_) |
        IrType::Object(_) => {
            output.push_str("gera___rc_incr(");
            output.push_str(variable);
            output.push_str(".allocation);\n");
        }
        IrType::ConcreteObject(_) => {}
        IrType::Variants(variant_idx) => {
            output.push_str("switch(");
            output.push_str(variable);
            output.push_str(".tag) {");
            for (variant_name, variant_type) in types.get_variants(variant_idx) {
                if let IrType::Unit = variant_type.direct(types) { continue; }
                output.push_str("\n    case ");
                output.push_str(&variant_name.0.to_string());
                output.push_str(":\n");
                let mut var_decr = String::new();
                emit_rc_incr(
                    &format!("{}.value.{}", variable, strings.get(*variant_name)),
                    *variant_type, types, strings, &mut var_decr
                );
                let mut var_decr_indented = String::new();
                indent(&var_decr, &mut var_decr_indented);
                indent(&var_decr_indented, output);
                output.push_str("        break;");
            }
            output.push_str("\n}\n");
        }
        IrType::ProcedurePtr(_) => {}
        IrType::Indirect(indirect_idx) => emit_rc_incr(
            variable, *types.get_indirect(indirect_idx), types, strings, output
        )
    }
}

fn emit_rc_decr(
    variable: &str,
    freed_type: IrType,
    types: &IrTypeBank,
    strings: &StringMap,
    output: &mut String
) {
    match freed_type {
        IrType::Unit => {}
        IrType::Boolean => {}
        IrType::Integer => {}
        IrType::Float => {}
        IrType::String => {}
        IrType::Array(_) |
        IrType::Object(_) => {
            output.push_str("gera___rc_decr(");
            output.push_str(variable);
            output.push_str(".allocation);\n");
        }
        IrType::ConcreteObject(_) => {}
        IrType::Variants(variant_idx) => {
            output.push_str("switch(");
            output.push_str(variable);
            output.push_str(".tag) {");
            for (variant_name, variant_type) in types.get_variants(variant_idx) {
                if let IrType::Unit = variant_type.direct(types) { continue; }
                output.push_str("\n    case ");
                output.push_str(&variant_name.0.to_string());
                output.push_str(":\n");
                let mut var_decr = String::new();
                emit_rc_decr(
                    &format!("{}.value.{}", variable, strings.get(*variant_name)),
                    *variant_type, types, strings, &mut var_decr
                );
                let mut var_decr_indented = String::new();
                indent(&var_decr, &mut var_decr_indented);
                indent(&var_decr_indented, output);
                output.push_str("        break;");
            }
            output.push_str("\n}\n");
        }
        IrType::ProcedurePtr(_) => {}
        IrType::Indirect(indirect_idx) => emit_rc_decr(
            variable, *types.get_indirect(indirect_idx), types, strings, output
        )
    }
}

fn emit_array_free_handler_name(array_idx: usize, output: &mut String) {
    output.push_str("gerafreearray");
    output.push_str(&array_idx.to_string());
}

fn emit_object_free_handler_name(object_idx: usize, output: &mut String) {
    output.push_str("gerafreeobject");
    output.push_str(&object_idx.to_string());
}

fn emit_free_handler_functions(types: &IrTypeBank, strings: &StringMap, output: &mut String) {
    for array_idx in 0..types.get_all_arrays().len() {
        let element_type = types.get_all_arrays()[array_idx];
        if let IrType::Unit = element_type.direct(types) {} else {
            output.push_str("void ");
            emit_array_free_handler_name(array_idx, output);
            output.push_str("(char* data, size_t size) {");
            output.push_str("\n    ");
            emit_type(element_type, types, output);
            output.push_str("* elements = (");
            emit_type(element_type, types, output);
            output.push_str("*) data;");
            output.push_str("\n    for(size_t i = 0; i < size; i += 1) {");
            output.push_str("\n        ");
            emit_type(element_type, types, output);
            output.push_str(" element = elements[i];\n");
            let mut element_rc_decr = String::new();
            emit_rc_decr("element", element_type, types, strings, &mut element_rc_decr);
            let mut element_rc_decr_indented = String::new();
            indent(&element_rc_decr, &mut element_rc_decr_indented);
            indent(&element_rc_decr_indented, output);
            output.push_str("\n    }\n}\n");
        }
    }
    for object_idx in 0..types.get_all_objects().len() {
        output.push_str("void ");
        emit_object_free_handler_name(object_idx, output);
        output.push_str("(char* data, size_t size) {");
        output.push_str("\n    ");
        emit_object_alloc_name(object_idx, output);
        output.push_str("* object = (");
        emit_object_alloc_name(object_idx, output);
        output.push_str("*) data;\n");
        for (member_name, member_type) in &types.get_all_objects()[object_idx] {
            let mut member_rc_decr = String::new();
            emit_rc_decr(
                &format!("object->member{}", member_name.0.to_string()),
                *member_type, types, strings, &mut member_rc_decr
            );
            indent(&member_rc_decr, output);
        }
        output.push_str("}\n");
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

fn emit_scope_decrements(
    free: &HashSet<usize>,
    variable_types: &Vec<IrType>,
    types: &IrTypeBank,
    strings: &StringMap,
    output: &mut String
) {
    for variable_idx in free {
        let mut variable = String::new();
        emit_variable(IrVariable { index: *variable_idx, version: 0 }, &mut variable);
        emit_rc_decr(&variable, variable_types[*variable_idx], types, strings, output);
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
            output.push_str(") {\n");
            for variable_idx in 0..variables.len() {
                if let IrType::Unit = variables[variable_idx].direct(types) { continue; }
                output.push_str("    ");
                emit_type(variables[variable_idx], types, output);
                output.push_str(" ");
                emit_variable(IrVariable { index: variable_idx, version: 0 }, output);
                output.push_str(";\n");
            }
            let mut body_str = String::new();
            let mut body_free = HashSet::new();
            emit_block(
                body, variables, &mut body_free, types, external, symbols, strings,
                &mut body_str
            );
            body_str.push_str("\n");
            emit_scope_decrements(&body_free, variables, types, strings, &mut body_str);
            indent(&body_str, output);
            output.push_str("}\n");
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

fn emit_type(t: IrType, types: &IrTypeBank, output: &mut String) {
    match t {
        IrType::Unit => output.push_str("void"),
        IrType::Boolean => output.push_str("char"),
        IrType::Integer => output.push_str("long long int"),
        IrType::Float => output.push_str("double"),
        IrType::String => output.push_str("char*"),
        IrType::Array(_) => output.push_str("GeraArray"),
        IrType::Object(o) => emit_object_name(o.0, output),
        IrType::ConcreteObject(o) => emit_concrete_object_name(o.0, output),
        IrType::Variants(v) => emit_variants_name(v.0, output),
        IrType::ProcedurePtr(p) => emit_procedure_ptr_name(p.0, output),
        IrType::Indirect(i) => emit_type(*types.get_indirect(i), types, output)
    }
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
        &value.replace("\n", "\\n")
            .replace("\r", "\\r")
            .replace("\"", "\\\"")
    );
    output.push('"');
}

fn emit_value(value: &Value, output: &mut String) {
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
    free: &mut HashSet<usize>,
    types: &IrTypeBank,
    external: &HashMap<NamespacePath, StringIdx>,
    symbols: &Vec<IrSymbol>,
    strings: &StringMap,
    output: &mut String
) {
    output.push_str("{\n");
    for instruction in instructions {
        let mut o = String::new();
        emit_instruction(instruction, variable_types, free, types, external, symbols, strings, &mut o);
        indent(&o, output);
    }
    output.push_str("}");
}

fn emit_variable(variable: IrVariable, output: &mut String) {
    output.push_str("local");
    output.push_str(&variable.index.to_string());
}

fn emit_variable_poly(
    variable: &str,
    mut from_type: IrType,
    mut to_type: IrType,
    types: &IrTypeBank,
    output: &mut String
) {
    from_type = from_type.direct(types);
    to_type = to_type.direct(types);
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
    if let IrType::Object(from_variants_idx) = from_type.direct(types) {
        if let IrType::Object(to_variants_idx) = to_type.direct(types) {
            emit_object_conversion_name(from_variants_idx.0, to_variants_idx.0, output);
            output.push_str("(");
            output.push_str(variable);
            output.push_str(")");
            return;
        }
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
    free: &mut HashSet<usize>,
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
        IrInstruction::LoadObject { member_values, into } => {
            let object_idx = if let IrType::Object(object_idx) = variable_types[into.index]
                .direct(types) {
                object_idx.0
            } else { panic!("should be an object"); };
            output.push_str("{\n    GeraAllocation* allocation = gera___rc_alloc(sizeof(");
            emit_object_alloc_name(object_idx, output);
            output.push_str("), &");
            emit_object_free_handler_name(object_idx, output);
            output.push_str(");\n    ");
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            output.push_str(&into_str);
            output.push_str(".allocation = allocation;\n    ");
            emit_object_alloc_name(object_idx, output);
            output.push_str("* object = (");
            emit_object_alloc_name(object_idx, output);
            output.push_str("*) allocation->data;\n");
            for (member_name, member_value) in member_values {
                let member_type = *types.get_all_objects()[object_idx].get(member_name)
                    .expect("member should exist");
                output.push_str("    object->member");
                output.push_str(&member_name.0.to_string());
                output.push_str(" = ");
                let mut member_value_str = String::new();
                emit_variable(*member_value, &mut member_value_str);
                emit_variable_poly(
                    &member_value_str, variable_types[member_value.index], member_type, types,
                    output
                );
                output.push_str(";\n");
                let mut member_value_incr_str = String::new();
                emit_rc_incr(
                    &member_value_str, variable_types[member_value.index], types, strings,
                    &mut member_value_incr_str
                );
                indent(&member_value_incr_str, output);
                output.push_str("    ");
                output.push_str(&into_str);
                output.push_str(".member");
                output.push_str(&member_name.0.to_string());
                output.push_str(" = &object->member");
                output.push_str(&member_name.0.to_string());
                output.push_str(";\n");
            }
            output.push_str("}\n");
            free.insert(into.index);
        }
        IrInstruction::LoadArray { element_values, into } => {
            let array_idx = if let IrType::Array(array_idx) = variable_types[into.index]
                .direct(types) {
                array_idx.0
            } else { panic!("should be an array"); };
            let element_type = types.get_all_arrays()[array_idx];
            output.push_str("{\n    GeraAllocation* allocation = gera___rc_alloc(sizeof(");
            emit_type(element_type, types, output);
            output.push_str(") * ");
            output.push_str(&element_values.len().to_string());
            output.push_str(", &");
            emit_array_free_handler_name(array_idx, output);
            output.push_str(");\n    ");
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            output.push_str(&into_str);
            output.push_str(".allocation = allocation;\n    ");
            output.push_str(&into_str);
            output.push_str(".data = allocation->data;\n    ");
            output.push_str(&into_str);
            output.push_str(".length = ");
            output.push_str(&element_values.len().to_string());
            output.push_str(";\n    ");
            emit_type(element_type, types, output);
            output.push_str("* elements = (");
            emit_type(element_type, types, output);
            output.push_str("*) allocation->data;\n");
            for value_idx in 0..element_values.len() {
                output.push_str("    elements[");
                output.push_str(&value_idx.to_string());
                output.push_str("] = ");
                let mut element_value_str = String::new();
                emit_variable(element_values[value_idx], &mut element_value_str);
                emit_variable_poly(
                    &element_value_str, variable_types[element_values[value_idx].index],
                    element_type, types, output
                );
                output.push_str(";\n");
                let mut element_value_incr_str = String::new();
                emit_rc_incr(
                    &element_value_str, variable_types[element_values[value_idx].index], types,
                    strings, &mut element_value_incr_str
                );
                indent(&element_value_incr_str, output);
            }
            output.push_str("}\n");
            free.insert(into.index);
        }
        IrInstruction::LoadVariant { name, v, into } => {
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            if into.version > 0 {
                emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            }
            output.push_str(&into_str);
            output.push_str(" = ");
            let variant_idx = if let IrType::Variants(v)
                = variable_types[into.index].direct(types) { v.0 }
                else { panic!("should be a variant"); };
            output.push_str("(");
            emit_variants_name(variant_idx, output);
            output.push_str(") { .tag = ");
            output.push_str(&name.0.to_string());
            if let IrType::Unit = variable_types[v.index].direct(types) {} else {
                output.push_str(", .value = { .");
                output.push_str(strings.get(*name));
                output.push_str(" = ");
                emit_variable(*v, output);
                output.push_str(" }");
            }
            output.push_str(" };\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
            free.insert(into.index);
        }
        IrInstruction::LoadGlobalVariable { path, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            if into.version > 0 {
                emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            }
            output.push_str(&into_str);
            output.push_str(" = ");
            if let Some(backing) = external.get(path) {
                output.push_str(strings.get(*backing));
            } else {
                emit_path(path, strings, output);
            }
            output.push_str(";\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
            free.insert(into.index);
        }
        IrInstruction::LoadProcedurePtr { path, variant, into } => {
            emit_variable(*into, output);
            output.push_str(" = &");
            emit_procedure_name(path, *variant, strings, output);
            output.push_str(";\n");
        }
        IrInstruction::LoadParameter { index, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            if into.version > 0 {
                emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            }
            output.push_str(&into_str);
            output.push_str(" = param");
            output.push_str(&index.to_string());
            output.push_str(";\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
            free.insert(into.index);
        }
        IrInstruction::GetObjectMember { accessed, member, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            let member_type = if let IrType::Object(object_idx) = variable_types[accessed.index]
                .direct(types) {
                *types.get_object(object_idx).get(member).expect("member should exist")
            } else { panic!("accessed should be an object"); };
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            if into.version > 0 {
                emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            }
            output.push_str(&into_str);
            output.push_str(" = ");
            let mut access_str = String::new();
            access_str.push_str("(*");
            emit_variable(*accessed, &mut access_str);
            access_str.push_str(".member");
            access_str.push_str(&member.0.to_string());
            access_str.push_str(")");
            emit_variable_poly(
                &mut access_str, member_type, variable_types[into.index], types, output
            );
            output.push_str(";\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
            free.insert(into.index);
        }
        IrInstruction::SetObjectMember { value, accessed, member } => {
            if let IrType::Unit = variable_types[value.index].direct(types) { return; }
            let member_type = if let IrType::Object(object_idx) = variable_types[accessed.index]
                .direct(types) {
                *types.get_object(object_idx).get(member).expect("member should exist")
            } else { panic!("accessed should be an object"); };
            let mut member_str = String::new();
            member_str.push_str("(*");
            emit_variable(*accessed, &mut member_str);
            member_str.push_str(".member");
            member_str.push_str(&member.0.to_string());
            member_str.push_str(")");
            emit_rc_decr(&member_str, member_type, types, strings, output);
            output.push_str(&member_str);
            output.push_str(" = ");
            let mut value_str = String::new();
            emit_variable(*value, &mut value_str);
            emit_variable_poly(&value_str, variable_types[value.index], member_type, types, output);
            output.push_str(";\n");
            emit_rc_incr(&member_str, member_type, types, strings, output);
        }
        IrInstruction::GetArrayElement { accessed, index, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            let element_type = if let IrType::Array(array_idx) = variable_types[accessed.index]
                .direct(types) {
                *types.get_array(array_idx)
            } else { panic!("should be an array"); };
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            if into.version > 0 {
                emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            }
            output.push_str(&into_str);
            output.push_str(" = ");
            let mut access_str = String::new();
            access_str.push_str("((");
            emit_type(element_type, types, &mut access_str);
            access_str.push_str("*) ");
            emit_variable(*accessed, &mut access_str);
            access_str.push_str(".data)[");
            emit_variable(*index, &mut access_str);
            access_str.push_str("]");
            emit_variable_poly(
                &mut access_str, element_type, variable_types[into.index], types, output
            );
            output.push_str(";\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
            free.insert(into.index);
        }
        IrInstruction::SetArrayElement { value, accessed, index } => {
            if let IrType::Unit = variable_types[value.index].direct(types) { return; }
            let element_type = if let IrType::Array(array_idx) = variable_types[accessed.index]
                .direct(types) {
                *types.get_array(array_idx)
            } else { panic!("should be an array"); };
            let mut element_str = String::new();
            element_str.push_str("((");
            emit_type(element_type, types, &mut element_str);
            element_str.push_str("*) ");
            emit_variable(*accessed, &mut element_str);
            element_str.push_str(".data)[");
            emit_variable(*index, &mut element_str);
            element_str.push_str("]");
            emit_rc_decr(&element_str, element_type, types, strings, output);
            output.push_str(&element_str);
            output.push_str(" = ");
            let mut value_str = String::new();
            emit_variable(*value, &mut value_str);
            emit_variable_poly(&value_str, variable_types[value.index], element_type, types, output);
            output.push_str(";\n");
            emit_rc_incr(&element_str, element_type, types, strings, output);
        }
        IrInstruction::Move { from, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            if *from == *into { return; }
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            if into.version > 0 {
                emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            }
            output.push_str(&into_str);
            output.push_str(" = ");
            emit_variable(*from, output);
            output.push_str(";\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
            free.insert(into.index);
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
                    &branches[branch_idx].1, variable_types, free, types, external, symbols, 
                    strings, &mut branch_str
                );
                indent(&branch_str, output);
                output.push_str(" else ");
            }
            let mut else_branch_str = String::new();
            emit_block(
                else_branch, variable_types, free, types, external, symbols, strings,
                &mut else_branch_str
            );
            indent(&else_branch_str, output);
            output.push_str("\n}\n");
        }
        IrInstruction::BranchOnVariant { value, branches, else_branch } => {
            output.push_str("switch(");
            emit_variable(*value, output);
            output.push_str(".tag) {");
            // let variant_idx = if let IrType::Variants(v)
            //     = variable_types[value.index].direct(types) { v.0 }
            //     else { panic!("should be a variant"); };
            for (branch_variant, branch_variable, branch_body) in branches {
                output.push_str("\n    case ");
                output.push_str(&branch_variant.0.to_string());
                output.push_str(":\n");
                if let IrType::Unit = variable_types[branch_variable.index].direct(types) {} else {
                    output.push_str("        ");
                    let mut branch_variable_str = String::new();
                    emit_variable(*branch_variable, &mut branch_variable_str);
                    output.push_str(&branch_variable_str);
                    output.push_str(" = ");
                    emit_variable(*value, output);
                    output.push_str(".value.");
                    output.push_str(strings.get(*branch_variant));
                    output.push_str(";\n");
                    let mut branch_variable_incr = String::new();
                    emit_rc_incr(
                        &branch_variable_str, variable_types[branch_variable.index], types, strings,
                        &mut branch_variable_incr
                    );
                    let mut branch_variable_incr_indented = String::new();
                    indent(&branch_variable_incr, &mut branch_variable_incr_indented);
                    indent(&branch_variable_incr_indented, output);
                    free.insert(branch_variable.index);
                }
                let mut branch = String::new();
                for instruction in branch_body {
                    emit_instruction(
                        instruction, variable_types, free, types, external, symbols, strings,
                        &mut branch
                    );
                }
                let mut branch_indented = String::new();
                indent(&branch, &mut branch_indented);
                indent(&branch_indented, output);
                output.push_str("\n        break;");
            }
            if else_branch.len() > 0 {
                output.push_str("\n    default:\n");
                let mut branch = String::new();
                for instruction in else_branch {
                    emit_instruction(
                        instruction, variable_types, free, types, external, symbols, strings,
                        &mut branch
                    );
                }
                let mut branch_indented = String::new();
                indent(&branch, &mut branch_indented);
                indent(&branch_indented, output);
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
            free.insert(into.index);
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
            free.insert(into.index);
        }
        IrInstruction::Return { value } => {
            let mut returned = String::new();
            emit_variable(*value, &mut returned);
            emit_rc_incr(&returned, variable_types[value.index], types, strings, output);
            emit_scope_decrements(free, variable_types, types, strings, output);
            if let IrType::Unit = variable_types[value.index].direct(types) {
                output.push_str("return;\n");
            } else {
                output.push_str("return ");
                output.push_str(&returned);
                output.push_str(";\n");
            }
        }
        IrInstruction::Phi { .. } => {}
    }
}