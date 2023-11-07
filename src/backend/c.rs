
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
    emit_type_members(&types, strings, &mut output);
    output.push_str("\n");
    emit_free_handler_functions(&types, strings, &mut output);
    output.push_str("\n");
    emit_conversion_functions(&types, strings, &mut output);
    output.push_str("\n");
    emit_comparison_functions(&types, strings, &mut output);
    output.push_str("\n");
    emit_symbol_declarations(
        &symbols, &types, strings, &mut external, &mut output
    );
    output.push('\n');
    let mut closure_bodies = Vec::new();
    let mut procedures = String::new();
    emit_procedure_impls(
        &symbols, &types, strings, &mut closure_bodies, &mut external, &mut procedures
    );
    procedures.push('\n');
    for closure_body in &closure_bodies {
        output.push_str(&closure_body);
    }
    output.push_str("\n");
    output.push_str(&procedures);
    emit_main_function(&main_procedure_path, strings, &mut output);
    return output;
}

fn emit_core_library(output: &mut String) {
    output.push_str(include_str!("./gen_libs/gera.h"));
    output.push_str("\n");
    output.push_str(include_str!("./gen_libs/core.c"));
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

fn emit_closure_name(index: usize, output: &mut String) {
    output.push_str("GeraClosure");
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
        output.push_str("typedef union ");
        emit_variants_name(variants_idx, output);
        output.push_str("Value ");
        emit_variants_name(variants_idx, output);
        output.push_str("Value;\n");
    }
    for closure_idx in 0..types.get_all_closures().len() {
        output.push_str("typedef struct ");
        emit_closure_name(closure_idx, output);
        output.push_str(" ");
        emit_closure_name(closure_idx, output);
        output.push_str(";\n");
    }
}

fn emit_type_members(types: &IrTypeBank, strings: &StringMap, output: &mut String) {
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
    for closure_idx in 0..types.get_all_closures().len() {
        output.push_str("typedef struct ");
        emit_closure_name(closure_idx, output);
        output.push_str(" {\n");
        output.push_str("    GeraAllocation* allocation;\n");
        let (parameter_types, return_type) = &types.get_all_closures()[closure_idx];
        output.push_str("    ");
        emit_type(*return_type, types, output);
        output.push_str(" (*procedure)(GeraAllocation*");
        for parameter_type in parameter_types {
            output.push_str(", ");
            emit_type(*parameter_type, types, output);
        }
        output.push_str(");\n");
        output.push_str("} ");
        emit_closure_name(closure_idx, output);
        output.push_str(";\n");
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
        IrType::Closure(_) => {
            output.push_str("gera___rc_incr(");
            output.push_str(variable);
            output.push_str(".allocation);\n");
        }
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
        IrType::Closure(_) => {
            output.push_str("gera___rc_decr(");
            output.push_str(variable);
            output.push_str(".allocation);\n");
        }
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
            output.push_str("\n    for(size_t i = 0; i < size / sizeof(");
            emit_type(element_type, types, output);
            output.push_str("); i += 1) {\n");
            let mut element_rc_decr = String::new();
            emit_rc_decr("(elements[i])", element_type, types, strings, &mut element_rc_decr);
            let mut element_rc_decr_indented = String::new();
            indent(&element_rc_decr, &mut element_rc_decr_indented);
            indent(&element_rc_decr_indented, output);
            output.push_str("    }\n}\n");
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

fn emit_variant_conversion_name(from: usize, to: usize, output: &mut String) {
    output.push_str("geravariant");
    output.push_str(&from.to_string());
    output.push_str("into");
    output.push_str(&to.to_string());
}

fn emit_object_conversion_name(from: usize, to: usize, output: &mut String) {
    output.push_str("geraobject");
    output.push_str(&from.to_string());
    output.push_str("intoobject");
    output.push_str(&to.to_string());
}

fn emit_object_concrete_object_conversion_name(from: usize, to: usize, output: &mut String) {
    output.push_str("geraobject");
    output.push_str(&from.to_string());
    output.push_str("intoconcrete");
    output.push_str(&to.to_string());
}

fn emit_concrete_object_object_conversion_name(from: usize, to: usize, output: &mut String) {
    output.push_str("geraconcrete");
    output.push_str(&from.to_string());
    output.push_str("intoobject");
    output.push_str(&to.to_string());
}

fn emit_conversion_functions(types: &IrTypeBank, strings: &StringMap, output: &mut String) {
    fn variant_is_subset(major: usize, minor: usize, types: &IrTypeBank) -> bool {
        if major == minor { return false; }
        for (variant_name, variant_type) in &types.get_all_variants()[minor] {
            if let Some(major_variant_type) = types.get_all_variants()[major].get(variant_name) {
                if !variant_type.eq(major_variant_type, types, &mut Vec::new()) { return false; }
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
                if !member_type.eq(major_member_type, types, &mut Vec::new()) { return false; }
            } else { return false; }
        }
        return true;
    }
    fn object_is_concrete_object_subset(major: usize, minor: usize, types: &IrTypeBank) -> bool {
        for (member_name, member_type) in &types.get_all_objects()[minor] {
            if let Some((_, major_member_type)) = types.get_all_concrete_objects()[major].iter()
                .filter(|(mn, _)| *mn == *member_name).next() {
                if !member_type.eq(major_member_type, types, &mut Vec::new()) { return false; }
            } else { return false; }
        }
        return true;
    }
    for to_object_idx in 0..types.get_all_objects().len() {
        let to_has_values = types.get_all_objects()[to_object_idx].iter().filter(|(_, mt)|
                if let IrType::Unit = mt.direct(types) { false } else { true }
            ).next().is_some();
        if !to_has_values { continue; }
        for from_object_idx in 0..types.get_all_objects().len() {
            let from_has_values = types.get_all_objects()[from_object_idx].iter().filter(|(_, mt)|
                if let IrType::Unit = mt.direct(types) { false } else { true }
            ).next().is_some();
            if !object_is_subset(from_object_idx, to_object_idx, types) { continue; }
            if !from_has_values { continue; }
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
        for from_concrete_object_idx in 0..types.get_all_concrete_objects().len() {
            // we can safely assume that each concrete object has at least one member
            if !object_is_concrete_object_subset(from_concrete_object_idx, to_object_idx, types) {
                continue;
            }
            emit_object_name(to_object_idx, output);
            output.push_str(" ");
            emit_concrete_object_object_conversion_name(from_concrete_object_idx, to_object_idx, output);
            output.push_str("(");
            emit_concrete_object_name(from_concrete_object_idx, output);
            output.push_str(" from) {\n");

            output.push_str("    ");
            emit_object_name(to_object_idx, output);
            output.push_str(" result;\n");
            output.push_str("    GeraAllocation* allocation = gera___rc_alloc(sizeof(");
            emit_object_alloc_name(to_object_idx, output);
            output.push_str("), &");
            emit_object_free_handler_name(to_object_idx, output);
            output.push_str(");\n");
            output.push_str("    result.allocation = allocation;\n");
            output.push_str("    ");
            emit_object_alloc_name(to_object_idx, output);
            output.push_str("* object = (");
            emit_object_alloc_name(to_object_idx, output);
            output.push_str("*) allocation->data;\n");
            for (member_name, member_type) in &types.get_all_objects()[to_object_idx] {
                output.push_str("    object->member");
                output.push_str(&member_name.0.to_string());
                output.push_str(" = from.");
                output.push_str(strings.get(*member_name));
                output.push_str(";\n");
                let mut member_value_incr_str = String::new();
                emit_rc_incr(
                    &format!("from.{}", strings.get(*member_name)),
                    *member_type, types, strings, &mut member_value_incr_str
                );
                indent(&member_value_incr_str, output);
                output.push_str("    ");
                output.push_str("result.member");
                output.push_str(&member_name.0.to_string());
                output.push_str(" = &object->member");
                output.push_str(&member_name.0.to_string());
                output.push_str(";\n");
            }
            output.push_str("    return result;\n");
            output.push_str("}\n");
        }
    }
    fn concrete_object_is_object_subset(major: usize, minor: usize, types: &IrTypeBank) -> bool {
        for (member_name, member_type) in &types.get_all_concrete_objects()[minor] {
            if let Some(major_member_type) = types.get_all_objects()[major].get(member_name) {
                if !member_type.eq(major_member_type, types, &mut Vec::new()) { return false; }
            } else { return false; }
        }
        return true;
    }
    for to_concrete_object_idx in 0..types.get_all_concrete_objects().len() {
        // we can safely assume that each concrete object has at least one member
        for from_object_idx in 0..types.get_all_objects().len() {
            let from_has_values = types.get_all_objects()[from_object_idx].iter().filter(|(_, mt)|
                if let IrType::Unit = mt.direct(types) { false } else { true }
            ).next().is_some();
            if !from_has_values { continue; }
            if !concrete_object_is_object_subset(from_object_idx, to_concrete_object_idx, types) {
                continue;
            }
            emit_concrete_object_name(to_concrete_object_idx, output);
            output.push_str(" ");
            emit_object_concrete_object_conversion_name(from_object_idx, to_concrete_object_idx, output);
            output.push_str("(");
            emit_object_name(from_object_idx, output);
            output.push_str(" from) {\n    return (");
            emit_concrete_object_name(to_concrete_object_idx, output);
            output.push_str(") {");
            for (member_name, _) in &types.get_all_concrete_objects()[to_concrete_object_idx] {
                output.push_str("\n        .");
                output.push_str(strings.get(*member_name));
                output.push_str(" = (*from.member");
                output.push_str(&member_name.0.to_string());
                output.push_str("),");
            }
            output.push_str("\n    };\n}\n");
        }
    }
}

fn emit_array_comparison_name(array_idx: usize, output: &mut String) {
    output.push_str("geraarray");
    output.push_str(&array_idx.to_string());
    output.push_str("eq");
}

fn emit_object_comparison_name(object_idx: usize, output: &mut String) {
    output.push_str("geraobject");
    output.push_str(&object_idx.to_string());
    output.push_str("eq");
}

fn emit_variant_comparison_name(variant_idx: usize, output: &mut String) {
    output.push_str("geravariant");
    output.push_str(&variant_idx.to_string());
    output.push_str("eq");
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
        IrType::Array(array_idx) => {
            emit_array_comparison_name(array_idx.0, output);
            output.push_str("(");
            output.push_str(a);
            output.push_str(", ");
            output.push_str(b);
            output.push_str(")");
        }
        IrType::Object(object_idx) => {
            emit_object_comparison_name(object_idx.0, output);
            output.push_str("(");
            output.push_str(a);
            output.push_str(", ");
            output.push_str(b);
            output.push_str(")");
        }
        IrType::ConcreteObject(_) => panic!("We should never have to compare concrete objects!"),
        IrType::Variants(variant_idx) => {
            emit_variant_comparison_name(variant_idx.0, output);
            output.push_str("(");
            output.push_str(a);
            output.push_str(", ");
            output.push_str(b);
            output.push_str(")");
        }
        IrType::Indirect(_) => panic!("should be direct"),
        _ => {
            output.push_str(a);
            output.push_str(" == ");
            output.push_str(b);
        }
    }
}

fn emit_comparison_functions(
    types: &IrTypeBank,
    strings: &StringMap,
    output: &mut String
) {
    for array_idx in 0..types.get_all_arrays().len() {
        output.push_str("char ");
        emit_array_comparison_name(array_idx, output);
        output.push_str("(GeraArray a, GeraArray b);\n");
    }
    for object_idx in 0..types.get_all_objects().len() {
        output.push_str("char ");
        emit_object_comparison_name(object_idx, output);
        output.push_str("(");
        emit_object_name(object_idx, output);
        output.push_str(" a, ");
        emit_object_name(object_idx, output);
        output.push_str(" b);\n");
    }
    for variant_idx in 0..types.get_all_variants().len() {
        output.push_str("char ");
        emit_variant_comparison_name(variant_idx, output);
        output.push_str("(");
        emit_variants_name(variant_idx, output);
        output.push_str(" a, ");
        emit_variants_name(variant_idx, output);
        output.push_str(" b);\n");
    }
    for array_idx in 0..types.get_all_arrays().len() {
        let element_type = types.get_all_arrays()[array_idx];
        output.push_str("char ");
        emit_array_comparison_name(array_idx, output);
        output.push_str("(GeraArray a, GeraArray b) {\n");
        output.push_str("    if(a.length != b.length) { return 0; }\n");
        output.push_str("    ");
        emit_type(element_type, types, output);
        output.push_str("* a_elements = (");
        emit_type(element_type, types, output);
        output.push_str("*) a.data;\n");
        output.push_str("    ");
        emit_type(element_type, types, output);
        output.push_str("* b_elements = (");
        emit_type(element_type, types, output);
        output.push_str("*) b.data;\n");
        output.push_str("    for(size_t i = 0; i < a.length; i += 1) {\n");
        output.push_str("        if(!(");
        emit_equality("a_elements[i]", "b_elements[i]", element_type, types, output);
        output.push_str(")) { return 0; }\n");
        output.push_str("    }\n");
        output.push_str("    return 1;\n");
        output.push_str("}\n");
    }
    for object_idx in 0..types.get_all_objects().len() {
        output.push_str("char ");
        emit_object_comparison_name(object_idx, output);
        output.push_str("(");
        emit_object_name(object_idx, output);
        output.push_str(" a, ");
        emit_object_name(object_idx, output);
        output.push_str(" b) {");
        for (member_name, member_type) in &types.get_all_objects()[object_idx] {
            output.push_str("\n    if(!(");
            emit_equality(
                &format!("(*a.member{})", member_name.0),
                &format!("(*b.member{})", member_name.0),
                *member_type, types, output
            );
            output.push_str(")) { return 0; }");
        }
        output.push_str("\n    return 1;\n}\n");
    }
    for variant_idx in 0..types.get_all_variants().len() {
        output.push_str("char ");
        emit_variant_comparison_name(variant_idx, output);
        output.push_str("(");
        emit_variants_name(variant_idx, output);
        output.push_str(" a, ");
        emit_variants_name(variant_idx, output);
        output.push_str(" b) {\n");
        output.push_str("    if(a.tag != b.tag) { return 0; }\n");
        output.push_str("    switch(a.tag) {\n");
        for(variant_name, variant_type) in &types.get_all_variants()[variant_idx] {
            if let IrType::Unit = variant_type.direct(types) { continue; }
            output.push_str("        case ");
            output.push_str(&variant_name.0.to_string());
            output.push_str(":\n");
            output.push_str("            if(!(");
            emit_equality(
                &format!("(a.value.{})", strings.get(*variant_name)),
                &format!("(b.value.{})", strings.get(*variant_name)),
                *variant_type, types, output
            );
            output.push_str(")) { return 0; }\n");
            output.push_str("            break;\n");
        }
        output.push_str("    }\n");
        output.push_str("    return 1;\n");
        output.push_str("}\n")
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
            IrSymbol::BuiltInProcedure { path, variant, parameter_types, return_type } => {
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
            IrSymbol::Variable { path, value_type, value } => {
                if let IrType::Unit = value_type.direct(types) { continue; }
                output.push_str("const ");
                emit_type(*value_type, types, output);
                output.push_str(" ");
                emit_path(path, strings, output);
                output.push_str(" = ");
                emit_value(value, *value_type, types, strings, output);
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
    if from_type.eq(&to_type, types, &mut Vec::new()) {
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
    if let IrType::Object(from_object_idx) = from_type.direct(types) {
        if let IrType::Object(to_object_idx) = to_type.direct(types) {
            emit_object_conversion_name(from_object_idx.0, to_object_idx.0, output);
            output.push_str("(");
            output.push_str(variable);
            output.push_str(")");
            return;
        }
        if let IrType::ConcreteObject(to_concrete_object_idx) = to_type.direct(types) {
            emit_object_concrete_object_conversion_name(
                from_object_idx.0, to_concrete_object_idx.0, output
            );
            output.push_str("(");
            output.push_str(variable);
            output.push_str(")");
            return;
        }
    }
    if let IrType::ConcreteObject(from_concrete_object_idx) = from_type.direct(types) {
        if let IrType::Object(to_object_idx) = to_type.direct(types) {
            emit_concrete_object_object_conversion_name(
                from_concrete_object_idx.0, to_object_idx.0, output
            );
            output.push_str("(");
            output.push_str(variable);
            output.push_str(")");
            return;
        }
    }
    println!("types = {:?}", types);
    println!("from = {:?}", from_type);
    println!("to = {:?}", to_type);
    panic!("unexpected implicit conversion!");
}

fn emit_conditional_decr(
    variable: IrVariable,
    variable_type: IrType,
    types: &IrTypeBank,
    strings: &StringMap,
    output: &mut String
) {
    let mut var_str = String::new();
    emit_variable(variable, &mut var_str);
    let mut decr_str = String::new();
    emit_rc_decr(&var_str, variable_type, types, strings, &mut decr_str);
    if decr_str.len() == 0 { return; }
    output.push_str("if(");
    output.push_str(&var_str);
    output.push_str("assigned) {\n");
    indent(&decr_str, output);
    output.push_str("}\n");
}

fn emit_scope_decrements(
    free: &HashSet<usize>,
    variable_types: &Vec<IrType>,
    types: &IrTypeBank,
    strings: &StringMap,
    output: &mut String
) {
    for variable_idx in free {
        emit_conditional_decr(
            IrVariable { index: *variable_idx, version: 0 },
            variable_types[*variable_idx],
            types, strings, output
        );
    }
}

fn get_builtin_bodies(strings: &mut StringMap) -> HashMap<NamespacePath, fn(&Vec<IrType>, IrType, &IrTypeBank, &mut StringMap) -> String> {
    fn path_from(segments: &[&'static str], strings: &mut StringMap) -> NamespacePath {
        NamespacePath::new(segments.iter().map(|s| strings.insert(s)).collect())
    }
    let mut builtins: HashMap<NamespacePath, fn(&Vec<IrType>, IrType, &IrTypeBank, &mut StringMap) -> String> = HashMap::new();
    builtins.insert(path_from(&["core", "addr_eq"], strings), |_, _, _, _| {
        String::from("
return param0.allocation == param1.allocation;
")
    });
    builtins.insert(path_from(&["core", "tag_eq"], strings), |_, _, _, _| {
        String::from("
return param0.tag == param1.tag;
")
    });
    builtins.insert(path_from(&["core", "length"], strings), |param_types, _, types, _| {
        if let IrType::String = param_types[0].direct(types) {
            String::from("
for(size_t c = 0;; c += 1) {
    if(param0[c] == '\\0') { return c; }
}
")
        } else {
            String::from("
return param0.length;
")
        }
    });
    builtins.insert(path_from(&["core", "array"], strings), |param_types, return_type, types, strings| {
        let array_idx = if let IrType::Array(array_idx) = return_type.direct(types) {
            array_idx.0
        } else { panic!("should be an array!"); };
        let mut result = String::new();
        result.push_str("GeraArray result;\n");
        result.push_str("GeraAllocation* allocation = gera___rc_alloc(sizeof(");
        emit_type(param_types[0], types, &mut result);
        result.push_str(") * param1, &");
        emit_array_free_handler_name(array_idx, &mut result);
        result.push_str(");\n");
        result.push_str("result.allocation = allocation;\n");
        result.push_str("result.data = allocation->data;\n");
        result.push_str("result.length = param1;\n");
        emit_type(param_types[0], types, &mut result);
        result.push_str("* elements = (");
        emit_type(param_types[0], types, &mut result);
        result.push_str("*) allocation->data;\n");
        result.push_str("for(size_t i = 0; i < param1; i += 1) {\n");
        result.push_str("    elements[i] = param0;\n");
        let mut value_rc_incr = String::new();
        emit_rc_incr("param0", param_types[0], types, strings, &mut value_rc_incr);
        indent(&value_rc_incr, &mut result);
        result.push_str("}\n");
        result.push_str("return result;\n");
        result
    });
    builtins.insert(path_from(&["core", "exhaust"], strings), |_, _, _, strings| {
        format!("
while(((param0.procedure)(param0.allocation)).tag == {}) {{}}
", strings.insert("next").0)
    });
    return builtins;
}

fn emit_procedure_impls(
    symbols: &Vec<IrSymbol>,
    types: &IrTypeBank,
    strings: &mut StringMap,
    closure_bodies: &mut Vec<String>,
    external: &mut HashMap<NamespacePath, StringIdx>,
    output: &mut String
) {
    let builtin_bodies = get_builtin_bodies(strings);
    for symbol in symbols {
        match symbol {
            IrSymbol::Procedure { path, variant, parameter_types, return_type, variables, body } => {
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
                    output.push_str("    char ");
                    emit_variable(IrVariable { index: variable_idx, version: 0 }, output);
                    output.push_str("assigned = 0;\n");
                }
                if let IrType::Unit = return_type.direct(types) {} else {
                    output.push_str("    ");
                    emit_type(*return_type, types, output);
                    output.push_str(" returned;\n");
                }
                let mut body_str = String::new();
                let mut body_free = HashSet::new();
                emit_block(
                    body, variables, &mut body_free, closure_bodies, types, external, symbols, strings,
                    &mut body_str
                );
                body_str.push_str("\nret:\n");
                emit_scope_decrements(&body_free, variables, types, strings, &mut body_str);
                indent(&body_str, output);
                if let IrType::Unit = return_type.direct(types) {} else {
                    output.push_str("    return returned;\n");
                }
                output.push_str("}\n");
            }
            IrSymbol::BuiltInProcedure { path, variant, parameter_types, return_type } => {
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
        IrType::Closure(p) => emit_closure_name(p.0, output),
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

fn emit_value(
    value: &Value, value_type: IrType, types: &IrTypeBank, strings: &StringMap, output: &mut String
) {
    match value {
        Value::Unit => panic!("should not have to emit unit value!"),
        Value::Boolean(b) => output.push_str(if *b { "1" } else { "0" }),
        Value::Integer(i) => output.push_str(&i.to_string()),
        Value::Float(f) => output.push_str(&format!("{:.}", *f)),
        Value::String(s) => emit_string_literal(std::rc::Rc::as_ref(s), output),
        Value::Array(_) => panic!("constant arrays are forbidden!"),
        Value::Object(_) => panic!("constant objects are forbidden!"),
        Value::Closure(_, _, _, _, _) => panic!("constant closures are forbidden!"),
        Value::Variant(variant_name, variant_value) => {
            let variant_idx = if let IrType::Variants(variant_idx) = value_type.direct(types) {
                variant_idx.0
            } else { panic!("value type should be variants"); };
            output.push_str("(");
            emit_variants_name(variant_idx, output);
            output.push_str(") { .tag = ");
            output.push_str(&variant_name.0.to_string());
            let variant_type = types.get_all_variants()[variant_idx].get(variant_name)
                .expect("variant should exist").direct(types);
            if let IrType::Unit = variant_type {} else {
                output.push_str(", .value = { .");
                output.push_str(strings.get(*variant_name));
                output.push_str(" = ");
                emit_value(variant_value, variant_type, types, strings, output);
                output.push_str(" }");
            }
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
    variable_types: &Vec<IrType>,
    free: &mut HashSet<usize>,
    closure_bodies: &mut Vec<String>,
    types: &IrTypeBank,
    external: &HashMap<NamespacePath, StringIdx>,
    symbols: &Vec<IrSymbol>,
    strings: &StringMap,
    output: &mut String
) {
    output.push_str("{\n");
    for instruction in instructions {
        let mut o = String::new();
        emit_instruction(
            instruction, variable_types, free, closure_bodies, types, external, symbols, strings,
            &mut o
        );
        indent(&o, output);
    }
    output.push_str("}");
}

fn mark_as_assigned(
    variable: IrVariable,
    variable_type: IrType,
    types: &IrTypeBank,
    output: &mut String
) {
    if let IrType::Unit = variable_type.direct(types) {} else {
        emit_variable(variable, output);
        output.push_str("assigned = 1;\n");
    }
}

fn emit_closure_body_name(closure_idx: usize, variant: usize, output: &mut String) {
    output.push_str("geraclosure");
    output.push_str(&closure_idx.to_string());
    output.push_str("body");
    output.push_str(&variant.to_string())
}

fn emit_instruction(
    instruction: &IrInstruction,
    variable_types: &Vec<IrType>,
    free: &mut HashSet<usize>,
    closure_bodies: &mut Vec<String>,
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
            emit_conditional_decr(*into, variable_types[into.index], types, strings, output);
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
            mark_as_assigned(*into, variable_types[into.index], types, output);
            free.insert(into.index);
        }
        IrInstruction::LoadArray { element_values, into } => {
            let array_idx = if let IrType::Array(array_idx) = variable_types[into.index]
                .direct(types) {
                array_idx.0
            } else { panic!("should be an array"); };
            emit_conditional_decr(*into, variable_types[into.index], types, strings, output);
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
            mark_as_assigned(*into, variable_types[into.index], types, output);
            free.insert(into.index);
        }
        IrInstruction::LoadVariant { name, v, into } => {
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_conditional_decr(*into, variable_types[into.index], types, strings, output);
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
            mark_as_assigned(*into, variable_types[into.index], types, output);
            free.insert(into.index);
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
                    let (closure_idx, (expected_parameter_types, expected_return_type)) =
                        if let IrType::Closure(closure_idx) = variable_types[into.index].direct(types) {
                            (closure_idx.0, types.get_closure(closure_idx))
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
                                        &mut Vec::new()
                                    ) {
                                        params_match = false;
                                        break;
                                    }
                                }
                                if !params_match { continue; }
                                if !return_type.eq(expected_return_type, types, &mut Vec::new()) {
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
                    let mut closure_body = String::new();
                    let closure_variant = closure_bodies.len();
                    emit_type(*expected_return_type, types, &mut closure_body);
                    closure_body.push_str(" ");
                    emit_closure_body_name(closure_idx, closure_variant, &mut closure_body);
                    closure_body.push_str("(GeraAllocation* allocation");
                    for param_idx in 0..expected_parameter_types.len() {
                        closure_body.push_str(", ");
                        emit_type(expected_parameter_types[param_idx], types, &mut closure_body);
                        closure_body.push_str(" param");
                        closure_body.push_str(&param_idx.to_string());
                    }
                    closure_body.push_str(") {\n");
                    closure_body.push_str("    (void)(allocation);\n");
                    closure_body.push_str("    return ");
                    if let Some(backing) = external.get(path) {
                        closure_body.push_str(strings.get(*backing));
                    } else {
                        emit_procedure_name(path, found_variant, strings, &mut closure_body);
                    }
                    closure_body.push_str("(");
                    for param_idx in 0..expected_parameter_types.len() {
                        if param_idx > 0 { closure_body.push_str(", "); }
                        closure_body.push_str("param");
                        closure_body.push_str(&param_idx.to_string());
                    }
                    closure_body.push_str(");\n");
                    closure_body.push_str("}\n");
                    closure_bodies.push(closure_body);
                    emit_conditional_decr(*into, variable_types[into.index], types, strings, output);
                    output.push_str("{\n");
                    output.push_str("    GeraAllocation* allocation = gera___rc_alloc(1, &gera___free_nothing);\n");
                    let mut into_str = String::new();
                    emit_variable(*into, &mut into_str);
                    output.push_str("    ");
                    output.push_str(&into_str);
                    output.push_str(".allocation = allocation;\n");
                    output.push_str("    ");
                    output.push_str(&into_str);
                    output.push_str(".procedure = &");
                    emit_closure_body_name(closure_idx, closure_variant, output);
                    output.push_str(";\n");
                    output.push_str("}\n");
                    mark_as_assigned(*into, variable_types[into.index], types, output);
                    free.insert(into.index);
                }
                IrSymbol::Variable { .. } |
                IrSymbol::ExternalVariable { .. } => {
                    let mut into_str = String::new();
                    emit_variable(*into, &mut into_str);
                    emit_conditional_decr(*into, variable_types[into.index], types, strings, output);
                    output.push_str(&into_str);
                    output.push_str(" = ");
                    if let Some(backing) = external.get(path) {
                        output.push_str(strings.get(*backing));
                    } else {
                        emit_path(path, strings, output);
                    }
                    output.push_str(";\n");
                    emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
                    mark_as_assigned(*into, variable_types[into.index], types, output);
                    free.insert(into.index);
                }
            }
        }
        IrInstruction::LoadParameter { index, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_conditional_decr(*into, variable_types[into.index], types, strings, output);
            output.push_str(&into_str);
            output.push_str(" = param");
            output.push_str(&index.to_string());
            output.push_str(";\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
            mark_as_assigned(*into, variable_types[into.index], types, output);
            free.insert(into.index);
        }
        IrInstruction::LoadClosure {
            parameter_types, return_type, captured, variables, body, into
        } => {
            fn emit_closure_captures_name(closure_idx: usize, variant: usize, output: &mut String) {
                emit_closure_name(closure_idx, output);
                output.push_str("Captures");
                output.push_str(&variant.to_string())
            }
            fn emit_closure_free_name(closure_idx: usize, variant: usize, output: &mut String) {
                output.push_str("geraclosure");
                output.push_str(&closure_idx.to_string());
                output.push_str("free");
                output.push_str(&variant.to_string())
            }
            let closure_idx = if let IrType::Closure(closure_idx) = variable_types[into.index]
                .direct(types) {
                closure_idx.0
            } else { panic!("should be closure type"); };
            let mut closure_body = String::new();
            let variant = closure_bodies.len();
            // emit closure captures struct
            closure_body.push_str("typedef struct ");
            emit_closure_captures_name(closure_idx, variant, &mut closure_body);
            closure_body.push_str(" {\n");
            for (capture_name, capture_variable) in captured {
                let capture_type = variable_types[capture_variable.index];
                closure_body.push_str("    ");
                emit_type(capture_type, types, &mut closure_body);
                closure_body.push_str(" ");
                closure_body.push_str(strings.get(*capture_name));
                closure_body.push_str(";\n");
            }
            closure_body.push_str("} ");
            emit_closure_captures_name(closure_idx, variant, &mut closure_body);
            closure_body.push_str(";\n");
            // emit closure captures free
            closure_body.push_str("void ");
            emit_closure_free_name(closure_idx, variant, &mut closure_body);
            closure_body.push_str("(char* data, size_t size) {\n");
            closure_body.push_str("    ");
            emit_closure_captures_name(closure_idx, variant, &mut closure_body);
            closure_body.push_str("* captures = (");
            emit_closure_captures_name(closure_idx, variant, &mut closure_body);
            closure_body.push_str("*) data;\n");
            for (capture_name, capture_variable) in captured {
                let capture_type = variable_types[capture_variable.index];
                let mut capture_rc_decr = String::new();
                emit_rc_decr(
                    &format!("captures->{}", strings.get(*capture_name)),
                    capture_type, types, strings, &mut capture_rc_decr
                );
                indent(&capture_rc_decr, &mut closure_body);
            }
            closure_body.push_str("}\n");
            // emit closure body procedure
            emit_type(*return_type, types, &mut closure_body);
            closure_body.push_str(" ");
            emit_closure_body_name(closure_idx, variant, &mut closure_body);
            closure_body.push_str("(GeraAllocation* allocation");
            for param_idx in 0..parameter_types.len() {
                closure_body.push_str(", ");
                emit_type(parameter_types[param_idx], types, &mut closure_body);
                closure_body.push_str(" param");
                closure_body.push_str(&param_idx.to_string());
            }
            closure_body.push_str(") {\n");
            closure_body.push_str("    ");
            emit_closure_captures_name(closure_idx, variant, &mut closure_body);
            closure_body.push_str("* captures = (");
            emit_closure_captures_name(closure_idx, variant, &mut closure_body);
            closure_body.push_str("*) allocation->data;\n");
            for variable_idx in 0..variables.len() {
                if let IrType::Unit = variables[variable_idx].direct(types) { continue; }
                closure_body.push_str("    ");
                emit_type(variables[variable_idx], types, &mut closure_body);
                closure_body.push_str(" ");
                emit_variable(IrVariable { index: variable_idx, version: 0 }, &mut closure_body);
                closure_body.push_str(";\n");
                closure_body.push_str("    char ");
                emit_variable(IrVariable { index: variable_idx, version: 0 }, &mut closure_body);
                closure_body.push_str("assigned = 0;\n");
            }
            if let IrType::Unit = return_type.direct(types) {} else {
                closure_body.push_str("    ");
                emit_type(*return_type, types, &mut closure_body);
                closure_body.push_str(" returned;\n");
            }
            let mut body_str = String::new();
            let mut body_free = HashSet::new();
            emit_block(
                body, variables, &mut body_free, closure_bodies, types, external, symbols, strings,
                &mut body_str
            );
            body_str.push_str("\nret:\n");
            emit_scope_decrements(&body_free, variables, types, strings, &mut body_str);
            indent(&body_str, &mut closure_body);
            if let IrType::Unit = return_type.direct(types) {} else {
                closure_body.push_str("    return returned;\n");
            }
            closure_body.push_str("}\n");
            closure_bodies.push(closure_body);
            // emit closure literal
            emit_conditional_decr(*into, variable_types[into.index], types, strings, output);
            output.push_str("{\n");
            output.push_str("    GeraAllocation* allocation = gera___rc_alloc(sizeof(");
            emit_closure_captures_name(closure_idx, variant, output);
            output.push_str("), &");
            emit_closure_free_name(closure_idx, variant, output);
            output.push_str(");\n");
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            output.push_str("    ");
            output.push_str(&into_str);
            output.push_str(".allocation = allocation;\n");
            output.push_str("    ");
            output.push_str(&into_str);
            output.push_str(".procedure = &");
            emit_closure_body_name(closure_idx, variant, output);
            output.push_str(";\n");
            output.push_str("    ");
            emit_closure_captures_name(closure_idx, variant, output);
            output.push_str("* captures = (");
            emit_closure_captures_name(closure_idx, variant, output);
            output.push_str("*) allocation->data;\n");
            for (capture_name, capture_value) in captured {
                output.push_str("    captures->");
                output.push_str(strings.get(*capture_name));
                output.push_str(" = ");
                let mut capture_value_str = String::new();
                emit_variable(*capture_value, &mut capture_value_str);
                output.push_str(&capture_value_str);
                output.push_str(";\n");
                let mut member_value_incr_str = String::new();
                emit_rc_incr(
                    &capture_value_str, variable_types[capture_value.index], types, strings,
                    &mut member_value_incr_str
                );
                indent(&member_value_incr_str, output);
            }
            output.push_str("}\n");
            mark_as_assigned(*into, variable_types[into.index], types, output);
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
            emit_conditional_decr(*into, variable_types[into.index], types, strings, output);
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
            mark_as_assigned(*into, variable_types[into.index], types, output);
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
            emit_conditional_decr(*into, variable_types[into.index], types, strings, output);
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
            mark_as_assigned(*into, variable_types[into.index], types, output);
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
        IrInstruction::GetClosureCapture { name, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_conditional_decr(*into, variable_types[into.index], types, strings, output);
            output.push_str(&into_str);
            output.push_str(" = captures->");
            output.push_str(strings.get(*name));
            output.push_str("");
            output.push_str(";\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
            mark_as_assigned(*into, variable_types[into.index], types, output);
            free.insert(into.index);
        }
        IrInstruction::SetClosureCapture { value, name } => {
            if let IrType::Unit = variable_types[value.index].direct(types) { return; }
            let mut capture_str = String::new();
            capture_str.push_str("captures->");
            capture_str.push_str(strings.get(*name));
            emit_rc_decr(&capture_str, variable_types[value.index], types, strings, output);
            output.push_str(&capture_str);
            output.push_str(" = ");
            emit_variable(*value, output);
            output.push_str(";\n");
            emit_rc_incr(&capture_str, variable_types[value.index], types, strings, output);
        }
        IrInstruction::Move { from, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            if *from == *into { return; }
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_conditional_decr(*into, variable_types[into.index], types, strings, output);
            output.push_str(&into_str);
            output.push_str(" = ");
            emit_variable(*from, output);
            output.push_str(";\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
            mark_as_assigned(*into, variable_types[into.index], types, output);
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
            let mut a_str = String::new();
            emit_variable(*a, &mut a_str);
            let mut b_str = String::new();
            emit_variable(*b, &mut b_str);
            emit_equality(&a_str, &b_str, variable_types[a.index], types, output);
            output.push_str(";\n");
        }
        IrInstruction::NotEquals { a, b, into } => {
            emit_variable(*into, output);
            output.push_str(" = !(");
            let mut a_str = String::new();
            emit_variable(*a, &mut a_str);
            let mut b_str = String::new();
            emit_variable(*b, &mut b_str);
            emit_equality(&a_str, &b_str, variable_types[a.index], types, output);
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
                emit_value(
                    &branches[branch_idx].0, variable_types[value.index], types, strings, output
                );
                output.push_str(";");
            }
            output.push_str("\n");
            let mut v_str = String::new();
            emit_variable(*value, &mut v_str);
            let mut branches_str = String::new();
            for branch_idx in 0..branches.len() {
                branches_str.push_str("if(");
                let mut b_str = String::from("branchval");
                b_str.push_str(&branch_idx.to_string());
                emit_equality(&v_str, &b_str, variable_types[value.index], types, &mut branches_str);
                branches_str.push_str(") ");
                emit_block(
                    &branches[branch_idx].1, variable_types, free, closure_bodies, types, external,
                    symbols, strings, &mut branches_str
                );
                branches_str.push_str(" else ");
            }
            emit_block(
                else_branch, variable_types, free, closure_bodies, types, external, symbols,
                strings, &mut branches_str
            );
            indent(&branches_str, output);
            output.push_str("\n}\n");
        }
        IrInstruction::BranchOnVariant { value, branches, else_branch } => {
            output.push_str("switch(");
            emit_variable(*value, output);
            output.push_str(".tag) {");
            for (branch_variant, branch_variable, branch_body) in branches {
                output.push_str("\n    case ");
                output.push_str(&branch_variant.0.to_string());
                output.push_str(":\n");
                if let Some(branch_variable) = branch_variable {
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
                }
                let mut branch = String::new();
                for instruction in branch_body {
                    emit_instruction(
                        instruction, variable_types, free, closure_bodies, types, external, symbols,
                        strings, &mut branch
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
                        instruction, variable_types, free, closure_bodies, types, external, symbols,
                        strings, &mut branch
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
                IrSymbol::BuiltInProcedure { path: p, variant: v, .. } => *path == *p && *variant == *v,
                IrSymbol::ExternalProcedure { path: p, .. } => *path == *p,
                _ => false
            }).next().expect("should exist") {
                IrSymbol::Procedure { parameter_types, return_type, .. } |
                IrSymbol::BuiltInProcedure { parameter_types, return_type, .. } |
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
            mark_as_assigned(*into, variable_types[into.index], types, output);
            free.insert(into.index);
        }
        IrInstruction::CallClosure { called, arguments, into } => {
            let (parameter_types, return_type) = if let IrType::Closure(p)
                    = variable_types[called.index].direct(types) {
                types.get_closure(p)
            } else { panic!("should be a closure"); };
            let returns_value = if let IrType::Unit = return_type.direct(types) { false }
                else { true };
            let mut value = String::new();
            let mut called_str = String::new();
            emit_variable(*called, &mut called_str);
            value.push_str("(");
            value.push_str(&called_str);
            value.push_str(".procedure)(");
            value.push_str(&called_str);
            value.push_str(".allocation");
            for argument_idx in 0..arguments.len() {
                value.push_str(", ");
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
            mark_as_assigned(*into, variable_types[into.index], types, output);
            free.insert(into.index);
        }
        IrInstruction::Return { value } => {
            let mut returned = String::new();
            emit_variable(*value, &mut returned);
            if let IrType::Unit = variable_types[value.index].direct(types) {
                output.push_str("goto ret;\n");
            } else {
                output.push_str("returned = ");
                output.push_str(&returned);
                output.push_str(";\n");
                emit_rc_incr(&returned, variable_types[value.index], types, strings, output);
                output.push_str("goto ret;\n");
            }
        }
        IrInstruction::Phi { .. } => {}
    }
}