
use std::collections::{HashMap, HashSet};

use crate::backend::{
    ir::{IrSymbol, IrTypeBank, IrType, IrInstruction, IrVariable},
    constants::{ConstantPool, ConstantValue, ConstantPoolValue}
};
use crate::frontend::modules::NamespacePath;
use crate::util::strings::{StringMap, StringIdx};

struct ConversionFunctions {
    declarations: String,
    bodies: String,
    declared: HashSet<(IrType, IrType)>
}

pub fn generate_c(
    symbols: Vec<IrSymbol>,
    mut types: IrTypeBank,
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
    emit_comparison_functions(&types, strings, &mut output);
    let mut constants = ConstantPool::new();
    let mut static_var_vals = HashMap::new();
    let mut constant_dependants = String::new();
    constant_dependants.push_str("\n");
    emit_symbol_declarations(
        &symbols, &types, &mut constants, &mut static_var_vals, strings, &mut external,
        &mut constant_dependants
    );
    let mut conversions = ConversionFunctions {
        declarations: String::new(),
        bodies: String::new(),
        declared: HashSet::new()
    };
    let mut closure_bodies = Vec::new();
    let mut procedure_impls = String::new();
    emit_procedure_impls(
        &symbols, &types, &mut constants, strings, &mut closure_bodies, &mut conversions, &mut external,
        &mut procedure_impls
    );
    constant_dependants.push_str("\n");
    constant_dependants.push_str(&conversions.declarations);
    constant_dependants.push_str("\n");
    constant_dependants.push_str(&conversions.bodies);
    constant_dependants.push_str("\n");
    for closure_body in &closure_bodies {
        constant_dependants.push_str(&closure_body);
    }
    constant_dependants.push_str("\n");
    constant_dependants.push_str(&procedure_impls);
    output.push_str("\n");
    emit_constant_declarations(&constants, &mut types, &mut output);
    output.push_str(&constant_dependants);
    output.push_str("\n");
    emit_constant_initializers(&constants, &static_var_vals, &mut types, strings, &mut output);
    output.push_str("\n");
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
    for object_idx in 0..types.get_all_objects().len() {
        output.push_str("typedef struct ");
        emit_object_name(object_idx, output);
        output.push_str(" {\n    GeraAllocation* allocation;");
        for (member_name, member_type) in &types.get_all_objects()[object_idx] {
            if let IrType::Unit = member_type.direct(types) { continue; }
            output.push_str("\n    ");
            emit_type(*member_type, types, output);
            output.push_str("* member");
            output.push_str(&member_name.0.to_string());
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
            if let IrType::Unit = member_type.direct(types) { continue; }
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
            if let IrType::Unit = parameter_type.direct(types) { continue; }
            output.push_str(", ");
            emit_type(*parameter_type, types, output);
        }
        output.push_str(");\n");
        output.push_str("} ");
        emit_closure_name(closure_idx, output);
        output.push_str(";\n");
    }
    for object_idx in 0..types.get_all_objects().len() {
        output.push_str("typedef struct ");
        emit_object_alloc_name(object_idx, output);
        output.push_str(" {");
        let mut had_member = false;
        for (member_name, member_type) in &types.get_all_objects()[object_idx] {
            if let IrType::Unit = member_type.direct(types) { continue; }
            output.push_str("\n    ");
            emit_type(*member_type, types, output);
            output.push_str(" member");
            output.push_str(&member_name.0.to_string());
            output.push_str(";");
            had_member = true;
        }
        if !had_member {
            output.push_str("\n    char empty;");
        }
        output.push_str("\n} ");
        emit_object_alloc_name(object_idx, output);
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
        IrType::String |
        IrType::Array(_) |
        IrType::Object(_) => {
            output.push_str("gera___rc_incr(");
            output.push_str(variable);
            output.push_str(".allocation);\n");
        }
        IrType::ConcreteObject(_) => {}
        IrType::Variants(variant_idx) => {
            let mut switch = String::new();
            let mut had_effect = false;
            switch.push_str("switch(");
            switch.push_str(variable);
            switch.push_str(".tag) {");
            for (variant_name, variant_type) in types.get_variants(variant_idx) {
                if let IrType::Unit = variant_type.direct(types) { continue; }
                switch.push_str("\n    case ");
                switch.push_str(&variant_name.0.to_string());
                switch.push_str(":\n");
                let mut var_decr = String::new();
                emit_rc_incr(
                    &format!("{}.value.{}", variable, strings.get(*variant_name)),
                    *variant_type, types, strings, &mut var_decr
                );
                if var_decr.len() > 0 { had_effect = true; }
                let mut var_decr_indented = String::new();
                indent(&var_decr, &mut var_decr_indented);
                indent(&var_decr_indented, &mut switch);
                switch.push_str("        break;");
            }
            switch.push_str("\n}\n");
            if had_effect {
                output.push_str(&switch);
            }
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
        IrType::String |
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
        IrType::Closure(_) => {
            output.push_str(a);
            output.push_str(".allocation == ");
            output.push_str(b);
            output.push_str(".allocation");
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
    constants: &mut ConstantPool,
    static_var_vals: &mut HashMap<NamespacePath, (ConstantValue, IrType)>,
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
                let mut had_param = false;
                for p in 0..parameter_types.len() {
                    if let IrType::Unit = parameter_types[p].direct(types) { continue; }
                    if had_param { output.push_str(", "); }
                    had_param = true;
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
                let mut had_param = false;
                for p in 0..parameter_types.len() {
                    if let IrType::Unit = parameter_types[p].direct(types) { continue; }
                    if had_param { output.push_str(", "); }
                    had_param = true;
                    emit_type(parameter_types[p], types, output);
                    output.push_str(" param");
                    output.push_str(&p.to_string());
                }
                output.push_str(");\n");
            }
            IrSymbol::Variable { path, value_type, value } => {
                if let IrType::Unit = value_type.direct(types) { continue; }
                emit_type(*value_type, types, output);
                output.push_str(" ");
                emit_path(path, strings, output);
                output.push_str(";\n");
                let value = constants.insert(value, *value_type, types);
                static_var_vals.insert(path.clone(), (value, *value_type));
            }
            IrSymbol::ExternalVariable { path, backing, value_type } => {
                if let IrType::Unit = value_type.direct(types) { continue; }
                external.insert(path.clone(), *backing);
                output.push_str("extern ");
                emit_type(*value_type, types, output);  
                output.push_str(" ");
                output.push_str(strings.get(*backing));
                output.push_str(";\n");
            }
        }
    }
}

fn emit_conversion_function_name(from: IrType, to: IrType, output: &mut String) {
    fn emit_type(t: IrType, output: &mut String) {
        match t {
            IrType::Unit | IrType::Boolean | IrType::Integer | IrType::Float | IrType::String |
            IrType::Array(_) | IrType::Closure(_) | IrType::Indirect(_) => {
                panic!("The type {:?} may not be involved in type conversions!", t);
            }
            IrType::Object(object_idx) => {
                output.push_str("object");
                output.push_str(&object_idx.0.to_string());
            }
            IrType::ConcreteObject(cobject_idx) => {
                output.push_str("cobject");
                output.push_str(&cobject_idx.0.to_string());
            }
            IrType::Variants(variants_idx) => {
                output.push_str("variants");
                output.push_str(&variants_idx.0.to_string());
            }
        }
    }
    output.push_str("geraconvert");
    emit_type(from, output);
    output.push_str("to");
    emit_type(to, output);
}

fn emit_implicit_conversion(
    variable: &str,
    mut from_type: IrType,
    mut to_type: IrType,
    conversions: &mut ConversionFunctions,
    types: &IrTypeBank,
    strings: &StringMap,
    output: &mut String
) {
    from_type = from_type.direct(types);
    to_type = to_type.direct(types);
    if from_type == to_type {
        output.push_str(variable);
        return;
    }
    let mut conversion_function_name = String::new();
    emit_conversion_function_name(from_type, to_type, &mut conversion_function_name);
    if !conversions.declared.contains(&(from_type, to_type)) {
        conversions.declared.insert((from_type, to_type));
        let mut conversion_signature = String::new();
        emit_type(to_type, types, &mut conversion_signature);
        conversion_signature.push_str(" ");
        conversion_signature.push_str(&conversion_function_name);
        conversion_signature.push_str("(");
        emit_type(from_type, types, &mut conversion_signature);
        conversion_signature.push_str(" from)");
        conversions.declarations.push_str(&conversion_signature);
        conversions.declarations.push_str(";\n");
        conversions.bodies.push_str(&conversion_signature);
        conversions.bodies.push_str(" {\n");
        match to_type {
            IrType::Unit | IrType::Boolean | IrType::Integer | IrType::Float | IrType::String |
            IrType::Array(_) | IrType::Closure(_) | IrType::Indirect(_) => {
                panic!("The type {:?} may not be involved in type conversions!", to_type);
            }
            IrType::Object(to_object_idx) => match from_type {
                IrType::Object(_) => {
                    conversions.bodies.push_str("    return (");
                    emit_type(to_type, types, &mut conversions.bodies);
                    conversions.bodies.push_str(") {\n");
                    for (member_name, member_type) in types.get_object(to_object_idx) {
                        conversions.bodies.push_str("        .member");
                        conversions.bodies.push_str(&member_name.0.to_string());
                        conversions.bodies.push_str(" = ");
                        if let IrType::Variants(_) = member_type.direct(types) {
                            conversions.bodies.push_str("(");
                            emit_type(*member_type, types, &mut conversions.bodies);
                            conversions.bodies.push_str("*) ");
                        }
                        conversions.bodies.push_str("from.member");
                        conversions.bodies.push_str(&member_name.0.to_string());
                        conversions.bodies.push_str(",\n");
                    }
                    conversions.bodies.push_str("    };\n");
                }
                IrType::ConcreteObject(_) => {
                    conversions.bodies.push_str("    ");
                    emit_type(to_type, types, &mut conversions.bodies);
                    conversions.bodies.push_str(" result;\n");
                    conversions.bodies.push_str("    GeraAllocation* allocation = gera___rc_alloc(sizeof(");
                    emit_object_alloc_name(to_object_idx.0, &mut conversions.bodies);
                    conversions.bodies.push_str("), &");
                    emit_object_free_handler_name(to_object_idx.0, &mut conversions.bodies);
                    conversions.bodies.push_str(");\n");
                    conversions.bodies.push_str("    result.allocation = allocation;\n");
                    conversions.bodies.push_str("    ");
                    emit_object_alloc_name(to_object_idx.0, &mut conversions.bodies);
                    conversions.bodies.push_str("* object = (");
                    emit_object_alloc_name(to_object_idx.0, &mut conversions.bodies);
                    conversions.bodies.push_str("*) allocation->data;\n");
                    for (member_name, member_type) in types.get_object(to_object_idx) {
                        conversions.bodies.push_str("    object->member");
                        conversions.bodies.push_str(&member_name.0.to_string());
                        conversions.bodies.push_str(" = from.");
                        conversions.bodies.push_str(strings.get(*member_name));
                        conversions.bodies.push_str(";\n");
                        let mut member_value_incr_str = String::new();
                        emit_rc_incr(
                            &format!("from.{}", strings.get(*member_name)),
                            *member_type, types, strings, &mut member_value_incr_str
                        );
                        indent(&member_value_incr_str, &mut conversions.bodies);
                        conversions.bodies.push_str("    ");
                        conversions.bodies.push_str("result.member");
                        conversions.bodies.push_str(&member_name.0.to_string());
                        conversions.bodies.push_str(" = &object->member");
                        conversions.bodies.push_str(&member_name.0.to_string());
                        conversions.bodies.push_str(";\n");
                    }
                    conversions.bodies.push_str("    return result;\n");
                }
                _ => panic!("Cannot convert a {:?} to a {:?}!", from_type, to_type)
            },
            IrType::ConcreteObject(to_cobject_idx) => match from_type {
                IrType::Object(_) => {
                    conversions.bodies.push_str("    return (");
                    emit_type(to_type, types, &mut conversions.bodies);
                    conversions.bodies.push_str(") {\n");
                    for (member_name, member_type) in types.get_concrete_object(to_cobject_idx) {
                        conversions.bodies.push_str("        .");
                        conversions.bodies.push_str(strings.get(*member_name));
                        conversions.bodies.push_str(" = *((");
                        emit_type(*member_type, types, &mut conversions.bodies);
                        conversions.bodies.push_str("*) from.member");
                        conversions.bodies.push_str(&member_name.0.to_string());
                        conversions.bodies.push_str("),\n");
                    }
                    conversions.bodies.push_str("    };\n");
                }
                IrType::ConcreteObject(_) => {
                    panic!("Should not need to directly convert a cobject to a cobject?");
                }
                _ => panic!("Cannot convert a {:?} to a {:?}!", from_type, to_type)
            },
            IrType::Variants(to_variant_idx) => if let IrType::Variants(from_variant_idx) = from_type {
                let from_has_values = types.get_variants(from_variant_idx).iter().find(|(_, vt)|
                    if let IrType::Unit = vt.direct(types) { false } else { true }
                ).is_some();
                let to_has_values = types.get_variants(to_variant_idx).iter().find(|(_, vt)|
                    if let IrType::Unit = vt.direct(types) { false } else { true }
                ).is_some();
                conversions.bodies.push_str("    ");
                emit_type(to_type, types, &mut conversions.bodies);
                conversions.bodies.push_str(" to;\n");
                if to_has_values && from_has_values {
                    conversions.bodies.push_str("    for(unsigned long long int b = 0; b < sizeof(");
                    emit_variants_name(from_variant_idx.0, &mut conversions.bodies);
                    conversions.bodies.push_str("Value); b += 1) {\n");
                    conversions.bodies.push_str("        ((char*) &to.value)[b] = ((char*) &from.value)[b];\n");
                    conversions.bodies.push_str("    }\n");
                }
                conversions.bodies.push_str("    to.tag = from.tag;\n");
                conversions.bodies.push_str("    return to;\n");
            } else { panic!("Cannot convert a {:?} to a {:?}!", from_type, to_type); }
        }
        conversions.bodies.push_str("}\n");
    }
    output.push_str(&conversion_function_name);
    output.push_str("(");
    output.push_str(variable);
    output.push_str(")");
}

fn emit_variable(variable: IrVariable, output: &mut String) {
    output.push_str("local");
    output.push_str(&variable.index.to_string());
}

fn emit_scope_decrements(
    free: &HashSet<usize>,
    variable_types: &Vec<IrType>,
    types: &IrTypeBank,
    strings: &StringMap,
    output: &mut String
) {
    for variable_idx in free {
        let mut variable_str = String::new();
        emit_variable(IrVariable { index: *variable_idx, version: 0 }, &mut variable_str);
        emit_rc_decr(
            &variable_str,
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
        String::from(r#"
return param0.allocation == param1.allocation;
"#)
    });
    builtins.insert(path_from(&["core", "tag_eq"], strings), |_, _, _, _| {
        String::from(r#"
return param0.tag == param1.tag;
"#)
    });
    builtins.insert(path_from(&["core", "length"], strings), |_, _, _, _| {
        String::from(r#"
return param0.length;
"#)
    });
    builtins.insert(path_from(&["core", "array"], strings), |param_types, return_type, types, strings| {
        let array_idx = if let IrType::Array(array_idx) = return_type.direct(types) {
            array_idx.0
        } else { panic!("should be an array!"); };
        let mut result = String::new();
        result.push_str(r#"
if(param1 < 0) {
    size_t error_message_length = snprintf(NULL, 0, "the array length %lld is not valid", param1);
    char error_message[error_message_length + 1];
    sprintf(error_message, "the array length %lld is not valid", param1);
    gera___panic(error_message);
}"#);
        result.push_str("GeraArray result;\n");
        result.push_str("GeraAllocation* allocation = gera___rc_alloc(");
        if let IrType::Unit = param_types[0].direct(types) {
            result.push_str("1, &gera___free_nothing");
        } else {
            result.push_str("param1 == 0? 1 : sizeof(");
            emit_type(param_types[0], types, &mut result);
            result.push_str(") * param1, &");
            emit_array_free_handler_name(array_idx, &mut result);
        }
        result.push_str(");\n");
        result.push_str("result.allocation = allocation;\n");
        result.push_str("result.data = allocation->data;\n");
        result.push_str("result.length = param1;\n");
        if let IrType::Unit = param_types[0].direct(types) {} else {
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
        }
        result.push_str("return result;\n");
        result
    });
    builtins.insert(path_from(&["core", "exhaust"], strings), |_, _, _, strings| {
        format!("
gera___st_push(\"<closure>\", \"???\", 0);
while(((param0.procedure)(param0.allocation)).tag == {}) {{}}
gera___st_pop();
", strings.insert("next").0)
    });
    builtins.insert(path_from(&["core", "panic"], strings), |_, _, _, _| {
        String::from(r#"
GERA_STRING_NULL_TERM(param0, message_nt);
gera___panic(message_nt);
"#)
    });
    builtins.insert(path_from(&["core", "as_str"], strings), |param_types, _, types, strings| {
        match param_types[0].direct(types) {
            IrType::Unit => String::from(r#"
return gera___alloc_string("<unit>");
"#),
            IrType::Boolean => String::from(r#"
return gera___alloc_string(param0? "true" : "false");
"#),
            IrType::Integer => String::from(r#"
size_t result_length = snprintf(NULL, 0, "%lld", param0);
char result[result_length + 1];
sprintf(result, "%lld", param0);
return gera___alloc_string(result);
"#),
            IrType::Float => String::from(r#"
size_t result_length = snprintf(NULL, 0, "%f", param0);
char result[result_length + 1];
sprintf(result, "%f", param0);
return gera___alloc_string(result);
"#),
            IrType::String => {
                let mut result = String::new();
                emit_rc_incr("param0", param_types[0], types, strings, &mut result);
                result.push_str("return param0;\n");
                result
            }
            IrType::Array(_) => String::from(r#"
size_t result_length = snprintf(NULL, 0, "<array %p>", param0.allocation);
char result[result_length + 1];
sprintf(result, "<array %p>", param0.allocation);
return gera___alloc_string(result);
"#),
            IrType::Object(_) => String::from(r#"
size_t result_length = snprintf(NULL, 0, "<object %p>", param0.allocation);
char result[result_length + 1];
sprintf(result, "<object %p>", param0.allocation);
return gera___alloc_string(result);
"#),
            IrType::ConcreteObject(_) => String::from(r#"
return gera___alloc_string("<object>");
"#),
            IrType::Variants(variant_idx) => {
                let variant_types = types.get_variants(variant_idx);
                let mut result = String::from("switch(param0.tag) {\n");
                for (variant_name, _) in variant_types {
                    result.push_str("    case ");
                    result.push_str(&variant_name.0.to_string());
                    result.push_str(": return gera___alloc_string(\"#");
                    result.push_str(strings.get(*variant_name));
                    result.push_str(" <...>\");\n");
                }
                result.push_str("}\n");
                result
            }
            IrType::Closure(_) => String::from(r#"
size_t result_length = snprintf(NULL, 0, "<closure %p>", param0.allocation);
char result[result_length + 1];
sprintf(result, "<closure %p>", param0.allocation);
return gera___alloc_string(result);
"#),
            IrType::Indirect(_) => panic!("should be direct"),
        }
    });
    builtins.insert(path_from(&["core", "as_int"], strings), |_, _, _, _| {
        String::from(r#"
return (gint) param0;
"#)
    });
    builtins.insert(path_from(&["core", "as_flt"], strings), |_, _, _, _| {
        String::from(r#"
return (gfloat) param0;
"#)
    });
    builtins.insert(path_from(&["core", "substring"], strings), |_, _, _, _| {
        String::from(r#"
gint start_idx = param1;
if(param1 < 0) { start_idx = param0.length + param1; }
if(start_idx > param0.length) { 
    size_t error_message_length = snprintf(NULL, 0, "the start index %lld is out of bounds for a string of length %lld", param1, param0.length);
    char error_message[error_message_length + 1];
    sprintf(error_message, "the start index %lld is out of bounds for a string of length %lld", param1, param0.length);
    gera___panic(error_message);
}
gint end_idx = param2;
if(param2 < 0) { end_idx = param0.length + param2; }
if(end_idx > param0.length) {
    size_t error_message_length = snprintf(NULL, 0, "the end index %lld is out of bounds for a string of length %lld", param2, param0.length);
    char error_message[error_message_length + 1];
    sprintf(error_message, "the end index %lld is out of bounds for a string of length %lld", param2, param0.length);
    gera___panic(error_message);
}
if(start_idx > end_idx) {
    size_t error_message_length = snprintf(NULL, 0, "the start index %lld is larger than the end index %lld (length of string is %lld)", param1, param2, param0.length);
    char error_message[error_message_length + 1];
    sprintf(error_message, "the start index %lld is larger than the end index %lld (length of string is %lld)", param1, param2, param0.length);
    gera___panic(error_message);
}
return gera___substring(param0, param1, param2);
"#)
    });
    builtins.insert(path_from(&["core", "concat"], strings), |_, _, _, _| {
        String::from(r#"
return gera___concat(param0, param1);
"#)
    });
    builtins.insert(path_from(&["core", "parse_flt"], strings), |_, return_type, types, strings| {
        let variant_idx = if let IrType::Variants(v) = return_type.direct(types) { v }
        else { panic!("should be variants"); };
        let mut result = String::new();
        result.push_str("gfloat value = gera___parse_flt(param0);\n");
        result.push_str("if(gera___parse_success) { return (");
        emit_variants_name(variant_idx.0, &mut result);
        result.push_str(") { .tag = ");
        result.push_str(&strings.insert("some").0.to_string());
        result.push_str(", .value = { .some = value } }; }\n");
        result.push_str("return (");
        emit_variants_name(variant_idx.0, &mut result);
        result.push_str(") { .tag = ");
        result.push_str(&strings.insert("none").0.to_string());
        result.push_str(" };\n");
        result
    });
    builtins.insert(path_from(&["core", "parse_int"], strings), |_, return_type, types, strings| {
        let variant_idx = if let IrType::Variants(v) = return_type.direct(types) { v }
        else { panic!("should be variants"); };
        let mut result = String::new();
        result.push_str("gint value = gera___parse_int(param0);\n");
        result.push_str("if(gera___parse_success) { return (");
        emit_variants_name(variant_idx.0, &mut result);
        result.push_str(") { .tag = ");
        result.push_str(&strings.insert("some").0.to_string());
        result.push_str(", .value = { .some = value } }; }\n");
        result.push_str("return (");
        emit_variants_name(variant_idx.0, &mut result);
        result.push_str(") { .tag = ");
        result.push_str(&strings.insert("none").0.to_string());
        result.push_str(" };\n");
        result
    });
    builtins.insert(path_from(&["core", "string"], strings), |_, _, _, _| {
        String::from(r#"
if(param1 < 0) {
    size_t error_message_length = snprintf(NULL, 0, "the string repetition count %lld is not valid", param1);
    char error_message[error_message_length + 1];
    sprintf(error_message, "the string repetition count %lld is not valid", param1);
    gera___panic(error_message);
}
GeraAllocation* allocation = gera___rc_alloc(param1 == 0? 1 : param0.length_bytes * param1, &gera___free_nothing);
GeraString result;
result.allocation = allocation;
result.data = allocation->data;
result.length = param0.length * param1;
result.length_bytes = param0.length_bytes * param1;
for(size_t i = 0; i < result.length_bytes; i += 1) {
    result.data[i] = param0.data[i % param0.length_bytes];
}
return result;
"#)
    });
    builtins.insert(path_from(&["core", "hash"], strings), |param_types, _, types, _| {
        match param_types[0].direct(types) {
            IrType::Unit => String::from(r#"
return 0;
"#),
            IrType::Boolean |
            IrType::Integer |
            IrType::Float |
            IrType::Array(_) |
            IrType::Object(_) |
            IrType::ConcreteObject(_) |
            IrType::Variants(_) |
            IrType::Closure(_) => {
                let mut result = String::new();
                result.push_str("return gera___hash((unsigned char*) &param0, sizeof(");
                emit_type(param_types[0], types, &mut result);
                result.push_str("));\n");
                result
            }
            IrType::String => String::from(r#"
return gera___hash((unsigned char*) param0.data, param0.length_bytes);
"#),
            IrType::Indirect(_) => panic!("should be direct")
        }
    });
    return builtins;
}

fn emit_variable_default_value(
    variable_type: IrType,
    types: &IrTypeBank,
    output: &mut String
) {
    match variable_type.direct(types) {
        IrType::Unit => panic!("should not be unit"),
        IrType::Boolean => output.push_str("0"),
        IrType::Integer => output.push_str("0"),
        IrType::Float => output.push_str("0.0"),
        IrType::String | IrType::Array(_) | IrType::Object(_) | IrType::Closure(_) => {
            output.push_str("(");
            emit_type(variable_type, types, output);
            output.push_str(") { .allocation = NULL }");
        }
        IrType::ConcreteObject(_) => {
            output.push_str("(");
            emit_type(variable_type, types, output);
            output.push_str(") { 0 }");
        }
        IrType::Variants(_) => {
            output.push_str("(");
            emit_type(variable_type, types, output);
            output.push_str(") { .tag = 0xFFFFFFFF }");
        }
        IrType::Indirect(_) => panic!("should be direct")
    }
}

fn emit_procedure_impls(
    symbols: &Vec<IrSymbol>,
    types: &IrTypeBank,
    constants: &mut ConstantPool,
    strings: &mut StringMap,
    closure_bodies: &mut Vec<String>,
    conversions: &mut ConversionFunctions,
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
                let mut had_param = false;
                for p in 0..parameter_types.len() {
                    if let IrType::Unit = parameter_types[p].direct(types) { continue; }
                    if had_param { output.push_str(", "); }
                    had_param = true;
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
                    output.push_str(" = ");
                    emit_variable_default_value(variables[variable_idx], types, output);
                    output.push_str(";\n");
                }
                if let IrType::Unit = return_type.direct(types) {} else {
                    output.push_str("    ");
                    emit_type(*return_type, types, output);
                    output.push_str(" returned;\n");
                }
                let mut body_str = String::new();
                let mut body_free = HashSet::new();
                emit_block(
                    body, variables, *return_type, &mut body_free, &mut HashMap::new(), 
                    closure_bodies, conversions, types, constants, external, symbols, strings, &mut body_str
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
                let mut had_param = false;
                for p in 0..parameter_types.len() {
                    if let IrType::Unit = parameter_types[p].direct(types) { continue; }
                    if had_param { output.push_str(", "); }
                    had_param = true;
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

fn emit_constant_name(idx: usize, output: &mut String) {
    output.push_str("geraconstant");
    output.push_str(&idx.to_string());
}

fn emit_constant_declarations(constants: &ConstantPool, types: &mut IrTypeBank, output: &mut String) {
    for vi in 0..constants.get_value_count() {
        match constants.get_value(vi) {
            ConstantPoolValue::String(v) => {
                output.push_str("const GeraString ");
                emit_constant_name(vi, output);
                output.push_str(" = {\n");
                output.push_str("    .allocation = NULL,\n");
                output.push_str("    .length_bytes = ");
                output.push_str(&v.len().to_string());
                output.push_str(",\n");
                output.push_str("    .length = ");
                output.push_str(&v.chars().count().to_string());
                output.push_str(",\n");
                output.push_str("    .data = ");
                emit_string_literal(v, output);
                output.push_str("\n");
                output.push_str("};\n");
            }
            ConstantPoolValue::Array(values, element_type) => {
                if let IrType::Unit = element_type.direct(types) {
                    output.push_str("char");
                } else {
                    emit_type(element_type, types, output);
                }
                output.push_str(" ");
                emit_constant_name(vi, output);
                output.push_str("values[");
                if let IrType::Unit = element_type.direct(types) {
                    output.push_str("1");
                } else {
                    output.push_str(&values.len().to_string());
                }
                output.push_str("];\n");
                let constant_type = IrType::Array(types.insert_array(element_type));
                emit_type(constant_type, types, output);
                output.push_str(" ");
                emit_constant_name(vi, output);
                output.push_str(";\n");
            }
            ConstantPoolValue::Object(members) => {
                let constant_object_idx = types.insert_object(
                    members.iter().map(|(mn, (_, mt))| (*mn, *mt)).collect()
                );
                let constant_type = IrType::Object(constant_object_idx);
                emit_object_alloc_name(constant_object_idx.0, output);
                output.push_str(" ");
                emit_constant_name(vi, output);
                output.push_str("values;\n");
                emit_type(constant_type, types, output);
                output.push_str(" ");
                emit_constant_name(vi, output);
                output.push_str(";\n");
            }
            ConstantPoolValue::Variant(_, _, _) => {}
        }
    }
}

fn emit_constant_initializers(
    constants: &ConstantPool, static_var_vals: &HashMap<NamespacePath, (ConstantValue, IrType)>,
    types: &mut IrTypeBank, strings: &StringMap, output: &mut String
) {
    output.push_str("void gera_init_constants(void) {\n");
    let mut cinit = String::new();
    for vi in 0..constants.get_value_count() {
        match constants.get_value(vi) {
            ConstantPoolValue::String(_) => {}
            ConstantPoolValue::Array(values, element_type) => {
                if let IrType::Unit = element_type.direct(types) {} else {
                    for (i, value) in values.iter().enumerate() {
                        emit_constant_name(vi, &mut cinit);
                        cinit.push_str("values[");
                        cinit.push_str(&i.to_string());
                        cinit.push_str("] = ");
                        emit_value(
                            *value, element_type, types, constants, strings, &mut cinit
                        );
                        cinit.push_str(";\n");
                    }
                }
                let constant_type = IrType::Array(types.insert_array(element_type));
                emit_constant_name(vi, &mut cinit);
                cinit.push_str(" = (");
                emit_type(constant_type, types, &mut cinit);
                cinit.push_str(") {\n");
                cinit.push_str("    .allocation = NULL,\n");
                cinit.push_str("    .length = ");
                cinit.push_str(&values.len().to_string());
                cinit.push_str(",\n");
                cinit.push_str("    .data = (char*) ");
                emit_constant_name(vi, &mut cinit);
                cinit.push_str("values\n");
                cinit.push_str("};\n");
            }
            ConstantPoolValue::Object(members) => {
                let constant_object_idx = types.insert_object(
                    members.iter().map(|(mn, (_, mt))| (*mn, *mt)).collect()
                );
                emit_constant_name(vi, &mut cinit);
                cinit.push_str("values = (");
                emit_object_alloc_name(constant_object_idx.0, &mut cinit);
                cinit.push_str(") {\n");
                for (member_name, (member_value, member_type)) in members {
                    cinit.push_str("    .member");
                    cinit.push_str(&member_name.0.to_string());
                    cinit.push_str(" = ");
                    emit_value(*member_value, *member_type, types, constants, strings, &mut cinit);
                    cinit.push_str(",\n");
                }
                cinit.push_str("};\n");
                let constant_type = IrType::Object(constant_object_idx);
                emit_constant_name(vi, &mut cinit);
                cinit.push_str(" = (");
                emit_type(constant_type, types, &mut cinit);
                cinit.push_str(") {\n");
                cinit.push_str("    .allocation = NULL,\n");
                for (member_name, _) in members {
                    cinit.push_str("    .member");
                    cinit.push_str(&member_name.0.to_string());
                    cinit.push_str(" = &");
                    emit_constant_name(vi, &mut cinit);
                    cinit.push_str("values.member");
                    cinit.push_str(&member_name.0.to_string());
                    cinit.push_str(",\n");
                }
                cinit.push_str("};\n");
            }
            ConstantPoolValue::Variant(_, _, _) => {}
        }
    }
    for (path, (value, value_type)) in static_var_vals {
        emit_path(path, strings, &mut cinit);
        cinit.push_str(" = ");
        emit_value(*value, *value_type, types, constants, strings, &mut cinit);
        cinit.push_str(";\n");
    }
    indent(&cinit, output);
    output.push_str("}\n");
}

fn emit_main_function(
    main_procedure_path: &NamespacePath,
    strings: &StringMap,
    output: &mut String
) {
    output.push_str("int GERA_ARGC;\n");
    output.push_str("char** GERA_ARGV;\n");
    output.push_str("int main(int argc, char** argv) {\n");
    output.push_str("    GERA_ARGC = argc;\n");
    output.push_str("    GERA_ARGV = argv;\n");
    output.push_str("    gera_init_constants();\n");
    output.push_str("    gera___st_init();\n");
    output.push_str("    gera___st_push(");
    emit_string_literal(&main_procedure_path.display(strings), output);
    output.push_str(", \"???\", 0);\n");
    output.push_str("    ");
    emit_procedure_name(main_procedure_path, 0, strings, output);
    output.push_str("();\n");
    output.push_str("    return 0;\n");
    output.push_str("}\n");
}

fn emit_type(t: IrType, types: &IrTypeBank, output: &mut String) {
    match t {
        IrType::Unit => output.push_str("void"),
        IrType::Boolean => output.push_str("gbool"),
        IrType::Integer => output.push_str("gint"),
        IrType::Float => output.push_str("gfloat"),
        IrType::String => output.push_str("GeraString"),
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
        &value.replace("\\", "\\\\")
            .replace("\n", "\\n")
            .replace("\r", "\\r")
            .replace("\"", "\\\"")
    );
    output.push('"');
}

fn emit_value(
    value: ConstantValue, value_type: IrType, types: &IrTypeBank, constants: &ConstantPool,
    strings: &StringMap, output: &mut String
) {
    match value {
        ConstantValue::Unit => panic!("should not have to emit unit value!"),
        ConstantValue::Boolean(b) => output.push_str(if b { "1" } else { "0" }),
        ConstantValue::Integer(i) => {
            if i == i64::MIN {
                output.push_str(&format!("-{} - 1", i64::MAX));
            } else { output.push_str(&i.to_string()) }
        }
        ConstantValue::Float(f) => {
            if f == f64::INFINITY { output.push_str("(1.0 / 0.0)"); }
            else if f == f64::NEG_INFINITY { output.push_str("(-1.0 / 0.0)"); }
            else if f.is_nan() { output.push_str("(0.0 / 0.0)"); }
            else { output.push_str(&format!("{:.}", f)); }
        }
        ConstantValue::String(s) => emit_constant_name(s.into(), output),
        ConstantValue::Array(a) => emit_constant_name(a.into(), output),
        ConstantValue::Object(o) => emit_constant_name(o.into(), output),
        ConstantValue::Variant(v) => {
            let variant_idx = if let IrType::Variants(variant_idx) = value_type.direct(types) {
                variant_idx.0
            } else { panic!("value type should be variants"); };
            let (variant_tag, variant_value, _) = constants.get_variant(v);
            output.push_str("(");
            emit_variants_name(variant_idx, output);
            output.push_str(") { .tag = ");
            output.push_str(&variant_tag.0.to_string());
            let variant_type = types.get_all_variants()[variant_idx].get(&variant_tag)
                .expect("variant should exist").direct(types);
            if let IrType::Unit = variant_type {} else {
                output.push_str(", .value = { .");
                output.push_str(strings.get(variant_tag));
                output.push_str(" = ");
                emit_value(variant_value, variant_type, types, constants, strings, output);
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
    return_type: IrType,
    free: &mut HashSet<usize>,
    capture_types: &mut HashMap<StringIdx, IrType>,
    closure_bodies: &mut Vec<String>,
    conversions: &mut ConversionFunctions,
    types: &IrTypeBank,
    constants: &mut ConstantPool,
    external: &HashMap<NamespacePath, StringIdx>,
    symbols: &Vec<IrSymbol>,
    strings: &StringMap,
    output: &mut String
) {
    output.push_str("{\n");
    for instruction in instructions {
        let mut o = String::new();
        emit_instruction(
            instruction, variable_types, return_type, free, capture_types, closure_bodies,
            conversions, types, constants, external, symbols, strings, &mut o
        );
        indent(&o, output);
    }
    output.push_str("}");
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
    return_type: IrType,
    free: &mut HashSet<usize>,
    capture_types: &mut HashMap<StringIdx, IrType>,
    closure_bodies: &mut Vec<String>,
    conversions: &mut ConversionFunctions,
    types: &IrTypeBank,
    constants: &mut ConstantPool,
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
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            output.push_str(&into_str);
            output.push_str(" = gera___alloc_string(");
            emit_string_literal(strings.get(*value), output);
            output.push_str(");\n");
        }
        IrInstruction::LoadObject { member_values, into } => {
            let object_idx = if let IrType::Object(object_idx) = variable_types[into.index]
                .direct(types) {
                object_idx.0
            } else { panic!("should be an object"); };
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            output.push_str("{\n    GeraAllocation* allocation = gera___rc_alloc(sizeof(");
            emit_object_alloc_name(object_idx, output);
            output.push_str("), &");
            emit_object_free_handler_name(object_idx, output);
            output.push_str(");\n    ");
            output.push_str(&into_str);
            output.push_str(".allocation = allocation;\n    ");
            emit_object_alloc_name(object_idx, output);
            output.push_str("* object = (");
            emit_object_alloc_name(object_idx, output);
            output.push_str("*) allocation->data;\n");
            for (member_name, member_value) in member_values {
                if let IrType::Unit = variable_types[member_value.index].direct(types) { continue; }
                let member_type = *types.get_all_objects()[object_idx].get(member_name)
                    .expect("member should exist");
                output.push_str("    object->member");
                output.push_str(&member_name.0.to_string());
                output.push_str(" = ");
                let mut member_value_str = String::new();
                emit_variable(*member_value, &mut member_value_str);
                emit_implicit_conversion(
                    &member_value_str, variable_types[member_value.index], member_type, conversions,
                    types, strings, output
                );
                output.push_str(";\n");
                let mut member_value_incr_str = String::new();
                emit_rc_incr(
                    &member_value_str, variable_types[member_value.index], types, 
                    strings, &mut member_value_incr_str
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
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            let element_type = types.get_all_arrays()[array_idx];
            output.push_str("{\n");
            output.push_str("    GeraAllocation* allocation = gera___rc_alloc(");
            if let IrType::Unit = element_type.direct(types) {
                output.push_str("1, &gera___free_nothing");
            } else if element_values.len() == 0 {
                output.push_str("1, &gera___free_nothing");
            } else {
                output.push_str("sizeof(");
                emit_type(element_type, types, output);
                output.push_str(") * ");
                output.push_str(&element_values.len().to_string());
                output.push_str(", &");
                emit_array_free_handler_name(array_idx, output);
            }
            output.push_str(");\n");
            output.push_str("    ");
            output.push_str(&into_str);
            output.push_str(".allocation = allocation;\n");
            output.push_str("    ");
            output.push_str(&into_str);
            output.push_str(".data = allocation->data;\n");
            output.push_str("    ");
            output.push_str(&into_str);
            output.push_str(".length = ");
            output.push_str(&element_values.len().to_string());
            output.push_str(";\n");
            if let IrType::Unit = element_type.direct(types) {} else {
                output.push_str("    ");
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
                    emit_implicit_conversion(
                        &element_value_str, variable_types[element_values[value_idx].index],
                        element_type, conversions, types, strings, output
                    );
                    output.push_str(";\n");
                    let mut element_value_incr_str = String::new();
                    emit_rc_incr(
                        &element_value_str, variable_types[element_values[value_idx].index], types,
                        strings, &mut element_value_incr_str
                    );
                    indent(&element_value_incr_str, output);
                }
            }
            output.push_str("}\n");
            free.insert(into.index);
        }
        IrInstruction::LoadVariant { name, v, into } => {
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            output.push_str(&into_str);
            output.push_str(" = ");
            let variant_idx = if let IrType::Variants(v)
                = variable_types[into.index].direct(types) {
                    v.0
                }
                else { panic!("should be a variant"); };
            output.push_str("(");
            emit_variants_name(variant_idx, output);
            output.push_str(") { .tag = ");
            output.push_str(&name.0.to_string());
            if let IrType::Unit = variable_types[v.index].direct(types) {} else {
                output.push_str(", .value = { .");
                output.push_str(strings.get(*name));
                output.push_str(" = ");
                let mut v_str = String::new();
                emit_variable(*v, &mut v_str);
                emit_implicit_conversion(
                    &v_str, variable_types[v.index], *types.get_all_variants()[variant_idx]
                        .get(name).expect("should have variant"),
                    conversions, types, strings, output
                );
                output.push_str(" }");
            }
            output.push_str(" };\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
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
                    let mut into_str = String::new();
                    emit_variable(*into, &mut into_str);
                    emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
                    output.push_str("    ");
                    output.push_str("{\n");
                    output.push_str("    GeraAllocation* allocation = gera___rc_alloc(1, &gera___free_nothing);\n");
                    output.push_str(&into_str);
                    output.push_str(".allocation = allocation;\n");
                    output.push_str("    ");
                    output.push_str(&into_str);
                    output.push_str(".procedure = &");
                    emit_closure_body_name(closure_idx, closure_variant, output);
                    output.push_str(";\n");
                    output.push_str("}\n");
                    free.insert(into.index);
                }
                IrSymbol::Variable { .. } |
                IrSymbol::ExternalVariable { .. } => {
                    let mut into_str = String::new();
                    emit_variable(*into, &mut into_str);
                    emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
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
            }
        }
        IrInstruction::LoadParameter { index, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            output.push_str(&into_str);
            output.push_str(" = param");
            output.push_str(&index.to_string());
            output.push_str(";\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
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
            // body needs to be done here because we need nested closures to register FIRST
            let mut body_str = String::new();
            let mut body_free = HashSet::new();
            emit_block(
                body, variables, *return_type, &mut body_free,
                &mut captured.iter().map(|(cn, cv)| (*cn, variable_types[cv.index])).collect(),
                closure_bodies, conversions, types, constants, external, symbols, strings,
                &mut body_str
            );
            let variant = closure_bodies.len();
            // emit closure captures struct
            closure_body.push_str("typedef struct ");
            emit_closure_captures_name(closure_idx, variant, &mut closure_body);
            closure_body.push_str(" {\n");
            for (capture_name, capture_variable) in captured {
                let capture_type = variable_types[capture_variable.index];
                if let IrType::Unit = capture_type.direct(types) { continue; }
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
                if let IrType::Unit = capture_type.direct(types) { continue; }
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
                if let IrType::Unit = parameter_types[param_idx].direct(types) { continue; }
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
                closure_body.push_str(" = ");
                emit_variable_default_value(variables[variable_idx], types, &mut closure_body);
                closure_body.push_str(";\n");
            }
            if let IrType::Unit = return_type.direct(types) {} else {
                closure_body.push_str("    ");
                emit_type(*return_type, types, &mut closure_body);
                closure_body.push_str(" returned;\n");
            }
            body_str.push_str("\nret:\n");
            emit_scope_decrements(&body_free, variables, types, strings, &mut body_str);
            indent(&body_str, &mut closure_body);
            if let IrType::Unit = return_type.direct(types) {} else {
                closure_body.push_str("    return returned;\n");
            }
            closure_body.push_str("}\n");
            closure_bodies.push(closure_body);
            // emit closure literal
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            output.push_str("    ");
            output.push_str("{\n");
            output.push_str("    GeraAllocation* allocation = gera___rc_alloc(sizeof(");
            emit_closure_captures_name(closure_idx, variant, output);
            output.push_str("), &");
            emit_closure_free_name(closure_idx, variant, output);
            output.push_str(");\n");
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
                if let IrType::Unit = variable_types[capture_value.index].direct(types) {
                    continue;
                }
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
            free.insert(into.index);
        }
        IrInstruction::LoadValue { value, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            let value = constants.insert(value, variable_types[into.index], types);
            emit_value(value, variable_types[into.index], types, constants, strings, output);
            output.push_str(";\n");
        }
        IrInstruction::GetObjectMember { accessed, member, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            let member_type = if let IrType::Object(object_idx) = variable_types[accessed.index]
                .direct(types) {
                *types.get_object(object_idx).get(member).expect("member should exist")
            } else { panic!("accessed should be an object"); };
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            output.push_str(&into_str);
            output.push_str(" = ");
            let mut access_str = String::new();
            access_str.push_str("(*");
            emit_variable(*accessed, &mut access_str);
            access_str.push_str(".member");
            access_str.push_str(&member.0.to_string());
            access_str.push_str(")");
            emit_implicit_conversion(
                &mut access_str, member_type, variable_types[into.index], conversions, types,
                strings, output
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
            emit_implicit_conversion(
                &value_str, variable_types[value.index], member_type, conversions, types,
                strings, output
            );
            output.push_str(";\n");
            emit_rc_incr(&member_str, member_type, types, strings, output);
        }
        IrInstruction::GetArrayElement { accessed, index, into, source } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            let element_type = if let IrType::Array(array_idx) = variable_types[accessed.index]
                .direct(types) {
                *types.get_array(array_idx)
            } else { panic!("should be an array"); };
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            let mut accessed_str = String::new();
            emit_variable(*accessed, &mut accessed_str);
            let mut index_str = String::new();
            emit_variable(*index, &mut index_str);
            output.push_str(&index_str);
            output.push_str(" = gera___verify_index(");
            output.push_str(&index_str);
            output.push_str(", ");
            output.push_str(&accessed_str);
            output.push_str(".length, ");
            emit_string_literal(strings.get(source.file_name()), output);
            output.push_str(", ");
            let source_line = strings.get(source.file_content())[..source.start_position()]
                .lines().collect::<Vec<&str>>().len();
            output.push_str(&source_line.to_string());
            output.push_str(");\n");
            output.push_str(&into_str);
            output.push_str(" = ");
            let mut access_str = String::new();
            access_str.push_str("((");
            emit_type(element_type, types, &mut access_str);
            access_str.push_str("*) ");
            access_str.push_str(&accessed_str);
            access_str.push_str(".data)[");
            access_str.push_str(&index_str);
            access_str.push_str("]");
            emit_implicit_conversion(
                &mut access_str, element_type, variable_types[into.index], conversions, types,
                strings, output
            );
            output.push_str(";\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
            free.insert(into.index);
        }
        IrInstruction::SetArrayElement { value, accessed, index, source } => {
            if let IrType::Unit = variable_types[value.index].direct(types) { return; }
            let element_type = if let IrType::Array(array_idx) = variable_types[accessed.index]
                .direct(types) {
                *types.get_array(array_idx)
            } else { panic!("should be an array"); };
            let mut accessed_str = String::new();
            emit_variable(*accessed, &mut accessed_str);
            let mut index_str = String::new();
            emit_variable(*index, &mut index_str);
            output.push_str(&index_str);
            output.push_str(" = gera___verify_index(");
            output.push_str(&index_str);
            output.push_str(", ");
            output.push_str(&accessed_str);
            output.push_str(".length, ");
            emit_string_literal(strings.get(source.file_name()), output);
            output.push_str(", ");
            let source_line = strings.get(source.file_content())[..source.start_position()]
                .lines().collect::<Vec<&str>>().len();
            output.push_str(&source_line.to_string());
            output.push_str(");\n");
            let mut element_str = String::new();
            element_str.push_str("((");
            emit_type(element_type, types, &mut element_str);
            element_str.push_str("*) ");
            element_str.push_str(&accessed_str);
            element_str.push_str(".data)[");
            element_str.push_str(&index_str);
            element_str.push_str("]");
            emit_rc_decr(&element_str, element_type, types, strings, output);
            output.push_str(&element_str);
            output.push_str(" = ");
            let mut value_str = String::new();
            emit_variable(*value, &mut value_str);
            emit_implicit_conversion(
                &value_str, variable_types[value.index], element_type, conversions, types,
                strings, output
            );
            output.push_str(";\n");
            emit_rc_incr(&element_str, element_type, types, strings, output);
        }
        IrInstruction::GetClosureCapture { name, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            output.push_str(&into_str);
            output.push_str(" = ");
            let mut accessed_str = String::new();
            accessed_str.push_str("captures->");
            accessed_str.push_str(strings.get(*name));
            emit_implicit_conversion(
                &accessed_str, *capture_types.get(name).expect("should be captured"), variable_types[into.index],
                conversions, types, strings, output
            );
            output.push_str(";\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
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
            let mut value_str = String::new();
            emit_variable(*value, &mut value_str);
            emit_implicit_conversion(
                &value_str, variable_types[value.index], *capture_types.get(name).expect("should be captured"),
                conversions, types, strings, output
            );
            output.push_str(";\n");
            emit_rc_incr(&capture_str, variable_types[value.index], types, strings, output);
        }
        IrInstruction::Move { from, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) { return; }
            if *from == *into { return; }
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
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
        IrInstruction::Divide { a, b, into, source } => {
            let mut divisor = String::new();
            emit_variable(*b, &mut divisor);
            if let IrType::Integer = variable_types[a.index].direct(types) {
                output.push_str("gera___verify_integer_divisor(");
                output.push_str(&divisor);
                output.push_str(", ");
                emit_string_literal(strings.get(source.file_name()), output);
                output.push_str(", ");
                let source_line = strings.get(source.file_content())[..source.start_position()]
                    .lines().collect::<Vec<&str>>().len();
                output.push_str(&source_line.to_string());
                output.push_str(");\n");
            }
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_variable(*a, output);
            output.push_str(" / ");
            output.push_str(&divisor);
            output.push_str(";\n");
        }
        IrInstruction::Modulo { a, b, into, source } => {
            let mut divisor = String::new();
            emit_variable(*b, &mut divisor);
            if let IrType::Integer = variable_types[a.index].direct(types) {
                output.push_str("gera___verify_integer_divisor(");
                output.push_str(&divisor);
                output.push_str(", ");
                emit_string_literal(strings.get(source.file_name()), output);
                output.push_str(", ");
                let source_line = strings.get(source.file_content())[..source.start_position()]
                    .lines().collect::<Vec<&str>>().len();
                output.push_str(&source_line.to_string());
                output.push_str(");\n");
            }
            emit_variable(*into, output);
            output.push_str(" = ");
            if let IrType::Float = variable_types[a.index].direct(types) {
                output.push_str("gera___float_mod(");
                emit_variable(*a, output);
                output.push_str(", ");
                output.push_str(&divisor);
                output.push_str(")");
            } else {
                emit_variable(*a, output);
                output.push_str(" % ");
                output.push_str(&divisor);
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
                let bvalue = constants.insert(&branches[branch_idx].0, variable_types[value.index], types);
                emit_value(bvalue, variable_types[value.index], types, constants, strings, output);
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
                    &branches[branch_idx].1, variable_types, return_type, free, capture_types,
                    closure_bodies, conversions, types, constants, external, symbols, strings,
                    &mut branches_str
                );
                branches_str.push_str(" else ");
            }
            emit_block(
                else_branch, variable_types, return_type, free, capture_types, closure_bodies,
                conversions, types, constants, external, symbols, strings, &mut branches_str
            );
            indent(&branches_str, output);
            output.push_str("\n}\n");
        }
        IrInstruction::BranchOnVariant { value, branches, else_branch } => {
            let value_variant_types = if let IrType::Variants(variants_idx) =
                variable_types[value.index].direct(types) {
                types.get_variants(variants_idx)
            } else { panic!("Branch on variant should be on a variant?"); };
            output.push_str("switch(");
            emit_variable(*value, output);
            output.push_str(".tag) {");
            for (branch_variant, branch_variable, branch_body) in branches {
                output.push_str("\n    case ");
                output.push_str(&branch_variant.0.to_string());
                output.push_str(":\n");
                if let Some(branch_variable) = branch_variable {
                    let branch_variable_type = variable_types[branch_variable.index];
                    if let IrType::Unit = branch_variable_type.direct(types) {} else {
                        output.push_str("        ");
                        let mut branch_variable_str = String::new();
                        emit_variable(*branch_variable, &mut branch_variable_str);
                        output.push_str(&branch_variable_str);
                        output.push_str(" = ");
                        let mut branch_variable_value_str = String::new();
                        emit_variable(*value, &mut branch_variable_value_str);
                        branch_variable_value_str.push_str(".value.");
                        branch_variable_value_str.push_str(strings.get(*branch_variant));
                        emit_implicit_conversion(
                            &branch_variable_value_str, *value_variant_types.get(branch_variant)
                                .expect("should have variant"),
                            branch_variable_type, conversions, types, strings, output
                        );
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
                        instruction, variable_types, return_type, free, capture_types,
                        closure_bodies, conversions, types, constants, external, symbols, strings,
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
                        instruction, variable_types, return_type, free, capture_types,
                        closure_bodies, conversions, types, constants, external, symbols, strings,
                        &mut branch
                    );
                }
                let mut branch_indented = String::new();
                indent(&branch, &mut branch_indented);
                indent(&branch_indented, output);
            }
            output.push_str("\n}\n");
        }
        IrInstruction::Call { path, variant, arguments, into, source } => {
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
            output.push_str("gera___st_push(");
            emit_string_literal(&path.display(strings), output);
            output.push_str(", ");
            emit_string_literal(strings.get(source.file_name()), output);
            output.push_str(", ");
            let source_line = strings.get(source.file_content())[..source.start_position()]
                .lines().collect::<Vec<&str>>().len();
            output.push_str(&source_line.to_string());
            output.push_str(");\n");
            let returns_value = if let IrType::Unit = return_type.direct(types) { false }
                else { true };
            let mut value = String::new();
            if let Some(backing) = external.get(path) {
                value.push_str(strings.get(*backing));
            } else {
                emit_procedure_name(path, *variant, strings, &mut value);
            }
            value.push_str("(");
            let mut had_param = false;
            for argument_idx in 0..arguments.len() {
                if let IrType::Unit = parameter_types[argument_idx].direct(types) { continue; }
                if had_param { value.push_str(", "); }
                had_param = true;
                let mut variable = String::new();
                emit_variable(arguments[argument_idx], &mut variable);
                emit_implicit_conversion(
                    &variable,
                    variable_types[arguments[argument_idx].index],
                    parameter_types[argument_idx],
                    conversions, types, strings, &mut value
                );
            }
            value.push_str(")");
            if returns_value {
                let mut into_str = String::new();
                emit_variable(*into, &mut into_str);
                emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
                output.push_str(&into_str);
                output.push_str(" = ");
                emit_implicit_conversion(
                    &value,
                    *return_type,
                    variable_types[into.index],
                    conversions, types, strings, output
                );
            } else {
                output.push_str(&value);
            }
            output.push_str(";\n");
            output.push_str("gera___st_pop();\n");
            free.insert(into.index);
        }
        IrInstruction::CallClosure { called, arguments, into, source } => {
            let (parameter_types, return_type) = if let IrType::Closure(p)
                    = variable_types[called.index].direct(types) {
                types.get_closure(p)
            } else { panic!("should be a closure"); };
            output.push_str("gera___st_push(\"<closure>\", ");
            emit_string_literal(strings.get(source.file_name()), output);
            output.push_str(", ");
            let source_line = strings.get(source.file_content())[..source.start_position()]
                .lines().collect::<Vec<&str>>().len();
            output.push_str(&source_line.to_string());
            output.push_str(");\n");
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
                if let IrType::Unit = parameter_types[argument_idx].direct(types) { continue; }
                value.push_str(", ");
                let mut variable = String::new();
                emit_variable(arguments[argument_idx], &mut variable);
                emit_implicit_conversion(
                    &variable,
                    variable_types[arguments[argument_idx].index],
                    parameter_types[argument_idx],
                    conversions, types, strings, &mut value
                );
            }
            value.push_str(")");
            if returns_value {
                let mut into_str = String::new();
                emit_variable(*into, &mut into_str);
                emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
                output.push_str(&into_str);
                output.push_str(" = ");
                emit_implicit_conversion(
                    &value,
                    *return_type,
                    variable_types[into.index],
                    conversions, types, strings, output
                );
            } else {
                output.push_str(&value);
            }
            output.push_str(";\n");
            output.push_str("gera___st_pop();\n");
            free.insert(into.index);
        }
        IrInstruction::Return { value } => {
            if let IrType::Unit = variable_types[value.index].direct(types) {
                output.push_str("goto ret;\n");
            } else {
                output.push_str("returned = ");
                let mut returned = String::new();
                emit_variable(*value, &mut returned);
                emit_implicit_conversion(
                    &returned, variable_types[value.index], return_type, conversions, types,
                    strings, output
                );
                output.push_str(";\n");
                emit_rc_incr(&returned, variable_types[value.index], types, strings, output);
                output.push_str("goto ret;\n");
            }
        }
        IrInstruction::Phi { .. } => {}
    }
}