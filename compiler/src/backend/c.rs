
use std::collections::{HashMap, HashSet};

use crate::backend::{
    ir::{IrSymbol, IrInstruction, IrVariable},
    constants::{ConstantPool, ConstantValue, ConstantPoolValue}
};
use crate::frontend::{
    modules::NamespacePath,
    types::{TypeMap, TypeGroup, Type}
};
use crate::util::strings::{StringMap, StringIdx};

pub fn generate_c(
    symbols: Vec<IrSymbol>,
    mut types: TypeMap,
    mut constants: ConstantPool,
    main_procedure_path: NamespacePath,
    strings: &mut StringMap
) -> String {
    types = types.deduplicated();
    types.replace_any_with_unit();
    let mut output = String::new();
    let mut external = HashMap::new();
    output.push_str("\n//
// Generated from Gera source code by gerap.
// See: https://github.com/geralang/gerac
//\n");
    output.push_str("\n");
    emit_core_library(&mut output);
    output.push_str("\n");
    let mut static_var_vals = HashMap::new();
    let mut constant_dependants = String::new();
    let mut declared_types = HashMap::new();
    let mut type_declarations = String::new();
    constant_dependants.push_str("\n");
    emit_symbol_declarations(
        &symbols, &mut types, &mut static_var_vals,
        strings, &mut declared_types, &mut type_declarations,
        &mut external, &mut constant_dependants
    );
    let mut closure_bodies = Vec::new();
    let mut procedure_impls = String::new();
    emit_procedure_impls(
        &symbols, &mut types, &mut constants, strings,
        &mut declared_types, &mut type_declarations,
        &mut closure_bodies, &mut external, &mut procedure_impls
    );
    let mut constant_inits = String::new();
    emit_constant_initializers(
        &mut constants, &static_var_vals, &mut external, &symbols,
        &mut closure_bodies, &mut types, strings, &mut declared_types, &mut type_declarations,
        &mut constant_inits
    );
    constant_dependants.push_str("\n");
    for closure_body in &closure_bodies {
        constant_dependants.push_str(&closure_body);
    }
    constant_dependants.push_str("\n");
    constant_dependants.push_str(&procedure_impls);
    let mut constant_decls = String::new();
    emit_constant_declarations(
        &constants, &mut types, strings, &mut declared_types, &mut type_declarations,
        &mut constant_decls
    );
    output.push_str("\n");
    output.push_str(&type_declarations);
    output.push_str("\n");
    output.push_str(&constant_decls);
    output.push_str(&constant_dependants);
    output.push_str("\n");
    output.push_str(&constant_inits);
    output.push_str("\n");
    emit_main_function(&main_procedure_path, strings, &mut output);
    return output;
}

fn emit_core_library(output: &mut String) {
    output.push_str(include_str!("./core/core.c"));
    output.push_str("\n");
}

fn emit_array_name(index: usize, output: &mut String) {
    output.push_str("GeraArray");
    output.push_str(&index.to_string());
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

fn emit_array_free_handler_name(
    index: usize, output: &mut String
) {
    output.push_str("gerafreearray");
    output.push_str(&index.to_string());
}

fn emit_object_free_handler_name(
    index: usize, output: &mut String
) {
    output.push_str("gerafreeobject");
    output.push_str(&index.to_string());
}

fn emit_array_comparison_function_name(
    index: usize, output: &mut String
) {
    output.push_str("geraarrayeq");
    output.push_str(&index.to_string());
}

fn emit_object_comparison_function_name(
    index: usize, output: &mut String
) {
    output.push_str("geraobjecteq");
    output.push_str(&index.to_string());
}

fn emit_variant_comparison_function_name(
    index: usize, output: &mut String
) {
    output.push_str("geravarianteq");
    output.push_str(&index.to_string());
}

fn emit_object_to_concrete_function_name(
    index: usize, output: &mut String
) {
    output.push_str("geraobjecttoconcrete");
    output.push_str(&index.to_string());
}

fn emit_object_to_inconcrete_function_name(
    index: usize, output: &mut String
) {
    output.push_str("geraobjecttoinconcrete");
    output.push_str(&index.to_string());
}

fn emit_type_declaration(
    t: TypeGroup, types: &mut TypeMap, strings: &StringMap,
    declared_types: &mut HashMap<String, bool>, type_declarations: &mut String
) {
    let mut type_iden = String::new();
    match types.group_concrete(t) {
        Type::Any | Type::Unit | Type::Boolean | Type::Integer | Type::Float |
        Type::String => return,
        Type::Array(arr) => emit_array_name(arr.get_internal_id(), &mut type_iden),
        Type::Object(obj) => emit_object_name(obj.get_internal_id(), &mut type_iden),
        Type::ConcreteObject(obj) => emit_concrete_object_name(obj.get_internal_id(), &mut type_iden),
        Type::Closure(clo) => emit_closure_name(clo.get_internal_id(), &mut type_iden),
        Type::Variants(var) => emit_variants_name(var.get_internal_id(), &mut type_iden),
    }
    if let Some(predeclared) = declared_types.get(&type_iden) {
        if *predeclared { return; }
        let mut declaration = String::new();
        match types.group_concrete(t) {
            Type::Any | Type::Unit | Type::Boolean | Type::Integer | Type::Float |
            Type::String => {}
            Type::Array(arr) => {
                let array_idx = arr.get_internal_id();
                declaration.push_str("gbool ");
                emit_array_comparison_function_name(array_idx, &mut declaration);
                declaration.push_str("(GeraArray a, GeraArray b);\n");
            }
            Type::Object(obj) => {
                let object_idx = obj.get_internal_id();
                declaration.push_str("typedef struct ");
                emit_object_name(object_idx, &mut declaration);
                declaration.push_str(" ");
                emit_object_name(object_idx, &mut declaration);
                declaration.push_str(";\n");
                declaration.push_str("typedef struct ");
                emit_object_alloc_name(object_idx, &mut declaration);
                declaration.push_str(" ");
                emit_object_alloc_name(object_idx, &mut declaration);
                declaration.push_str(";\n");
                declaration.push_str("gbool ");
                emit_object_comparison_function_name(object_idx, &mut declaration);
                declaration.push_str("(");
                emit_object_name(object_idx, &mut declaration);
                declaration.push_str(" a, ");
                emit_object_name(object_idx, &mut declaration);
                declaration.push_str(" b);\n");
            }
            Type::ConcreteObject(obj) => {
                let concrete_object_idx = obj.get_internal_id();
                declaration.push_str("typedef struct ");
                emit_concrete_object_name(concrete_object_idx, &mut declaration);
                declaration.push_str(" ");
                emit_concrete_object_name(concrete_object_idx, &mut declaration);
                declaration.push_str(";\n");   
            }
            Type::Closure(clo) => {
                let closure_idx = clo.get_internal_id();
                declaration.push_str("typedef struct ");
                emit_closure_name(closure_idx, &mut declaration);
                declaration.push_str(" ");
                emit_closure_name(closure_idx, &mut declaration);
                declaration.push_str(";\n"); 
            }
            Type::Variants(var) => {
                let variants_idx = var.get_internal_id();
                declaration.push_str("typedef struct ");
                emit_variants_name(variants_idx, &mut declaration);
                declaration.push_str(" ");
                emit_variants_name(variants_idx, &mut declaration);
                declaration.push_str(";\n");
                declaration.push_str("typedef union ");
                emit_variants_name(variants_idx, &mut declaration);
                declaration.push_str("Value ");
                emit_variants_name(variants_idx, &mut declaration);
                declaration.push_str("Value;\n");
                declaration.push_str("gbool ");
                emit_variant_comparison_function_name(variants_idx, &mut declaration);
                declaration.push_str("(");
                emit_variants_name(variants_idx, &mut declaration);
                declaration.push_str(" a, ");
                emit_variants_name(variants_idx, &mut declaration);
                declaration.push_str(" b);\n");
            }
        }
        type_declarations.push_str(&declaration);
        declared_types.insert(type_iden.clone(), true);
        return;
    }
    declared_types.insert(type_iden.clone(), false);
    let mut declaration = String::new();
    match types.group_concrete(t) {
        Type::Any | Type::Unit | Type::Boolean | Type::Integer | Type::Float |
        Type::String => {}
        Type::Array(arr) => {
            let array_idx = arr.get_internal_id();
            let element_type = types.internal_arrays()[array_idx];
            if let Type::Unit = types.group_concrete(element_type) {} else {
                declaration.push_str("void ");
                emit_array_free_handler_name(array_idx, &mut declaration);
                declaration.push_str("(char* data, size_t size) {");
                declaration.push_str("\n    ");
                emit_type(element_type, types, strings, declared_types, type_declarations, &mut declaration);
                declaration.push_str("* elements = (");
                emit_type(element_type, types, strings, declared_types, type_declarations, &mut declaration);
                declaration.push_str("*) data;");
                declaration.push_str("\n    for(size_t i = 0; i < size / sizeof(");
                emit_type(element_type, types, strings, declared_types, type_declarations, &mut declaration);
                declaration.push_str("); i += 1) {\n");
                let mut element_rc_decr = String::new();
                emit_rc_decr("(elements[i])", element_type, types, strings, &mut element_rc_decr);
                let mut element_rc_decr_indented = String::new();
                indent(&element_rc_decr, &mut element_rc_decr_indented);
                indent(&element_rc_decr_indented, &mut declaration);
                declaration.push_str("    }\n}\n");
            }
            declaration.push_str("gbool ");
            emit_array_comparison_function_name(array_idx, &mut declaration);
            declaration.push_str("(GeraArray a, GeraArray b) {\n");
            declaration.push_str("    if(a.length != b.length) { return 0; }\n");
            declaration.push_str("    ");
            emit_type(element_type, types, strings, declared_types, type_declarations, &mut declaration);
            declaration.push_str("* a_elements = (");
            emit_type(element_type, types, strings, declared_types, type_declarations, &mut declaration);
            declaration.push_str("*) a.data;\n");
            declaration.push_str("    ");
            emit_type(element_type, types, strings, declared_types, type_declarations, &mut declaration);
            declaration.push_str("* b_elements = (");
            emit_type(element_type, types, strings, declared_types, type_declarations, &mut declaration);
            declaration.push_str("*) b.data;\n");
            declaration.push_str("    for(size_t i = 0; i < a.length; i += 1) {\n");
            declaration.push_str("        if(!(");
            emit_equality("a_elements[i]", "b_elements[i]", element_type, types, &mut declaration);
            declaration.push_str(")) { return 0; }\n");
            declaration.push_str("    }\n");
            declaration.push_str("    return 1;\n");
            declaration.push_str("}\n");
        }
        Type::Object(obj) => {
            let object_idx = obj.get_internal_id();
            declaration.push_str("typedef struct ");
            emit_object_name(object_idx, &mut declaration);
            declaration.push_str(" {\n    GeraAllocation* allocation;");
            for (member_name, member_type) in types.internal_objects()[object_idx].0.clone() {
                if let Type::Unit = types.group_concrete(member_type) { continue; }
                declaration.push_str("\n    ");
                emit_type(member_type, types, strings, declared_types, type_declarations, &mut declaration);
                declaration.push_str("* member");
                declaration.push_str(&member_name.0.to_string());
                declaration.push_str(";");
            }
            declaration.push_str("\n} ");
            emit_object_name(object_idx, &mut declaration);
            declaration.push_str(";\n");
            declaration.push_str("typedef struct ");
            emit_object_alloc_name(object_idx, &mut declaration);
            declaration.push_str(" {");
            let mut had_member = false;
            for (member_name, member_type) in types.internal_objects()[object_idx].0.clone() {
                if let Type::Unit = types.group_concrete(member_type) { continue; }
                declaration.push_str("\n    ");
                emit_type(member_type, types, strings, declared_types, type_declarations, &mut declaration);
                declaration.push_str(" member");
                declaration.push_str(&member_name.0.to_string());
                declaration.push_str(";");
                had_member = true;
            }
            if !had_member {
                declaration.push_str("\n    char empty;");
            }
            declaration.push_str("\n} ");
            emit_object_alloc_name(object_idx, &mut declaration);
            declaration.push_str(";\n");
            declaration.push_str("void ");
            emit_object_free_handler_name(object_idx, &mut declaration);
            declaration.push_str("(char* data, size_t size) {");
            declaration.push_str("\n    ");
            emit_object_alloc_name(object_idx, &mut declaration);
            declaration.push_str("* object = (");
            emit_object_alloc_name(object_idx, &mut declaration);
            declaration.push_str("*) data;\n");
            for (member_name, member_type) in types.internal_objects()[object_idx].0.clone() {
                let mut member_rc_decr = String::new();
                emit_rc_decr(
                    &format!("object->member{}", member_name.0.to_string()),
                    member_type, types, strings, &mut member_rc_decr
                );
                indent(&member_rc_decr, &mut declaration);
            }
            declaration.push_str("}\n");
            declaration.push_str("char ");
            emit_object_comparison_function_name(object_idx, &mut declaration);
            declaration.push_str("(");
            emit_object_name(object_idx, &mut declaration);
            declaration.push_str(" a, ");
            emit_object_name(object_idx, &mut declaration);
            declaration.push_str(" b) {");
            for (member_name, member_type) in types.internal_objects()[object_idx].0.clone() {
                declaration.push_str("\n    if(!(");
                emit_equality(
                    &format!("(*a.member{})", member_name.0),
                    &format!("(*b.member{})", member_name.0),
                    member_type, types, &mut declaration
                );
                declaration.push_str(")) { return 0; }");
            }
            declaration.push_str("\n    return 1;\n}\n");
        }
        Type::ConcreteObject(obj) => {
            let concrete_object_idx = obj.get_internal_id();
            declaration.push_str("typedef struct ");
            emit_concrete_object_name(concrete_object_idx, &mut declaration);
            declaration.push_str(" {");
            for (member_name, member_type) in types.internal_concrete_objects()[concrete_object_idx].clone() {
                if let Type::Unit = types.group_concrete(member_type) { continue; }
                declaration.push_str("\n    ");
                emit_type(member_type, types, strings, declared_types, type_declarations, &mut declaration);
                declaration.push_str(" ");
                declaration.push_str(strings.get(member_name));
                declaration.push_str(";");
            }
            declaration.push_str("\n} ");
            emit_concrete_object_name(concrete_object_idx, &mut declaration);
            declaration.push_str(";\n");
            let inc_obj = types.insert_dedup_object((
                types.concrete_object(obj).iter().map(|m| *m).collect(),
                true
            ));
            let object_idx = inc_obj.get_internal_id();
            let inc_t = types.insert_group(&[Type::Object(inc_obj)]);
            emit_type(t, types, strings, declared_types, type_declarations, &mut declaration);
            declaration.push_str(" ");
            emit_object_to_concrete_function_name(concrete_object_idx, &mut declaration);
            declaration.push_str("(");
            emit_type(inc_t, types, strings, declared_types, type_declarations, &mut declaration);
            declaration.push_str(" inconcrete) {\n");
            declaration.push_str("    ");
            emit_type(t, types, strings, declared_types, type_declarations, &mut declaration);
            declaration.push_str(" concrete;\n");
            for (member_name, member_type) in types.concrete_object(obj).iter().map(|m| *m).collect::<Vec<(StringIdx, TypeGroup)>>() {
                declaration.push_str("    concrete.");
                declaration.push_str(strings.get(member_name));
                declaration.push_str(" = ");
                if let Type::ConcreteObject(member_obj) = types.group_concrete(member_type) {
                    emit_object_to_concrete_function_name(member_obj.get_internal_id(), &mut declaration);
                    declaration.push_str("(");
                }
                declaration.push_str("*inconcrete.member");
                declaration.push_str(&member_name.0.to_string());
                if let Type::ConcreteObject(_) = types.group_concrete(member_type) {
                    declaration.push_str(")");
                }
                declaration.push_str(";\n");
            }
            declaration.push_str("    return concrete;\n");
            declaration.push_str("}\n");
            emit_type(inc_t, types, strings, declared_types, type_declarations, &mut declaration);
            declaration.push_str(" ");
            emit_object_to_inconcrete_function_name(concrete_object_idx, &mut declaration);
            declaration.push_str("(");
            emit_type(t, types, strings, declared_types, type_declarations, &mut declaration);
            declaration.push_str(" concrete) {\n");
            declaration.push_str("    GeraAllocation* allocation = gera___rc_alloc(sizeof(");
            emit_object_alloc_name(object_idx, &mut declaration);
            declaration.push_str("), &");
            emit_object_free_handler_name(object_idx, &mut declaration);
            declaration.push_str(");\n");
            declaration.push_str("    ");
            emit_object_alloc_name(object_idx, &mut declaration);
            declaration.push_str("* object = (");
            emit_object_alloc_name(object_idx, &mut declaration);
            declaration.push_str("*) allocation->data;\n");
            declaration.push_str("    ");
            emit_type(inc_t, types, strings, declared_types, type_declarations, &mut declaration);
            declaration.push_str(" inconcrete;\n");
            declaration.push_str("    inconcrete.allocation = allocation;\n");
            for (member_name, member_type) in types.concrete_object(obj).iter().map(|m| *m).collect::<Vec<(StringIdx, TypeGroup)>>() {
                declaration.push_str("    object->member");
                declaration.push_str(&member_name.0.to_string());
                declaration.push_str(" = ");
                if let Type::ConcreteObject(member_obj) = types.group_concrete(member_type) {
                    emit_object_to_inconcrete_function_name(member_obj.get_internal_id(), &mut declaration);
                    declaration.push_str("(");
                }
                declaration.push_str("concrete.");
                declaration.push_str(strings.get(member_name));
                declaration.push_str(";\n");
                declaration.push_str("    inconcrete.member");
                declaration.push_str(&member_name.0.to_string());
                declaration.push_str(" = &object->member");
                declaration.push_str(&member_name.0.to_string());
                declaration.push_str(";\n");
            }
            declaration.push_str("    return inconcrete;\n");
            declaration.push_str("}\n");
        }
        Type::Closure(clo) => {
            let closure_idx = clo.get_internal_id();
            declaration.push_str("typedef struct ");
            emit_closure_name(closure_idx, &mut declaration);
            declaration.push_str(" {\n");
            declaration.push_str("    GeraAllocation* allocation;\n");
            let (parameter_types, return_type) = types.internal_closures()[closure_idx].clone();
            declaration.push_str("    ");
            emit_type(return_type, types, strings, declared_types, type_declarations, &mut declaration);
            declaration.push_str(" (*procedure)(GeraAllocation*");
            for parameter_type in parameter_types {
                if let Type::Unit = types.group_concrete(parameter_type) { continue; }
                declaration.push_str(", ");
                emit_type(parameter_type, types, strings, declared_types, type_declarations, &mut declaration);
            }
            declaration.push_str(");\n");
            declaration.push_str("} ");
            emit_closure_name(closure_idx, &mut declaration);
            declaration.push_str(";\n");
        }
        Type::Variants(var) => {
            let variants_idx = var.get_internal_id();
            let has_values = types.internal_variants()[variants_idx].0.clone().into_iter().filter(|(_, vt)|
                    if let Type::Unit = types.group_concrete(*vt) { false } else { true }
                ).next().is_some();
            if has_values {
                declaration.push_str("typedef union ");
                emit_variants_name(variants_idx, &mut declaration);
                declaration.push_str("Value {");
                for (variant_name, variant_type) in types.internal_variants()[variants_idx].0.clone() {
                    if let Type::Unit = types.group_concrete(variant_type) { continue; }
                    declaration.push_str("\n    ");
                    emit_type(variant_type, types, strings, declared_types, type_declarations, &mut declaration);
                    declaration.push_str(" ");
                    declaration.push_str(strings.get(variant_name));
                    declaration.push_str(";");
                }
                declaration.push_str("\n} ");
                emit_variants_name(variants_idx, &mut declaration);
                declaration.push_str("Value;\n");
            }
            declaration.push_str("typedef struct ");
            emit_variants_name(variants_idx, &mut declaration);
            declaration.push_str(" {\n    unsigned long long int tag;");
            if has_values {
                declaration.push_str("\n    ");
                emit_variants_name(variants_idx, &mut declaration);
                declaration.push_str("Value value;");
            }
            declaration.push_str("\n} ");
            emit_variants_name(variants_idx, &mut declaration);
            declaration.push_str(";\n");
            declaration.push_str("char ");
            emit_variant_comparison_function_name(variants_idx, &mut declaration);
            declaration.push_str("(");
            emit_variants_name(variants_idx, &mut declaration);
            declaration.push_str(" a, ");
            emit_variants_name(variants_idx, &mut declaration);
            declaration.push_str(" b) {\n");
            declaration.push_str("    if(a.tag != b.tag) { return 0; }\n");
            declaration.push_str("    switch(a.tag) {\n");
            for(variant_name, variant_type) in types.internal_variants()[variants_idx].0.clone() {
                if let Type::Unit = types.group_concrete(variant_type) { continue; }
                declaration.push_str("        case ");
                declaration.push_str(&variant_name.0.to_string());
                declaration.push_str(":\n");
                declaration.push_str("            if(!(");
                emit_equality(
                    &format!("(a.value.{})", strings.get(variant_name)),
                    &format!("(b.value.{})", strings.get(variant_name)),
                    variant_type, types, &mut declaration
                );
                declaration.push_str(")) { return 0; }\n");
                declaration.push_str("            break;\n");
            }
            declaration.push_str("    }\n");
            declaration.push_str("    return 1;\n");
            declaration.push_str("}\n")
        }
    }
    type_declarations.push_str(&declaration);
    declared_types.insert(type_iden, true);
}

fn emit_rc_incr(
    variable: &str,
    incr_type: TypeGroup,
    types: &mut TypeMap,
    strings: &StringMap,
    output: &mut String
) {
    match types.group_concrete(incr_type) {
        Type::Any |
        Type::Unit |
        Type::Boolean |
        Type::Integer |
        Type::Float => {}
        Type::String |
        Type::Array(_) |
        Type::Object(_) => {
            output.push_str("gera___rc_incr(");
            output.push_str(variable);
            output.push_str(".allocation);\n");
        }
        Type::ConcreteObject(_) => {}
        Type::Variants(variant_idx) => {
            let mut switch = String::new();
            let mut had_effect = false;
            switch.push_str("switch(");
            switch.push_str(variable);
            switch.push_str(".tag) {");
            for (variant_name, variant_type) in types.variants(variant_idx).0.clone() {
                if let Type::Unit = types.group_concrete(variant_type) { continue; }
                switch.push_str("\n    case ");
                switch.push_str(&variant_name.0.to_string());
                switch.push_str(":\n");
                let mut var_decr = String::new();
                emit_rc_incr(
                    &format!("{}.value.{}", variable, strings.get(variant_name)),
                    variant_type, types, strings, &mut var_decr
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
        Type::Closure(_) => {
            output.push_str("gera___rc_incr(");
            output.push_str(variable);
            output.push_str(".allocation);\n");
        }
    }
}

fn emit_rc_decr(
    variable: &str,
    freed_type: TypeGroup,
    types: &mut TypeMap, 
    strings: &StringMap,
    output: &mut String
) {
    match types.group_concrete(freed_type) {
        Type::Any |
        Type::Unit |
        Type::Boolean |
        Type::Integer |
        Type::Float => {}
        Type::String |
        Type::Array(_) |
        Type::Object(_) => {
            output.push_str("gera___rc_decr(");
            output.push_str(variable);
            output.push_str(".allocation);\n");
        }
        Type::ConcreteObject(_) => {}
        Type::Variants(variant_idx) => {
            output.push_str("switch(");
            output.push_str(variable);
            output.push_str(".tag) {");
            for (variant_name, variant_type) in types.variants(variant_idx).0.clone() {
                if let Type::Unit = types.group_concrete(variant_type) { continue; }
                output.push_str("\n    case ");
                output.push_str(&variant_name.0.to_string());
                output.push_str(":\n");
                let mut var_decr = String::new();
                emit_rc_decr(
                    &format!("{}.value.{}", variable, strings.get(variant_name)),
                    variant_type, types, strings, &mut var_decr
                );
                let mut var_decr_indented = String::new();
                indent(&var_decr, &mut var_decr_indented);
                indent(&var_decr_indented, output);
                output.push_str("        break;");
            }
            output.push_str("\n}\n");
        }
        Type::Closure(_) => {
            output.push_str("gera___rc_decr(");
            output.push_str(variable);
            output.push_str(".allocation);\n");
        }
    }
}

fn emit_equality(
    a: &str,
    b: &str,
    compared_types: TypeGroup,
    types: &mut TypeMap,
    output: &mut String
) {
    match types.group_concrete(compared_types) {
        Type::Any |
        Type::Unit => {
            output.push_str("1");
        }
        Type::String => {
            output.push_str("gera___string_eq(");
            output.push_str(a);
            output.push_str(", ");
            output.push_str(b);
            output.push_str(")");
        }
        Type::Array(arr) => {
            emit_array_comparison_function_name(arr.get_internal_id(), output);
            output.push_str("(");
            output.push_str(a);
            output.push_str(", ");
            output.push_str(b);
            output.push_str(")");
        }
        Type::Object(obj) => {
            emit_object_comparison_function_name(obj.get_internal_id(), output);
            output.push_str("(");
            output.push_str(a);
            output.push_str(", ");
            output.push_str(b);
            output.push_str(")");
        }
        Type::Variants(var) => {
            emit_variant_comparison_function_name(var.get_internal_id(), output);
            output.push_str("(");
            output.push_str(a);
            output.push_str(", ");
            output.push_str(b);
            output.push_str(")");
        }
        Type::ConcreteObject(_) => panic!("We should never have to compare concrete objects!"),
        Type::Closure(_) => {
            output.push_str(a);
            output.push_str(".allocation == ");
            output.push_str(b);
            output.push_str(".allocation");
        }
        _ => {
            output.push_str(a);
            output.push_str(" == ");
            output.push_str(b);
        }
    }
}

fn emit_symbol_declarations(
    symbols: &Vec<IrSymbol>,
    types: &mut TypeMap,
    static_var_vals: &mut HashMap<NamespacePath, (ConstantValue, TypeGroup)>,
    strings: &StringMap,
    declared_types: &mut HashMap<String, bool>, type_declarations: &mut String,
    external: &mut HashMap<NamespacePath, StringIdx>,
    output: &mut String
) {
    for symbol in symbols {
        match symbol {
            IrSymbol::Procedure { path, variant, parameter_types, return_type, .. } => {
                emit_type(*return_type, types, strings, declared_types, type_declarations, output);
                output.push_str(" ");
                emit_procedure_name(path, *variant, strings, output);
                output.push_str("(");
                let mut had_param = false;
                for p in 0..parameter_types.len() {
                    let param_type = parameter_types[p];
                    if let Type::Unit = types.group_concrete(param_type) { continue; }
                    if had_param { output.push_str(", "); }
                    had_param = true;
                    emit_type(param_type, types, strings, declared_types, type_declarations, output);
                    output.push_str(" param");
                    output.push_str(&p.to_string());
                }
                output.push_str(");\n");
            }
            IrSymbol::ExternalProcedure { path, backing, parameter_types, return_type } => {
                external.insert(path.clone(), *backing);
                output.push_str("extern ");
                emit_type(*return_type, types, strings, declared_types, type_declarations, output);
                output.push_str(" ");
                output.push_str(strings.get(*backing));
                output.push_str("(");
                for p in 0..parameter_types.len() {
                    let param_type = parameter_types[p];
                    if p > 0 { output.push_str(", "); }
                    emit_type(param_type, types, strings, declared_types, type_declarations, output);
                    output.push_str(" param");
                    output.push_str(&p.to_string());
                }
                output.push_str(");\n");
                if let Type::ConcreteObject(obj) = types.group_concrete(*return_type) {
                    let inc_obj = types.insert_dedup_object((
                        types.concrete_object(obj).iter().map(|m| *m).collect(),
                        true
                    ));
                    let inc_return_type = types.insert_group(&[Type::Object(inc_obj)]);
                    emit_type(inc_return_type, types, strings, declared_types, type_declarations, output);
                } else {
                    emit_type(*return_type, types, strings, declared_types, type_declarations, output);
                }
                output.push_str(" ");
                emit_external_function_wrapper(strings.get(*backing), output);
                output.push_str("(");
                for p in 0..parameter_types.len() {
                    let param_type = parameter_types[p];
                    if p > 0 { output.push_str(", "); }
                    if let Type::ConcreteObject(obj) = types.group_concrete(param_type) {
                        let inc_obj = types.insert_dedup_object((
                            types.concrete_object(obj).iter().map(|m| *m).collect(),
                            true
                        ));
                        let inc_param_type = types.insert_group(&[Type::Object(inc_obj)]);
                        emit_type(inc_param_type, types, strings, declared_types, type_declarations, output);
                    } else {
                        emit_type(param_type, types, strings, declared_types, type_declarations, output);
                    }
                    output.push_str(" param");
                    output.push_str(&p.to_string());
                }
                output.push_str(");\n");
            }
            IrSymbol::BuiltInProcedure { path, variant, parameter_types, return_type } => {
                emit_type(*return_type, types, strings, declared_types, type_declarations, output);
                output.push_str(" ");
                emit_procedure_name(path, *variant, strings, output);
                output.push_str("(");
                let mut had_param = false;
                for p in 0..parameter_types.len() {
                    let param_type = parameter_types[p];
                    if let Type::Unit = types.group_concrete(param_type) { continue; }
                    if had_param { output.push_str(", "); }
                    had_param = true;
                    emit_type(param_type, types, strings, declared_types, type_declarations, output);
                    output.push_str(" param");
                    output.push_str(&p.to_string());
                }
                output.push_str(");\n");
            }
            IrSymbol::Variable { path, value_type, value } => {
                if let Type::Unit = types.group_concrete(*value_type) { continue; }
                emit_type(*value_type, types, strings, declared_types, type_declarations, output);
                output.push_str(" ");
                emit_path(path, strings, output);
                output.push_str(";\n");
                static_var_vals.insert(path.clone(), (*value, *value_type));
            }
            IrSymbol::ExternalVariable { path, backing, value_type } => {
                if let Type::Unit = types.group_concrete(*value_type) { continue; }
                external.insert(path.clone(), *backing);
                output.push_str("extern ");
                emit_type(*value_type, types, strings, declared_types, type_declarations, output);  
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

fn emit_scope_decrements(
    free: &HashSet<usize>,
    variable_types: &Vec<TypeGroup>,
    types: &mut TypeMap,
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

fn get_builtin_bodies(strings: &mut StringMap) -> HashMap<NamespacePath, fn(&Vec<TypeGroup>, TypeGroup, &mut TypeMap, &mut StringMap, &mut HashMap<String, bool>, &mut String) -> String> {
    fn path_from(segments: &[&'static str], strings: &mut StringMap) -> NamespacePath {
        NamespacePath::new(segments.iter().map(|s| strings.insert(s)).collect())
    }
    let mut builtins: HashMap<NamespacePath, fn(&Vec<TypeGroup>, TypeGroup, &mut TypeMap, &mut StringMap, &mut HashMap<String, bool>, &mut String) -> String> = HashMap::new();
    builtins.insert(path_from(&["core", "addr_eq"], strings), |_, _, _, _, _, _| {
        String::from(r#"
return param0.allocation == param1.allocation;
"#)
    });
    builtins.insert(path_from(&["core", "tag_eq"], strings), |_, _, _, _, _, _| {
        String::from(r#"
return param0.tag == param1.tag;
"#)
    });
    builtins.insert(path_from(&["core", "length"], strings), |_, _, _, _, _, _| {
        String::from(r#"
return param0.length;
"#)
    });
    builtins.insert(path_from(&["core", "array"], strings), |param_types, return_type, types, strings, declared_types, type_declarations| {
        let array_idx = if let Type::Array(array_idx) = types.group_concrete(return_type) {
            array_idx.get_internal_id()
        } else { panic!("should be an array!"); };
        let mut result = String::new();
        result.push_str(r#"
if(param1 < 0) {
    size_t length_str_len = geracoredeps_display_sint_length(param1);
    char length_str[length_str_len + 1];
    geracoredeps_display_sint(param1, length_str);
    length_str[length_str_len] = '\0';
    gera___panic_pre();
    geracoredeps_eprint("the array length ");
    geracoredeps_eprint(length_str);
    geracoredeps_eprint(" is not valid");
    gera___panic_post();
}"#);
        result.push_str("GeraArray result;\n");
        result.push_str("GeraAllocation* allocation = gera___rc_alloc(");
        if let Type::Unit = types.group_concrete(param_types[0]) {
            result.push_str("1, &gera___free_nothing");
        } else {
            result.push_str("param1 == 0? 1 : sizeof(");
            emit_type(param_types[0], types, strings, declared_types, type_declarations, &mut result);
            result.push_str(") * param1, &");
            emit_array_free_handler_name(array_idx, &mut result);
        }
        result.push_str(");\n");
        result.push_str("result.allocation = allocation;\n");
        result.push_str("result.data = allocation->data;\n");
        result.push_str("result.length = param1;\n");
        if let Type::Unit = types.group_concrete(param_types[0]) {} else {
            emit_type(param_types[0], types, strings, declared_types, type_declarations, &mut result);
            result.push_str("* elements = (");
            emit_type(param_types[0], types, strings, declared_types, type_declarations, &mut result);
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
    builtins.insert(path_from(&["core", "exhaust"], strings), |_, _, _, strings, _, _| {
        format!("
while(((param0.procedure)(param0.allocation)).tag == {}) {{}}
", strings.insert("next").0)
    });
    builtins.insert(path_from(&["core", "panic"], strings), |_, _, _, _, _, _| {
        String::from(r#"
GERA_STRING_NULL_TERM(param0, message_nt);
gera___panic(message_nt);
"#)
    });
    builtins.insert(path_from(&["core", "as_str"], strings), |param_types, _, types, strings, _, _| {
        match types.group_concrete(param_types[0]) {
            Type::Unit | Type::Any => String::from(r#"
return gera___wrap_static_string("<unit>");
"#),
            Type::Boolean => String::from(r#"
return gera___wrap_static_string(param0? "true" : "false");
"#),
            Type::Integer => String::from(r#"
size_t result_length = geracoredeps_display_sint_length(param0);
char result[result_length + 1];
geracoredeps_display_sint(param0, result);
result[result_length] = '\0';
return gera___alloc_string(result);
"#),
            Type::Float => String::from(r#"
size_t result_length = geracoredeps_display_float_length(param0);
char result[result_length + 1];
geracoredeps_display_float(param0, result);
result[result_length] = '\0';
return gera___alloc_string(result);
"#),
            Type::String => {
                let mut result = String::new();
                emit_rc_incr("param0", param_types[0], types, strings, &mut result);
                result.push_str("return param0;\n");
                result
            }
            Type::Array(_) => String::from(r#"
size_t result_length = geracoredeps_display_pointer_length(param0.allocation) + 8;
char result[result_length + 1];
result[0] = '<'; result[1] = 'a'; result[2] = 'r';
result[3] = 'r'; result[4] = 'a'; result[5] = 'y';
result[6] = ' ';
geracoredeps_display_pointer(param0.allocation, result + 7);
result[result_length - 1] = '>';
result[result_length] = '\0';
return gera___alloc_string(result);
"#),
            Type::Object(_) => String::from(r#"
size_t result_length = geracoredeps_display_pointer_length(param0.allocation) + 9;
char result[result_length + 1];
result[0] = '<'; result[1] = 'o'; result[2] = 'b';
result[3] = 'j'; result[4] = 'e'; result[5] = 'c';
result[6] = 't'; result[7] = ' ';
geracoredeps_display_pointer(param0.allocation, result + 8);
result[result_length - 1] = '>';
result[result_length] = '\0';
return gera___alloc_string(result);
"#),
            Type::ConcreteObject(_) => String::from(r#"
return gera___wrap_static_string("<object>");
"#),
            Type::Variants(variant_idx) => {
                let variant_types = &types.variants(variant_idx).0;
                let mut result = String::from("switch(param0.tag) {\n");
                for (variant_name, _) in variant_types {
                    result.push_str("    case ");
                    result.push_str(&variant_name.0.to_string());
                    let variant_str = format!("#{} <...>", strings.get(*variant_name));
                    result.push_str(": return gera___wrap_static_string(\"");
                    result.push_str(&variant_str);
                    result.push_str("\");\n");
                }
                result.push_str("}\n");
                result
            }
            Type::Closure(_) => String::from(r#"
size_t result_length = geracoredeps_display_pointer_length(param0.allocation) + 10;
char result[result_length + 1];
result[0] = '<'; result[1] = 'c'; result[2] = 'l';
result[3] = 'o'; result[4] = 's'; result[5] = 'u';
result[6] = 'r'; result[7] = 'e'; result[8] = ' ';
geracoredeps_display_pointer(param0.allocation, result + 9);
result[result_length - 1] = '>';
result[result_length] = '\0';
return gera___alloc_string(result);
"#)
        }
    });
    builtins.insert(path_from(&["core", "as_int"], strings), |_, _, _, _, _, _| {
        String::from(r#"
return (gint) param0;
"#)
    });
    builtins.insert(path_from(&["core", "as_flt"], strings), |_, _, _, _, _, _| {
        String::from(r#"
return (gfloat) param0;
"#)
    });
    builtins.insert(path_from(&["core", "substring"], strings), |_, _, _, _, _, _| {
        String::from(r#"
size_t start_idx = param1;
if(param1 < 0) { start_idx = param0.length + param1; }
if(start_idx > param0.length) { 
    size_t start_str_len = geracoredeps_display_sint_length(param1);
    char start_str[start_str_len + 1];
    geracoredeps_display_sint(param1, start_str);
    start_str[start_str_len] = '\0';
    size_t length_str_len = geracoredeps_display_uint_length(param0.length);
    char length_str[length_str_len + 1];
    geracoredeps_display_uint(param0.length, length_str);
    length_str[length_str_len] = '\0';
    gera___panic_pre();
    geracoredeps_eprint("the start index ");
    geracoredeps_eprint(start_str);
    geracoredeps_eprint(" is out of bounds for a string of length ");
    geracoredeps_eprint(length_str);
    gera___panic_post();
}
size_t end_idx = param2;
if(param2 < 0) { end_idx = param0.length + param2; }
if(end_idx > param0.length) {
    size_t end_str_len = geracoredeps_display_sint_length(param2);
    char end_str[end_str_len + 1];
    geracoredeps_display_sint(param2, end_str);
    end_str[end_str_len] = '\0';
    size_t length_str_len = geracoredeps_display_uint_length(param0.length);
    char length_str[length_str_len + 1];
    geracoredeps_display_uint(param0.length, length_str);
    length_str[length_str_len] = '\0';
    gera___panic_pre();
    geracoredeps_eprint("the end index ");
    geracoredeps_eprint(end_str);
    geracoredeps_eprint(" is out of bounds for a string of length ");
    geracoredeps_eprint(length_str);
    gera___panic_post();
}
if(start_idx > end_idx) {
    size_t start_str_len = geracoredeps_display_sint_length(param1);
    char start_str[start_str_len + 1];
    geracoredeps_display_sint(param1, start_str);
    start_str[start_str_len] = '\0';
    size_t end_str_len = geracoredeps_display_sint_length(param2);
    char end_str[end_str_len + 1];
    geracoredeps_display_sint(param2, end_str);
    end_str[end_str_len] = '\0';
    size_t length_str_len = geracoredeps_display_uint_length(param0.length);
    char length_str[length_str_len + 1];
    geracoredeps_display_uint(param0.length, length_str);
    length_str[length_str_len] = '\0';
    gera___panic_pre();
    geracoredeps_eprint("the start index ");
    geracoredeps_eprint(start_str);
    geracoredeps_eprint(" is larger than the end index ");
    geracoredeps_eprint(end_str);
    geracoredeps_eprint(" (length of string is ");
    geracoredeps_eprint(length_str);
    geracoredeps_eprint(")");
    gera___panic_post();
}
return gera___substring(param0, start_idx, end_idx);
"#)
    });
    builtins.insert(path_from(&["core", "concat"], strings), |_, _, _, _, _, _| {
        String::from(r#"
return gera___concat(param0, param1);
"#)
    });
    builtins.insert(path_from(&["core", "parse_flt"], strings), |_, return_type, types, strings, _, _| {
        let variant_idx = if let Type::Variants(v) = types.group_concrete(return_type) { v }
        else { panic!("should be variants"); };
        let mut result = String::new();
        result.push_str("GERA_STRING_NULL_TERM(param0, param0_nt);\n");
        result.push_str("gfloat value = geracoredeps_parse_float(param0_nt);\n");
        result.push_str("if(geracoredeps_parse_success) { return (");
        emit_variants_name(variant_idx.get_internal_id(), &mut result);
        result.push_str(") { .tag = ");
        result.push_str(&strings.insert("some").0.to_string());
        result.push_str(", .value = { .some = value } }; }\n");
        result.push_str("return (");
        emit_variants_name(variant_idx.get_internal_id(), &mut result);
        result.push_str(") { .tag = ");
        result.push_str(&strings.insert("none").0.to_string());
        result.push_str(" };\n");
        result
    });
    builtins.insert(path_from(&["core", "parse_int"], strings), |_, return_type, types, strings, _, _| {
        let variant_idx = if let Type::Variants(v) = types.group_concrete(return_type) { v }
        else { panic!("should be variants"); };
        let mut result = String::new();
        result.push_str("GERA_STRING_NULL_TERM(param0, param0_nt);\n");
        result.push_str("gint value = geracoredeps_parse_sint(param0_nt);\n");
        result.push_str("if(geracoredeps_parse_success) { return (");
        emit_variants_name(variant_idx.get_internal_id(), &mut result);
        result.push_str(") { .tag = ");
        result.push_str(&strings.insert("some").0.to_string());
        result.push_str(", .value = { .some = value } }; }\n");
        result.push_str("return (");
        emit_variants_name(variant_idx.get_internal_id(), &mut result);
        result.push_str(") { .tag = ");
        result.push_str(&strings.insert("none").0.to_string());
        result.push_str(" };\n");
        result
    });
    builtins.insert(path_from(&["core", "string"], strings), |_, _, _, _, _, _| {
        String::from(r#"
if(param1 < 0) {
    size_t count_str_len = geracoredeps_display_sint_length(param1);
    char count_str[count_str_len + 1];
    geracoredeps_display_sint(param1, count_str);
    count_str[count_str_len] = '\0';
    gera___panic_pre();
    geracoredeps_eprint("the string repetition count ");
    geracoredeps_eprint(count_str);
    geracoredeps_eprint(" is not valid");
    gera___panic_post();
}
GeraAllocation* allocation = gera___rc_alloc(param1 == 0? 1 : param0.length_bytes * param1, &gera___free_nothing);
GeraString result;
result.allocation = allocation;
result.data = allocation->data;
result.length = param0.length * param1;
result.length_bytes = param0.length_bytes * param1;
for(size_t i = 0; i < result.length_bytes; i += 1) {
    allocation->data[i] = param0.data[i % param0.length_bytes];
}
return result;
"#)
    });
    builtins.insert(path_from(&["core", "hash"], strings), |param_types, _, types, strings, declared_types, type_declarations| {
        match types.group_concrete(param_types[0]) {
            Type::Unit | Type::Any => String::from(r#"
return 0;
"#),
            Type::Boolean |
            Type::Integer |
            Type::Float |
            Type::Array(_) |
            Type::Object(_) |
            Type::ConcreteObject(_) |
            Type::Variants(_) |
            Type::Closure(_) => {
                let mut result = String::new();
                result.push_str("return gera___hash((unsigned char*) &param0, sizeof(");
                emit_type(param_types[0], types, strings, declared_types, type_declarations, &mut result);
                result.push_str("));\n");
                result
            }
            Type::String => String::from(r#"
return gera___hash((unsigned char*) param0.data, param0.length_bytes);
"#)
        }
    });
    return builtins;
}

fn emit_variable_default_value(
    variable_type: TypeGroup,
    types: &mut TypeMap,
    strings: &StringMap,
    declared_types: &mut HashMap<String, bool>, type_declarations: &mut String,
    output: &mut String
) {
    match types.group_concrete(variable_type) {
        Type::Any | Type::Unit => panic!("should not be unit"),
        Type::Boolean => output.push_str("0"),
        Type::Integer => output.push_str("0"),
        Type::Float => output.push_str("0.0"),
        Type::String | Type::Array(_) | Type::Object(_) | Type::Closure(_) => {
            output.push_str("(");
            emit_type(variable_type, types, strings, declared_types, type_declarations, output);
            output.push_str(") { .allocation = NULL }");
        }
        Type::ConcreteObject(_) => {
            output.push_str("(");
            emit_type(variable_type, types, strings, declared_types, type_declarations, output);
            output.push_str(") { 0 }");
        }
        Type::Variants(_) => {
            output.push_str("(");
            emit_type(variable_type, types, strings, declared_types, type_declarations, output);
            output.push_str(") { .tag = 0xFFFFFFFF }");
        }
    }
}

fn emit_external_function_wrapper(
    backing: &str, output: &mut String
) {
    output.push_str("geraextwrapper_");
    output.push_str(backing);
}

fn emit_procedure_impls(
    symbols: &Vec<IrSymbol>,
    types: &mut TypeMap,
    constants: &mut ConstantPool,
    strings: &mut StringMap,
    declared_types: &mut HashMap<String, bool>, type_declarations: &mut String,
    closure_bodies: &mut Vec<String>,
    external: &mut HashMap<NamespacePath, StringIdx>,
    output: &mut String
) {
    let builtin_bodies = get_builtin_bodies(strings);
    for symbol in symbols {
        match symbol {
            IrSymbol::Procedure { path, variant, parameter_types, return_type, variables, body } => {
                emit_type(*return_type, types, strings, declared_types, type_declarations, output);
                output.push_str(" ");
                emit_procedure_name(path, *variant, strings, output);
                output.push_str("(");
                let mut had_param = false;
                for p in 0..parameter_types.len() {
                    let param_type = parameter_types[p];
                    if let Type::Unit = types.group_concrete(param_type) { continue; }
                    if had_param { output.push_str(", "); }
                    had_param = true;
                    emit_type(param_type, types, strings, declared_types, type_declarations, output);
                    output.push_str(" param");
                    output.push_str(&p.to_string());
                }
                output.push_str(") {\n");
                for variable_idx in 0..variables.len() {
                    let var_type = variables[variable_idx];
                    if let Type::Unit = types.group_concrete(var_type) { continue; }
                    output.push_str("    ");
                    emit_type(var_type, types, strings, declared_types, type_declarations, output);
                    output.push_str(" ");
                    emit_variable(IrVariable { index: variable_idx, version: 0 }, output);
                    output.push_str(" = ");
                    emit_variable_default_value(var_type, types, strings, declared_types, type_declarations, output);
                    output.push_str(";\n");
                }
                if let Type::Unit = types.group_concrete(*return_type) {} else {
                    output.push_str("    ");
                    emit_type(*return_type, types, strings, declared_types, type_declarations, output);
                    output.push_str(" returned;\n");
                }
                let mut body_str = String::new();
                let mut body_free = HashSet::new();
                emit_block(
                    body, variables, *return_type, &mut body_free, &mut HashMap::new(), 
                    closure_bodies, types,
                    constants, external, symbols, strings, declared_types, type_declarations,
                    &mut body_str
                );
                body_str.push_str("\nret:\n");
                emit_scope_decrements(&body_free, variables, types, strings, &mut body_str);
                if let Type::Unit = types.group_concrete(*return_type) {
                    body_str.push_str("return;\n");
                } else {
                    body_str.push_str("return returned;\n");
                }
                indent(&body_str, output);
                output.push_str("}\n");
            }
            IrSymbol::BuiltInProcedure { path, variant, parameter_types, return_type } => {
                emit_type(*return_type, types, strings, declared_types, type_declarations, output);
                output.push_str(" ");
                emit_procedure_name(path, *variant, strings, output);
                output.push_str("(");
                let mut had_param = false;
                let mut param_types = Vec::new();
                for p in 0..parameter_types.len() {
                    let param_type = parameter_types[p];
                    param_types.push(param_type);
                    if let Type::Unit = types.group_concrete(param_type) { continue; }
                    if had_param { output.push_str(", "); }
                    had_param = true;
                    emit_type(param_type, types, strings, declared_types, type_declarations, output);
                    output.push_str(" param");
                    output.push_str(&p.to_string());
                }
                output.push_str(") {\n");
                let body_str = (builtin_bodies
                    .get(path)
                    .expect("builtin should have implementation"))
                    (&param_types, *return_type, types, strings, declared_types, type_declarations);
                indent(&body_str, output);
                output.push_str("}\n");
            }
            IrSymbol::ExternalProcedure { path: _, backing, parameter_types, return_type } => {
                if let Type::ConcreteObject(obj) = types.group_concrete(*return_type) {
                    let inc_obj = types.insert_dedup_object((
                        types.concrete_object(obj).iter().map(|m| *m).collect(),
                        true
                    ));
                    let inc_return_type = types.insert_group(&[Type::Object(inc_obj)]);
                    emit_type(inc_return_type, types, strings, declared_types, type_declarations, output);
                } else {
                    emit_type(*return_type, types, strings, declared_types, type_declarations, output);
                }
                output.push_str(" ");
                emit_external_function_wrapper(strings.get(*backing), output);
                output.push_str("(");
                for p in 0..parameter_types.len() {
                    let param_type = parameter_types[p];
                    if p > 0 { output.push_str(", "); }
                    if let Type::ConcreteObject(obj) = types.group_concrete(param_type) {
                        let inc_obj = types.insert_dedup_object((
                            types.concrete_object(obj).iter().map(|m| *m).collect(),
                            true
                        ));
                        let inc_param_type = types.insert_group(&[Type::Object(inc_obj)]);
                        emit_type(inc_param_type, types, strings, declared_types, type_declarations, output);
                    } else {
                        emit_type(param_type, types, strings, declared_types, type_declarations, output);
                    }
                    output.push_str(" param");
                    output.push_str(&p.to_string());
                }
                output.push_str(") {\n");
                output.push_str("    return ");
                if let Type::ConcreteObject(obj) = types.group_concrete(*return_type) {
                    emit_object_to_inconcrete_function_name(obj.get_internal_id(), output);
                    output.push_str("(");
                }
                output.push_str(strings.get(*backing));
                output.push_str("(");
                for p in 0..parameter_types.len() {
                    let param_type = parameter_types[p];
                    if p > 0 { output.push_str(", "); }
                    if let Type::ConcreteObject(obj) = types.group_concrete(param_type) {
                        emit_object_to_concrete_function_name(obj.get_internal_id(), output);
                        output.push_str("(");
                    }
                    output.push_str("param");
                    output.push_str(&p.to_string());
                    if let Type::ConcreteObject(_) = types.group_concrete(param_type) {
                        output.push_str(")");
                    }
                }
                output.push_str(")");
                if let Type::ConcreteObject(_) = types.group_concrete(*return_type) {
                    output.push_str(")");
                }
                output.push_str(";\n");
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
    constants: &ConstantPool, types: &mut TypeMap,
    strings: &StringMap, declared_types: &mut HashMap<String, bool>, type_declarations: &mut String,
    output: &mut String
) {
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
                emit_string_literal(&v, output);
                output.push_str("\n");
                output.push_str("};\n");
            }
            ConstantPoolValue::Array(values, element_type) => {
                if let Type::Unit = types.group_concrete(element_type) {
                    output.push_str("char");
                } else {
                    emit_type(element_type, types, strings, declared_types, type_declarations, output);
                }
                output.push_str(" ");
                emit_constant_name(vi, output);
                output.push_str("values[");
                if let Type::Unit = types.group_concrete(element_type) {
                    output.push_str("1");
                } else {
                    output.push_str(&values.len().to_string());
                }
                output.push_str("];\n");
                let constant_tidx = types.insert_dedup_array(element_type);
                let constant_type = types.insert_group(&[Type::Array(constant_tidx)]);
                emit_type(constant_type, types, strings, declared_types, type_declarations, output);
                output.push_str(" ");
                emit_constant_name(vi, output);
                output.push_str(";\n");
            }
            ConstantPoolValue::Object(members) => {
                let constant_object_idx = types.insert_dedup_object((
                    members.iter().map(|(mn, (_, mt))| (*mn, *mt)).collect(),
                    true
                ));
                let constant_type = types.insert_group(&[Type::Object(constant_object_idx)]);
                emit_object_alloc_name(constant_object_idx.get_internal_id(), output);
                output.push_str(" ");
                emit_constant_name(vi, output);
                output.push_str("values;\n");
                emit_type(constant_type, types, strings, declared_types, type_declarations, output);
                output.push_str(" ");
                emit_constant_name(vi, output);
                output.push_str(";\n");
            }
            ConstantPoolValue::Closure(constant_closure) => {
                let constant_closure_idx = types.insert_dedup_closure((
                    constant_closure.parameter_types.clone(),
                    constant_closure.return_type
                ));
                let constant_type = types.insert_group(&[Type::Closure(constant_closure_idx)]);
                emit_type(constant_type, types, strings, declared_types, type_declarations, output);
                output.push_str(" ");
                emit_constant_name(vi, output);
                output.push_str(";\n");
            }
            ConstantPoolValue::Variant(_, _, _) => {}
        }
    }
}

fn emit_constant_initializers(
    constants: &mut ConstantPool,
    static_var_vals: &HashMap<NamespacePath, (ConstantValue, TypeGroup)>,
    external: &HashMap<NamespacePath, StringIdx>, symbols: &Vec<IrSymbol>,
    closure_bodies: &mut Vec<String>, types: &mut TypeMap, strings: &StringMap,
    declared_types: &mut HashMap<String, bool>, type_declarations: &mut String,
    output: &mut String
) {
    output.push_str("void gera_init_constants(void) {\n");
    let mut cinit = String::new();
    let mut vi = 0;
    while vi < constants.get_value_count() {
        match constants.get_value(vi) {
            ConstantPoolValue::String(_) => {}
            ConstantPoolValue::Array(values, element_type) => {
                if let Type::Unit = types.group_concrete(element_type) {} else {
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
                let constant_tidx = types.insert_dedup_array(element_type);
                let constant_type = types.insert_group(&[Type::Array(constant_tidx)]);
                emit_constant_name(vi, &mut cinit);
                cinit.push_str(" = (");
                emit_type(constant_type, types, strings, declared_types, type_declarations, &mut cinit);
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
                let constant_object_idx = types.insert_dedup_object((
                    members.iter().map(|(mn, (_, mt))| (*mn, *mt)).collect(),
                    true
                ));
                emit_constant_name(vi, &mut cinit);
                cinit.push_str("values = (");
                emit_object_alloc_name(constant_object_idx.get_internal_id(), &mut cinit);
                cinit.push_str(") {\n");
                for (member_name, (member_value, member_type)) in &members {
                    cinit.push_str("    .member");
                    cinit.push_str(&member_name.0.to_string());
                    cinit.push_str(" = ");
                    emit_value(*member_value, *member_type, types, constants, strings, &mut cinit);
                    cinit.push_str(",\n");
                }
                cinit.push_str("};\n");
                let constant_type = types.insert_group(&[Type::Object(constant_object_idx)]);
                emit_constant_name(vi, &mut cinit);
                cinit.push_str(" = (");
                emit_type(constant_type, types, strings, declared_types, type_declarations, &mut cinit);
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
            ConstantPoolValue::Closure(constant_closure) => {
                let closure_type_idx = types.insert_dedup_closure((
                    constant_closure.parameter_types.clone(),
                    constant_closure.return_type
                ));
                let closure_idx = closure_type_idx.get_internal_id();
                let closure_type = types.insert_group(&[Type::Closure(closure_type_idx)]);
                let mut closure_body = String::new();
                // body needs to be done here because we need nested closures to register FIRST
                let mut body_str = String::new();
                let mut body_free = HashSet::new();
                emit_block(
                    &constant_closure.body, &constant_closure.variables,
                    constant_closure.return_type, &mut body_free,
                    &mut constant_closure.captured
                        .iter().map(|(cn, cv)| (*cn, cv.get_type(constants, types))).collect(),
                    closure_bodies, types,
                    constants, external, symbols, strings, declared_types, type_declarations,
                    &mut body_str
                );
                let variant = closure_bodies.len();
                // emit closure captures struct
                let mut captures_typedef = String::new();
                captures_typedef.push_str("typedef struct ");
                emit_closure_captures_name(closure_idx, variant, &mut captures_typedef);
                captures_typedef.push_str(" {\n");
                let mut has_capture = false;
                for (capture_name, capture_value) in &constant_closure.captured {
                    let capture_type = capture_value.get_type(constants, types);
                    if let Type::Unit = types.group_concrete(capture_type) { continue; }
                    has_capture = true;
                    captures_typedef.push_str("    ");
                    emit_type(capture_type, types, strings, declared_types, type_declarations, &mut captures_typedef);
                    captures_typedef.push_str(" ");
                    captures_typedef.push_str(strings.get(*capture_name));
                    captures_typedef.push_str(";\n");
                }
                captures_typedef.push_str("} ");
                emit_closure_captures_name(closure_idx, variant, &mut captures_typedef);
                captures_typedef.push_str(";\n");
                if has_capture {
                    closure_body.push_str(&captures_typedef);
                }
                // emit closure body procedure
                emit_type(constant_closure.return_type, types, strings, declared_types, type_declarations, &mut closure_body);
                closure_body.push_str(" ");
                emit_closure_body_name(closure_idx, variant, &mut closure_body);
                closure_body.push_str("(GeraAllocation* allocation");
                for param_idx in 0..constant_closure.parameter_types.len() {
                    let param_type = constant_closure.parameter_types[param_idx];
                    if let Type::Unit = types.group_concrete(param_type) { continue; }
                    closure_body.push_str(", ");
                    emit_type(param_type, types, strings, declared_types, type_declarations, &mut closure_body);
                    closure_body.push_str(" param");
                    closure_body.push_str(&param_idx.to_string());
                }
                closure_body.push_str(") {\n");
                if has_capture {
                    closure_body.push_str("    ");
                    emit_closure_captures_name(closure_idx, variant, &mut closure_body);
                    closure_body.push_str("* captures = (");
                    emit_closure_captures_name(closure_idx, variant, &mut closure_body);
                    closure_body.push_str("*) allocation->data;\n");
                }
                for variable_idx in 0..constant_closure.variables.len() {
                    let var_type = constant_closure.variables[variable_idx];
                    if let Type::Unit = types.group_concrete(var_type) { continue; }
                    closure_body.push_str("    ");
                    emit_type(var_type, types, strings, declared_types, type_declarations, &mut closure_body);
                    closure_body.push_str(" ");
                    emit_variable(IrVariable { index: variable_idx, version: 0 }, &mut closure_body);
                    closure_body.push_str(" = ");
                    emit_variable_default_value(var_type, types, strings, declared_types, type_declarations, &mut closure_body);
                    closure_body.push_str(";\n");
                }
                if let Type::Unit = types.group_concrete(constant_closure.return_type) {} else {
                    closure_body.push_str("    ");
                    emit_type(constant_closure.return_type, types, strings, declared_types, type_declarations, &mut closure_body);
                    closure_body.push_str(" returned;\n");
                }
                body_str.push_str("\nret:\n");
                emit_scope_decrements(
                    &body_free, &constant_closure.variables, types, strings, &mut body_str
                );
                if let Type::Unit = types.group_concrete(constant_closure.return_type) {
                    body_str.push_str("return;\n");
                } else {
                    body_str.push_str("return returned;\n");
                }
                indent(&body_str, &mut closure_body);
                closure_body.push_str("}\n");
                closure_bodies.push(closure_body);
                // emit closure literal
                emit_constant_name(vi, &mut cinit);
                cinit.push_str(" = (");
                emit_type(closure_type, types, strings, declared_types, type_declarations, &mut cinit);
                cinit.push_str(") {\n");
                cinit.push_str("    .allocation = ");
                if has_capture {
                    cinit.push_str("gera___rc_alloc(sizeof(");
                    emit_closure_captures_name(closure_idx, variant, &mut cinit);
                    cinit.push_str("), &gera___free_nothing),\n");
                } else {
                    cinit.push_str("NULL,\n");
                }
                cinit.push_str("    .procedure = &");
                emit_closure_body_name(closure_idx, variant, &mut cinit);
                cinit.push_str("\n");
                cinit.push_str("};\n");
                if has_capture {
                    emit_closure_captures_name(closure_idx, variant, &mut cinit);
                    cinit.push_str("* ");
                    emit_constant_name(vi, &mut cinit);
                    cinit.push_str("_captures = (");
                    emit_closure_captures_name(closure_idx, variant, &mut cinit);
                    cinit.push_str("*) ");
                    emit_constant_name(vi, &mut cinit);
                    cinit.push_str(".allocation->data;\n");
                    for (capture_name, capture_value) in &constant_closure.captured {
                        emit_constant_name(vi, &mut cinit);
                        cinit.push_str("_captures->");
                        cinit.push_str(strings.get(*capture_name));
                        cinit.push_str(" = ");
                        let capture_type = capture_value.get_type(constants, types);
                        emit_value(*capture_value, capture_type, types, constants, strings, &mut cinit);
                        cinit.push_str(";\n");
                    }
                }
            }
            ConstantPoolValue::Variant(_, _, _) => {}
        }
        vi += 1;
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
    output.push_str("int main(int argc, char** argv) {\n");
    output.push_str("    gera___set_args(argc, argv);\n");
    output.push_str("    gera_init_constants();\n");
    output.push_str("    ");
    emit_procedure_name(main_procedure_path, 0, strings, output);
    output.push_str("();\n");
    output.push_str("    return 0;\n");
    output.push_str("}\n");
}

fn emit_type(
    t: TypeGroup, types: &mut TypeMap, strings: &StringMap,
    declared_types: &mut HashMap<String, bool>, type_declarations: &mut String, output: &mut String
) {
    emit_type_declaration(t, types, strings, declared_types, type_declarations);
    match types.group_concrete(t) {
        Type::Any | Type::Unit => output.push_str("void"),
        Type::Boolean => output.push_str("gbool"),
        Type::Integer => output.push_str("gint"),
        Type::Float => output.push_str("gfloat"),
        Type::String => output.push_str("GeraString"),
        Type::Array(_) => output.push_str("GeraArray"),
        Type::Object(o) => emit_object_name(o.get_internal_id(), output),
        Type::ConcreteObject(o) => emit_concrete_object_name(o.get_internal_id(), output),
        Type::Variants(v) => emit_variants_name(v.get_internal_id(), output),
        Type::Closure(p) => emit_closure_name(p.get_internal_id(), output),
    }
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
        &value
            .replace("\\", "\\\\")
            .replace("\n", "\\n")
            .replace("\r", "\\r")
            .replace("\"", "\\\"")
    );
    output.push('"');
}

fn emit_value(
    value: ConstantValue, value_type: TypeGroup, types: &mut TypeMap,
    constants: &ConstantPool, strings: &StringMap, output: &mut String
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
        ConstantValue::Closure(c) => emit_constant_name(c.into(), output),
        ConstantValue::Variant(v) => {
            let variant_idx = if let Type::Variants(variant_idx) = types.group_concrete(value_type) {
                variant_idx.get_internal_id()
            } else { panic!("value type should be variants"); };
            let (variant_tag, variant_value, _) = constants.get_variant(v);
            output.push_str("(");
            emit_variants_name(variant_idx, output);
            output.push_str(") { .tag = ");
            output.push_str(&variant_tag.0.to_string());
            let variant_type = *types.internal_variants()[variant_idx].0.get(&variant_tag)
                .expect("variant should exist");
            if let Type::Unit = types.group_concrete(variant_type) {} else {
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
    variable_types: &Vec<TypeGroup>,
    return_type: TypeGroup,
    free: &mut HashSet<usize>,
    capture_types: &mut HashMap<StringIdx, TypeGroup>,
    closure_bodies: &mut Vec<String>,
    types: &mut TypeMap,
    constants: &mut ConstantPool,
    external: &HashMap<NamespacePath, StringIdx>,
    symbols: &Vec<IrSymbol>,
    strings: &StringMap,
    declared_types: &mut HashMap<String, bool>, type_declarations: &mut String,
    output: &mut String
) {
    output.push_str("{\n");
    for instruction in instructions {
        let mut o = String::new();
        emit_instruction(
            instruction, variable_types, return_type, free, capture_types, closure_bodies,
            types, constants, external,
            symbols, strings, declared_types, type_declarations, &mut o
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

fn emit_instruction(
    instruction: &IrInstruction,
    variable_types: &Vec<TypeGroup>,
    return_type: TypeGroup,
    free: &mut HashSet<usize>,
    capture_types: &mut HashMap<StringIdx, TypeGroup>,
    closure_bodies: &mut Vec<String>,
    types: &mut TypeMap,
    constants: &mut ConstantPool,
    external: &HashMap<NamespacePath, StringIdx>,
    symbols: &Vec<IrSymbol>,
    strings: &StringMap,
    declared_types: &mut HashMap<String, bool>, type_declarations: &mut String,
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
            output.push_str(" = gera___wrap_static_string(");
            emit_string_literal(strings.get(*value), output);
            output.push_str(");\n");
        }
        IrInstruction::LoadObject { member_values, into } => {
            let object_idx = if let Type::Object(object_idx) = types.group_concrete(variable_types[into.index]) {
                object_idx.get_internal_id()
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
                if let Type::Unit = types.group_concrete(variable_types[member_value.index]) { continue; }
                output.push_str("    object->member");
                output.push_str(&member_name.0.to_string());
                output.push_str(" = ");
                let mut member_value_str = String::new();
                emit_variable(*member_value, &mut member_value_str);
                output.push_str(&member_value_str);
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
            let array_idx = if let Type::Array(array_idx) = types.group_concrete(variable_types[into.index]) {
                array_idx.get_internal_id()
            } else { panic!("should be an array"); };
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            let element_type = types.internal_arrays()[array_idx];
            output.push_str("{\n");
            output.push_str("    GeraAllocation* allocation = gera___rc_alloc(");
            if let Type::Unit = types.group_concrete(element_type) {
                output.push_str("1, &gera___free_nothing");
            } else if element_values.len() == 0 {
                output.push_str("1, &gera___free_nothing");
            } else {
                output.push_str("sizeof(");
                emit_type(element_type, types, strings, declared_types, type_declarations, output);
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
            if let Type::Unit = types.group_concrete(element_type) {} else {
                output.push_str("    ");
                emit_type(element_type, types, strings, declared_types, type_declarations, output);
                output.push_str("* elements = (");
                emit_type(element_type, types, strings, declared_types, type_declarations, output);
                output.push_str("*) allocation->data;\n");
                for value_idx in 0..element_values.len() {
                    output.push_str("    elements[");
                    output.push_str(&value_idx.to_string());
                    output.push_str("] = ");
                    let mut element_value_str = String::new();
                    emit_variable(element_values[value_idx], &mut element_value_str);
                    output.push_str(&element_value_str);
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
            let variant_idx = if let Type::Variants(v) = types.group_concrete(variable_types[into.index]) {
                v.get_internal_id()
            } else { panic!("should be a variant"); };
            output.push_str("(");
            emit_variants_name(variant_idx, output);
            output.push_str(") { .tag = ");
            output.push_str(&name.0.to_string());
            if let Type::Unit = types.group_concrete(variable_types[v.index]) {} else {
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
                    panic!("Should've been converted to 'IrInstruction::LoadProcedure'!")
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
            if let Type::Unit = types.group_concrete(variable_types[into.index]) { return; }
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
            let closure_idx = if let Type::Closure(closure_idx) = types.group_concrete(variable_types[into.index]) {
                closure_idx.get_internal_id()
            } else { panic!("should be closure type"); };
            let mut closure_body = String::new();
            // body needs to be done here because we need nested closures to register FIRST
            let mut body_str = String::new();
            let mut body_free = HashSet::new();
            emit_block(
                body, variables, *return_type, &mut body_free,
                &mut captured.iter().map(|(cn, cv)| (*cn, variable_types[cv.index])).collect(),
                closure_bodies, types,
                constants, external, symbols, strings, declared_types, type_declarations,
                &mut body_str
            );
            let variant = closure_bodies.len();
            // emit closure captures struct
            let mut captures_typedef = String::new();
            captures_typedef.push_str("typedef struct ");
            emit_closure_captures_name(closure_idx, variant, &mut captures_typedef);
            captures_typedef.push_str(" {\n");
            let mut has_capture = false;
            for (capture_name, capture_variable) in captured {
                let capture_type = variable_types[capture_variable.index];
                if let Type::Unit = types.group_concrete(capture_type) { continue; }
                has_capture = true;
                captures_typedef.push_str("    ");
                emit_type(capture_type, types, strings, declared_types, type_declarations, &mut captures_typedef);
                captures_typedef.push_str(" ");
                captures_typedef.push_str(strings.get(*capture_name));
                captures_typedef.push_str(";\n");
            }
            captures_typedef.push_str("} ");
            emit_closure_captures_name(closure_idx, variant, &mut captures_typedef);
            captures_typedef.push_str(";\n");
            if has_capture {
                closure_body.push_str(&captures_typedef);
            }
            // emit closure captures free
            if has_capture {
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
                    if let Type::Unit = types.group_concrete(capture_type) { continue; }
                    let mut capture_rc_decr = String::new();
                    emit_rc_decr(
                        &format!("captures->{}", strings.get(*capture_name)),
                        capture_type, types, strings, &mut capture_rc_decr
                    );
                    indent(&capture_rc_decr, &mut closure_body);
                }
                closure_body.push_str("}\n");
            }
            // emit closure body procedure
            emit_type(*return_type, types, strings, declared_types, type_declarations, &mut closure_body);
            closure_body.push_str(" ");
            emit_closure_body_name(closure_idx, variant, &mut closure_body);
            closure_body.push_str("(GeraAllocation* allocation");
            for param_idx in 0..parameter_types.len() {
                let param_type = parameter_types[param_idx];
                if let Type::Unit = types.group_concrete(param_type) { continue; }
                closure_body.push_str(", ");
                emit_type(param_type, types, strings, declared_types, type_declarations, &mut closure_body);
                closure_body.push_str(" param");
                closure_body.push_str(&param_idx.to_string());
            }
            closure_body.push_str(") {\n");
            if has_capture {
                closure_body.push_str("    ");
                emit_closure_captures_name(closure_idx, variant, &mut closure_body);
                closure_body.push_str("* captures = (");
                emit_closure_captures_name(closure_idx, variant, &mut closure_body);
                closure_body.push_str("*) allocation->data;\n");
            }
            for variable_idx in 0..variables.len() {
                let var_type = variables[variable_idx];
                if let Type::Unit = types.group_concrete(var_type) { continue; }
                closure_body.push_str("    ");
                emit_type(var_type, types, strings, declared_types, type_declarations, &mut closure_body);
                closure_body.push_str(" ");
                emit_variable(IrVariable { index: variable_idx, version: 0 }, &mut closure_body);
                closure_body.push_str(" = ");
                emit_variable_default_value(var_type, types, strings, declared_types, type_declarations, &mut closure_body);
                closure_body.push_str(";\n");
            }
            if let Type::Unit = types.group_concrete(*return_type) {} else {
                closure_body.push_str("    ");
                emit_type(*return_type, types, strings, declared_types, type_declarations, &mut closure_body);
                closure_body.push_str(" returned;\n");
            }
            body_str.push_str("\nret:\n");
            emit_scope_decrements(&body_free, &variables, types, strings, &mut body_str);
            if let Type::Unit = types.group_concrete(*return_type) {
                body_str.push_str("return;\n");
            } else {
                body_str.push_str("return returned;\n");
            }
            indent(&body_str, &mut closure_body);
            closure_body.push_str("}\n");
            closure_bodies.push(closure_body);
            // emit closure literal
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            output.push_str("{\n");
            output.push_str("    GeraAllocation* allocation = ");
            if has_capture {
                output.push_str("gera___rc_alloc(sizeof(");
                emit_closure_captures_name(closure_idx, variant, output);
                output.push_str("), &");
                emit_closure_free_name(closure_idx, variant, output);
                output.push_str(");\n");
            } else {
                output.push_str("NULL;\n");
            }
            output.push_str("    ");
            output.push_str(&into_str);
            output.push_str(".allocation = allocation;\n");
            output.push_str("    ");
            output.push_str(&into_str);
            output.push_str(".procedure = &");
            emit_closure_body_name(closure_idx, variant, output);
            output.push_str(";\n");
            if has_capture {
                output.push_str("    ");
                emit_closure_captures_name(closure_idx, variant, output);
                output.push_str("* captures = (");
                emit_closure_captures_name(closure_idx, variant, output);
                output.push_str("*) allocation->data;\n");
                for (capture_name, capture_value) in captured {
                    if let Type::Unit = types.group_concrete(variable_types[capture_value.index]) {
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
            }
            output.push_str("}\n");
            free.insert(into.index);
        }
        IrInstruction::LoadValue { value, into } => {
            emit_variable(*into, output);
            output.push_str(" = ");
            emit_value(*value, variable_types[into.index], types, constants, strings, output);
            output.push_str(";\n");
        }
        IrInstruction::GetObjectMember { accessed, member, into } => {
            if let Type::Unit = types.group_concrete(variable_types[into.index]) { return; }
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            let mut accessed_str = String::new();
            emit_variable(*accessed, &mut accessed_str);
            output.push_str("gera___rc_lock_read(");
            output.push_str(&accessed_str);
            output.push_str(".allocation);\n");
            output.push_str(&into_str);
            output.push_str(" = ");
            output.push_str("(*");
            output.push_str(&accessed_str);
            output.push_str(".member");
            output.push_str(&member.0.to_string());
            output.push_str(");\n");
            output.push_str("gera___rc_unlock_read(");
            output.push_str(&accessed_str);
            output.push_str(".allocation);\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
            free.insert(into.index);
        }
        IrInstruction::SetObjectMember { value, accessed, member } => {
            if let Type::Unit = types.group_concrete(variable_types[value.index]) { return; }
            let member_type = if let Type::Object(object_idx) = types.group_concrete(variable_types[accessed.index]) {
                *types.object(object_idx).0.get(member).expect("member should exist")
            } else { panic!("accessed should be an object"); };
            let mut accessed_str = String::new();
            emit_variable(*accessed, &mut accessed_str);
            output.push_str("gera___rc_lock_write(");
            output.push_str(&accessed_str);
            output.push_str(".allocation);\n");
            let mut member_str = String::new();
            member_str.push_str("(*");
            member_str.push_str(&accessed_str);
            member_str.push_str(".member");
            member_str.push_str(&member.0.to_string());
            member_str.push_str(")");
            emit_rc_decr(&member_str, member_type, types, strings, output);
            output.push_str(&member_str);
            output.push_str(" = ");
            emit_variable(*value, output);
            output.push_str(";\n");
            output.push_str("gera___rc_unlock_write(");
            output.push_str(&accessed_str);
            output.push_str(".allocation);\n");
            emit_rc_incr(&member_str, member_type, types, strings, output);
        }
        IrInstruction::GetArrayElement { accessed, index, into, source } => {
            if let Type::Unit = types.group_concrete(variable_types[into.index]) { return; }
            let element_type = if let Type::Array(array_idx) = types.group_concrete(variable_types[accessed.index]) {
                types.array(array_idx)
            } else { panic!("should be an array"); };
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            let mut accessed_str = String::new();
            emit_variable(*accessed, &mut accessed_str);
            output.push_str("gera___rc_lock_read(");
            output.push_str(&accessed_str);
            output.push_str(".allocation);\n");
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
            output.push_str("((");
            emit_type(element_type, types, strings, declared_types, type_declarations, output);
            output.push_str("*) ");
            output.push_str(&accessed_str);
            output.push_str(".data)[");
            output.push_str(&index_str);
            output.push_str("];\n");
            output.push_str("gera___rc_unlock_read(");
            output.push_str(&accessed_str);
            output.push_str(".allocation);\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
            free.insert(into.index);
        }
        IrInstruction::SetArrayElement { value, accessed, index, source } => {
            if let Type::Unit = types.group_concrete(variable_types[value.index]) { return; }
            let element_type = if let Type::Array(array_idx) = types.group_concrete(variable_types[accessed.index]) {
                types.array(array_idx)
            } else { panic!("should be an array"); };
            let mut accessed_str = String::new();
            emit_variable(*accessed, &mut accessed_str);
            output.push_str("gera___rc_lock_write(");
            output.push_str(&accessed_str);
            output.push_str(".allocation);\n");
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
            emit_type(element_type, types, strings, declared_types, type_declarations, &mut element_str);
            element_str.push_str("*) ");
            element_str.push_str(&accessed_str);
            element_str.push_str(".data)[");
            element_str.push_str(&index_str);
            element_str.push_str("]");
            emit_rc_decr(&element_str, element_type, types, strings, output);
            output.push_str(&element_str);
            output.push_str(" = ");
            emit_variable(*value, output);
            output.push_str(";\n");
            output.push_str("gera___rc_unlock_write(");
            output.push_str(&accessed_str);
            output.push_str(".allocation);\n");
            emit_rc_incr(&element_str, element_type, types, strings, output);
        }
        IrInstruction::GetClosureCapture { name, into } => {
            if let Type::Unit = types.group_concrete(variable_types[into.index]) { return; }
            let mut into_str = String::new();
            emit_variable(*into, &mut into_str);
            emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
            output.push_str("gera___rc_lock_read(allocation);\n");
            output.push_str(&into_str);
            output.push_str(" = ");
            output.push_str("captures->");
            output.push_str(strings.get(*name));
            output.push_str(";\n");
            output.push_str("gera___rc_unlock_read(allocation);\n");
            emit_rc_incr(&into_str, variable_types[into.index], types, strings, output);
            free.insert(into.index);
        }
        IrInstruction::SetClosureCapture { value, name } => {
            if let Type::Unit = types.group_concrete(variable_types[value.index]) { return; }
            output.push_str("gera___rc_lock_write(allocation);\n");
            let mut capture_str = String::new();
            capture_str.push_str("captures->");
            capture_str.push_str(strings.get(*name));
            emit_rc_decr(&capture_str, variable_types[value.index], types, strings, output);
            output.push_str(&capture_str);
            output.push_str(" = ");
            emit_variable(*value, output);
            output.push_str(";\n");
            output.push_str("gera___rc_unlock_write(allocation);\n");
            emit_rc_incr(&capture_str, variable_types[value.index], types, strings, output);
        }
        IrInstruction::Move { from, into } => {
            if let Type::Unit = types.group_concrete(variable_types[into.index]) { return; }
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
            if let Type::Integer = types.group_concrete(variable_types[a.index]) {
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
            if let Type::Integer = types.group_concrete(variable_types[a.index]) {
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
            if let Type::Float = types.group_concrete(variable_types[a.index]) {
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
            match types.group_concrete(variable_types[value.index]) {
                Type::Integer | Type::Boolean => {
                    output.push_str("switch(");
                    emit_variable(*value, output);
                    output.push_str(") {\n");
                    for (branch_value, branch_body) in branches {
                        output.push_str("    case ");
                        emit_value(*branch_value, variable_types[value.index], types, constants, strings, output);
                        output.push_str(":\n");
                        let mut branch = String::new();
                        for instruction in branch_body {
                            emit_instruction(
                                instruction, variable_types, return_type, free, capture_types,
                                closure_bodies, types,
                                constants, external, symbols, strings, declared_types, type_declarations,
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
                                instruction, variable_types, return_type, free, capture_types,
                                closure_bodies, types,
                                constants, external, symbols, strings, declared_types, type_declarations,
                                &mut else_branch_str
                            );
                        }
                        let mut else_branch_indented = String::new();
                        indent(&else_branch_str, &mut else_branch_indented);
                        indent(&else_branch_indented, output);
                    }
                    output.push_str("}\n");
                }
                _ => {
                    let mut v_str = String::new();
                    emit_variable(*value, &mut v_str);
                    for branch_idx in 0..branches.len() {
                        output.push_str("if(");
                        let mut b_str = String::new();
                        emit_value(branches[branch_idx].0, variable_types[value.index], types, constants, strings, &mut b_str);
                        emit_equality(&v_str, &b_str, variable_types[value.index], types, output);
                        output.push_str(") ");
                        emit_block(
                            &branches[branch_idx].1, variable_types, return_type, free, capture_types,
                            closure_bodies, types,
                            constants, external, symbols, strings, declared_types, type_declarations,
                            output
                        );
                        output.push_str(" else ");
                    }
                    emit_block(
                        else_branch, variable_types, return_type, free, capture_types, closure_bodies,
                        types, constants,
                        external, symbols, strings, declared_types, type_declarations,
                        output
                    );
                    output.push_str("\n");
                }
            }

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
                    let branch_variable_type = variable_types[branch_variable.index];
                    if let Type::Unit = types.group_concrete(branch_variable_type) {} else {
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
                        instruction, variable_types, return_type, free, capture_types,
                        closure_bodies, types,
                        constants, external, symbols, strings, declared_types, type_declarations,
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
                        closure_bodies, types,
                        constants, external, symbols, strings,
                        declared_types, type_declarations, &mut branch
                    );
                }
                let mut branch_indented = String::new();
                indent(&branch, &mut branch_indented);
                indent(&branch_indented, output);
            }
            output.push_str("\n}\n");
        }
        IrInstruction::Call { path, variant, arguments, into, source: _ } => {
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
            let returns_value = if let Type::Unit = types.group_concrete(*return_type) { false }
                else { true };
            let mut value = String::new();
            if let Some(backing) = external.get(path) {
                emit_external_function_wrapper(strings.get(*backing), &mut value);
            } else {
                emit_procedure_name(path, *variant, strings, &mut value);
            }
            value.push_str("(");
            let mut had_param = false;
            for argument_idx in 0..arguments.len() {
                let param_type = parameter_types[argument_idx];
                if let Type::Unit = types.group_concrete(param_type) { continue; }
                if had_param { value.push_str(", "); }
                had_param = true;
                emit_variable(arguments[argument_idx], &mut value);
            }
            value.push_str(")");
            if returns_value {
                let mut into_str = String::new();
                emit_variable(*into, &mut into_str);
                emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
                output.push_str(&into_str);
                output.push_str(" = ");
            }
            output.push_str(&value);
            output.push_str(";\n");
            free.insert(into.index);
        }
        IrInstruction::CallClosure { called, arguments, into, source: _ } => {
            let (parameter_types, return_type) = if let Type::Closure(p)
                    = types.group_concrete(variable_types[called.index]) {
                        types.closure(p).clone()
            } else { panic!("should be a closure"); };
            let returns_value = if let Type::Unit = types.group_concrete(return_type) { false }
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
                if let Type::Unit = types.group_concrete(parameter_types[argument_idx]) { continue; }
                value.push_str(", ");
                emit_variable(arguments[argument_idx], &mut value);
            }
            value.push_str(")");
            if returns_value {
                let mut into_str = String::new();
                emit_variable(*into, &mut into_str);
                emit_rc_decr(&into_str, variable_types[into.index], types, strings, output);
                output.push_str(&into_str);
                output.push_str(" = ");
            }
            output.push_str(&value);
            output.push_str(";\n");
            free.insert(into.index);
        }
        IrInstruction::Return { value } => {
            if let Type::Unit = types.group_concrete(variable_types[value.index]) {
                output.push_str("goto ret;\n");
            } else {
                output.push_str("returned = ");
                let mut returned = String::new();
                emit_variable(*value, &mut returned);
                output.push_str(&returned);
                output.push_str(";\n");
                emit_rc_incr(&returned, variable_types[value.index], types, strings, output);
                output.push_str("goto ret;\n");
            }
        }
        IrInstruction::Phi { .. } => {}
    }
}