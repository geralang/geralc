
use std::collections::HashMap;

use crate::backend::{
    ir::{IrSymbol, IrTypeBank, IrType, IrInstruction},
    interpreter::Value
};
use crate::frontend::modules::NamespacePath;
use crate::util::strings::{StringMap, StringIdx};

use super::ir::IrVariable;


pub fn generate_c(
    symbols: Vec<IrSymbol>,
    types: IrTypeBank,
    main_procedure_path: NamespacePath,
    strings: &mut StringMap
) -> String {
    let mut output = String::new();
    let mut external = HashMap::new();
    output.push_str("\n");
    emit_core_functions(&mut output);
    output.push_str("\n");
    emit_declarations(&symbols, &types, strings, &mut external, &mut output);
    output.push('\n');
    emit_procedure_impls(&symbols, &types, strings, &mut external, &mut output);
    output.push('\n');
    emit_main_function(&main_procedure_path, strings, &mut output);
    return output;
}

fn emit_core_functions(
    output: &mut String
) {
    output.push_str("double gerafmod(double x, double div) {
    if (div != div || x != x) { return x; }
    if (div == 0) { return (0.0f / 0.0f); }
    return x - (int) (x / div) * div;
}\n");
    output.push_str("char gerastreq(char* a, char* b) {
    char eq = 0;
    for(unsigned long long int i = 0;; i += 1) {
        if(a[i] != b[i]) { return 0; }
        if(a[i] != '\\0') { continue; }
        return 1;
    }
}\n");
}

fn emit_declarations(
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
            emit_block(body, variables, types, external, strings, &mut body_str);
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
        IrType::Object(o) => {
            output.push_str("GeraObject");
            output.push_str(&o.0.to_string());
        }
        IrType::ConcreteObject(o) => {
            output.push_str("GeraConcreteObject");
            output.push_str(&o.0.to_string());
        }
        IrType::Variants(v) => {
            output.push_str("GeraVariants");
            output.push_str(&v.0.to_string());
        }
        IrType::ProcedurePtr(p) => {
            output.push_str("GeraProcPtr");
            output.push_str(&p.0.to_string());
        }
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
    strings: &StringMap,
    output: &mut String
) {
    output.push_str("{");
    for instruction in instructions {
        let mut o = String::new();
        emit_instruction(instruction, variable_types, types, external, strings, &mut o);
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
            output.push_str("gerastreq(");
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
        IrInstruction::LoadVariant { name, v, into } => todo!("emit variant literals"),
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
                output.push_str("gerafmod(");
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
                    &branches[branch_idx].1, variable_types, types, external, strings,
                    &mut branch_str
                );
                indent(&branch_str, output, false);
                output.push_str(" else ");
            }
            let mut else_branch_str = String::new();
            emit_block(else_branch, variable_types, types, external, strings, &mut else_branch_str);
            indent(&else_branch_str, output, false);
            output.push_str("\n}\n");
        }
        IrInstruction::BranchOnVariant { value, branches, else_branch } => todo!("emit branch on variant"),
        IrInstruction::Call { path, variant, arguments, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) {} else {
                emit_variable(*into, output);
                output.push_str(" = ");
            }
            if let Some(backing) = external.get(path) {
                output.push_str(strings.get(*backing));
            } else {
                emit_procedure_name(path, *variant, strings, output);
            }
            output.push_str("(");
            for argument_idx in 0..arguments.len() {
                if argument_idx >= 1 { output.push_str(", "); }
                emit_variable(arguments[argument_idx], output);
            }
            output.push_str(");\n");
        }
        IrInstruction::CallPtr { called, arguments, into } => {
            if let IrType::Unit = variable_types[into.index].direct(types) {} else {
                emit_variable(*into, output);
                output.push_str(" = (");
            }
            emit_variable(*called, output);
            output.push_str(")(");
            for argument_idx in 0..arguments.len() {
                if argument_idx >= 1 { output.push_str(", "); }
                emit_variable(arguments[argument_idx], output);
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