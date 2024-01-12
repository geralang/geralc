
use std::collections::HashMap;

use crate::{util::{
    strings::StringIdx,
    error::{Error, ErrorSection, ErrorType},
    strings::StringMap,
    source::{SourceRange, HasSource}
}, frontend::types::ScopedTypeGroup};
use crate::frontend::{
    ast::{TypedAstNode, HasAstNodeVariant, AstNodeVariant},
    types::{TypeMap, TypeScope, TypeGroup, Type},
    type_checking::Symbol,
    modules::NamespacePath
};
use crate::backend::{
    ir::{IrInstruction, IrVariable, IrSymbol},
    interpreter::{Interpreter, Value}
};


fn enforce_valid_constant_value(value: &Value, source: SourceRange) -> Result<(), Error> {
    match value {
        Value::Unit |
        Value::Boolean(_) |
        Value::Integer(_) |
        Value::Float(_) |
        Value::String(_) => {},
        Value::Array(elements) => for e in elements.borrow().iter() {
            enforce_valid_constant_value(e, source)?;
        },
        Value::Object(members) => for (_, m) in members.borrow().iter() {
            enforce_valid_constant_value(m, source)?;
        },
        Value::Closure(_, _, _) => return Err(Error::new([
            ErrorSection::Error(ErrorType::ConstantClosure),
            ErrorSection::Code(source),
        ].into())),
        Value::Variant(_, variant_value) => enforce_valid_constant_value(&*variant_value, source)?
    }
    Ok(())
}

pub fn lower_typed_ast(
    strings: &mut StringMap,
    types: &mut TypeMap,
    typed_symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
    external_backings: &HashMap<NamespacePath, StringIdx>,
    main_procedure: (&NamespacePath, &Symbol<TypedAstNode>)
) -> Result<Vec<IrSymbol>, Error> {
    let mut interpreter = Interpreter::new(strings);
    let mut ir_symbols = Vec::new();
    if let Symbol::Procedure {
        public: _,
        parameter_names: _,
        parameter_types: _, returns,
        body, source: _,
        type_scope
    } = main_procedure.1 {
        let mut generator = IrGenerator::new();
        let body = generator.lower_nodes(
            body.as_ref().expect("should not be external"),
            &HashMap::new(),
            types, *type_scope, HashMap::new(), typed_symbols, strings,
            external_backings, &(HashMap::new(), Vec::new()), &mut interpreter, &mut ir_symbols
        )?;
        ir_symbols.push(IrSymbol::Procedure {
            path: main_procedure.0.clone(),
            variant: 0,
            parameter_types: Vec::new(), 
            return_type: *returns,
            variables: generator.variables.into_iter()
                .map(|v| v.1)
                .collect(),
            body,
            type_scope: *type_scope
        });
    } else { panic!("should be a procedure"); }
    for (symbol_path, typed_symbol) in typed_symbols {
        match typed_symbol {
            Symbol::Constant { public: _, value, value_types, type_scope } => {
                if let Some(value) = value {
                    let v = if let Some(value) = interpreter.get_constant_value(symbol_path) {
                        value.clone()
                    } else {
                        interpreter.evaluate_node(value, typed_symbols, external_backings, strings)?
                    };
                    enforce_valid_constant_value(&v, value.source())?;
                    ir_symbols.push(IrSymbol::Variable {
                        path: symbol_path.clone(),
                        value_type: *value_types,
                        value: v,
                        type_scope: *type_scope
                    });
                } else {
                    ir_symbols.push(IrSymbol::ExternalVariable {
                        path: symbol_path.clone(),
                        backing: *external_backings.get(symbol_path).expect("should have backing"),
                        value_type: *value_types,
                        type_scope: *type_scope
                    });
                }
            }
            Symbol::Procedure { .. } => {
                // IrGenerator will lower needed procedures
            }
        }
    }
    Ok(ir_symbols)
}


struct IrGenerator {
    instructions: Vec<Vec<IrInstruction>>,
    variables: Vec<(usize, TypeGroup)>,
    // reusable: Vec<usize>
}

impl IrGenerator {
    fn new() -> IrGenerator {
        IrGenerator { 
            instructions: Vec::new(),
            variables: Vec::new(),
            // reusable: Vec::new()
        }
    }

    fn lower_nodes(
        &mut self,
        nodes: &[TypedAstNode],
        captured: &HashMap<StringIdx, ScopedTypeGroup>,
        types: &mut TypeMap,
        type_scope: TypeScope,
        mut named_variables: HashMap<StringIdx, usize>,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &mut StringMap,
        external_backings: &HashMap<NamespacePath, StringIdx>,
        call_parameters: &(HashMap<StringIdx, usize>, Vec<TypeGroup>),
        interpreter: &mut Interpreter,
        ir_symbols: &mut Vec<IrSymbol>
    ) -> Result<Vec<IrInstruction>, Error> {
        self.enter();
        for node in nodes {
            self.lower_node(
                node, None, captured, types, type_scope, &mut named_variables,
                symbols, strings, external_backings, call_parameters, interpreter,
                ir_symbols
            )?;
        }
        Ok(self.exit())
    }

    fn enter(&mut self) {
        self.instructions.push(Vec::new());
    }

    fn exit(&mut self) -> Vec<IrInstruction> {
        self.instructions.pop().unwrap()
    }

    fn add(&mut self, instruction: IrInstruction) {
        self.instructions.last_mut().unwrap().push(instruction)
    }

    fn allocate(&mut self, variable_type: TypeGroup) -> IrVariable {
        // for reusable_entry_idx in 0..self.reusable.len() {
        //     let variable_idx = self.reusable[reusable_entry_idx];
        //     let variable = &mut self.variables[variable_idx];
        //     if variable.1 != variable_type { continue; }
        //     self.reusable.remove(reusable_entry_idx);
        //     variable.0 += 1;
        //     return IrVariable {
        //         index: variable_idx,
        //         version: 0,
        //         variable_type
        //     }
        // }
        let variable_idx = self.variables.len();
        self.variables.push((0, variable_type));
        return IrVariable {
            index: variable_idx,
            version: 0
        }
    }

    // fn allocate_temporary(&mut self, variable_type: IrType) -> IrVariable {
    //     let v = self.allocate(variable_type);
    //     self.reusable.push(v.index);
    //     v
    // }

    fn insert_phi(
        &mut self,
        branch_scopes: &[Vec<usize>]
    ) {
        let mut variants = Vec::new();
        for branch_vars in branch_scopes {
            for var_idx in 0..branch_vars.len() {
                if var_idx >= variants.len() {
                    variants.push(Vec::new());
                }
                let var_variants = &mut variants[var_idx];
                let new_variant = &branch_vars[var_idx];
                if !var_variants.contains(new_variant) {
                    var_variants.push(*new_variant);
                }
            }
        }
        for var_idx in 0..variants.len() {
            let var_variants = &variants[var_idx];
            if var_variants.len() == 1 { continue; }
            let var = &mut self.variables[var_idx];
            var.0 += 1;
            let options = var_variants.iter()
                .map(|v| IrVariable {
                    index: var_idx,
                    version: *v
                })
                .collect();
            let into = IrVariable {
                index: var_idx,
                version: var.0
            };
            self.add(IrInstruction::Phi { options, into });
        }
    }

    fn find_procedure(
        path: &NamespacePath,
        types: &mut TypeMap,
        type_scope: TypeScope,
        call_parameter_types: Vec<TypeGroup>,
        call_return_type: TypeGroup,
        parameter_names: &Vec<StringIdx>,
        body: &Option<Vec<TypedAstNode>>,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &mut StringMap,
        external_backings: &HashMap<NamespacePath, StringIdx>,
        interpreter: &mut Interpreter,
        ir_symbols: &mut Vec<IrSymbol>
    ) -> Result<usize, Error> {
        let mut exists = false;
        let mut found_variant = 0;
        for symbol in &*ir_symbols {
            match symbol {
                IrSymbol::ExternalProcedure { path: proc_path, .. } => {
                    if path != proc_path { continue; }
                    exists = true;
                    break;
                }   
                IrSymbol::Procedure {
                    path: proc_path, variant: proc_variant, parameter_types, return_type,
                    type_scope: symbol_type_scope, ..
                } | IrSymbol::BuiltInProcedure {
                    path: proc_path, variant: proc_variant, parameter_types, return_type,
                    type_scope: symbol_type_scope
                } => {
                    if path != proc_path { continue; }
                    found_variant = found_variant.max(*proc_variant + 1);
                    if parameter_types.len() != call_parameter_types.len() { continue; }
                    let mut params_eq = true;
                    for param_idx in 0..parameter_types.len() {
                        if types.groups_eq(
                            ScopedTypeGroup::new(call_parameter_types[param_idx], type_scope),
                            ScopedTypeGroup::new(parameter_types[param_idx], *symbol_type_scope)
                        ) {
                            continue;
                        }
                        params_eq = false;
                        break;
                    }
                    if !params_eq { continue; }
                    {
                        if !types.groups_eq(
                            ScopedTypeGroup::new(call_return_type, type_scope),
                            ScopedTypeGroup::new(*return_type, *symbol_type_scope)
                        ) {
                            continue;
                        }
                    }
                    found_variant = *proc_variant;
                    exists = true;
                    break;
                }
                _ => {}
            }
        }
        if !exists {
            if body.is_some() {
                let mut generator = IrGenerator::new();
                let mut call_parameters = (HashMap::new(), Vec::new());
                for arg_idx in 0..parameter_names.len() {
                    call_parameters.0.insert(parameter_names[arg_idx], call_parameters.1.len());
                    call_parameters.1.push(call_parameter_types[arg_idx]);
                }
                if let Some(body) = body {
                    let ir_symbol = ir_symbols.len();
                    ir_symbols.push(IrSymbol::Procedure {
                        path: path.clone(),
                        variant: found_variant,
                        parameter_types: call_parameter_types,
                        return_type: call_return_type,
                        variables: Vec::new(),
                        body: Vec::new(),
                        type_scope: type_scope.clone()
                    });
                    let new_body = generator.lower_nodes(
                        body, &HashMap::new(),
                        types, type_scope, HashMap::new(), symbols, strings,
                        external_backings, &call_parameters, interpreter, 
                        ir_symbols
                    )?;
                    if let IrSymbol::Procedure { body, variables, type_scope: symbol_type_scope, .. }
                        = &mut ir_symbols[ir_symbol] {
                        *body = new_body;
                        *variables = generator.variables.iter()
                            .map(|v| v.1)
                            .collect();
                        *symbol_type_scope = type_scope;
                    }
                } else {
                    panic!("procedure should exist");
                }
            } else if let Some(backing) = external_backings.get(path) {
                ir_symbols.push(IrSymbol::ExternalProcedure {
                    path: path.clone(),
                    backing: *backing, 
                    parameter_types: call_parameter_types,
                    return_type: call_return_type,
                    type_scope
                });
            } else {
                ir_symbols.push(IrSymbol::BuiltInProcedure {
                    path: path.clone(),
                    variant: found_variant,
                    parameter_types: call_parameter_types,
                    return_type: call_return_type,
                    type_scope
                });
            }
        }
        Ok(found_variant)
    }

    fn lower_node(
        &mut self,
        node: &TypedAstNode,
        into: Option<IrVariable>,
        captured: &HashMap<StringIdx, ScopedTypeGroup>,
        types: &mut TypeMap,
        type_scope: TypeScope,
        named_variables: &mut HashMap<StringIdx, usize>,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &mut StringMap,
        external_backings: &HashMap<NamespacePath, StringIdx>,
        call_parameters: &(HashMap<StringIdx, usize>, Vec<TypeGroup>),
        interpreter: &mut Interpreter,
        ir_symbols: &mut Vec<IrSymbol>
    ) -> Result<Option<IrVariable>, Error> {
        macro_rules! lower_node { ($node: expr, $into: expr) => {
            self.lower_node(
                $node, $into, captured, types, type_scope, named_variables,
                symbols, strings, external_backings, call_parameters, interpreter, ir_symbols
            )?.expect("should result in a value")
        } }
        macro_rules! into_given_or_alloc { ($temp_type: expr) => {
            into.unwrap_or_else(|| self.allocate($temp_type))
        } }
        match node.node_variant() {
            AstNodeVariant::Function { arguments, body } => {
                let (parameter_types, return_type, body_captures) =
                    if let Type::Closure(clo) = types.group_concrete(node.get_types(), type_scope) {
                    let t = types.closure(clo);
                    (t.0.clone(), t.1, t.2.clone().expect("node type should be a closure with capture info"))
                } else { panic!("should be a closure!"); };
                let mut body_captured = HashMap::new();
                for (capture_name, _) in &body_captures {
                    if let Some(parameter_index) = call_parameters.0.get(capture_name) {
                        let into = self.allocate(call_parameters.1[*parameter_index]);
                        self.add(IrInstruction::LoadParameter { index: *parameter_index, into });
                        body_captured.insert(*capture_name, into);
                    } else if let Some(var_idx) = named_variables.get(capture_name) {
                        let var = &self.variables[*var_idx];
                        body_captured.insert(*capture_name, IrVariable { 
                            index: *var_idx,
                            version: var.0
                        });
                    } else {
                        let into = self.allocate(
                            captured.get(capture_name)
                                .expect("variable should be captured").group()
                        );
                        self.add(IrInstruction::GetClosureCapture { name: *capture_name, into });
                        body_captured.insert(*capture_name, into);
                    }
                }
                let mut generator = IrGenerator::new();
                let mut parameters = (HashMap::new(), Vec::new());
                for param_idx in 0..parameter_types.len() {
                    parameters.0.insert(arguments[param_idx].0, param_idx);
                    parameters.1.push(parameter_types[param_idx].group());
                }
                let body = generator.lower_nodes(
                    body, &body_captures,
                    types, type_scope, HashMap::new(), symbols, strings,
                    external_backings, &parameters, interpreter, ir_symbols
                )?;
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::LoadClosure {
                    parameter_types: parameters.1,
                    return_type: return_type.group(),
                    captured: body_captured,
                    variables: generator.variables.into_iter()
                        .map(|v| v.1)
                        .collect(),
                    body,
                    into
                });
                Ok(Some(into))
            }
            AstNodeVariant::Variable { public: _, mutable: _, name, value_types, value } => {
                let var = self.allocate(value_types.expect("should have type info"));
                named_variables.insert(*name, var.index);
                if let Some(value) = value {
                    lower_node!(&*value, Some(var));
                    Ok(Some(var))
                } else {
                    Ok(None)
                }
            }
            AstNodeVariant::CaseBranches { value, branches: branch_nodes, else_body } => {
                let value = lower_node!(value, None);
                let mut branches = Vec::new();
                let mut branch_scopes = Vec::new();
                for branch in branch_nodes {
                    let branch_value = interpreter.evaluate_node(
                        &branch.0, symbols, external_backings, strings
                    )?;
                    enforce_valid_constant_value(&branch_value, branch.0.source())?;
                    let branch_body = self.lower_nodes(
                        &branch.1, captured, types, type_scope,
                        named_variables.clone(), symbols, strings, external_backings,
                        call_parameters, interpreter, ir_symbols
                    )?;
                    branches.push((branch_value, branch_body));
                    branch_scopes.push(self.variables.iter().map(|v| v.0).collect());
                }
                let else_branch = self.lower_nodes(
                    &else_body, captured, types, type_scope,
                    named_variables.clone(), symbols, strings, external_backings, call_parameters,
                    interpreter, ir_symbols
                )?;
                branch_scopes.push(self.variables.iter().map(|v| v.0).collect());
                self.add(IrInstruction::BranchOnValue {
                    value,
                    branches,
                    else_branch
                });
                self.insert_phi(&branch_scopes);
                Ok(None)
            }
            AstNodeVariant::CaseConditon { condition, body, else_body } => {
                let value = lower_node!(condition, None);
                let mut branch_scopes = Vec::new();
                let branches = vec![
                    (Value::Boolean(true), self.lower_nodes(
                        &body, captured, types, type_scope,
                        named_variables.clone(), symbols, strings, external_backings,
                        call_parameters, interpreter, ir_symbols
                    )?)
                ];
                branch_scopes.push(self.variables.iter().map(|v| v.0).collect());
                let else_branch = self.lower_nodes(
                    &else_body, captured, types, type_scope,
                    named_variables.clone(), symbols, strings, external_backings, call_parameters,
                    interpreter, ir_symbols
                )?;
                branch_scopes.push(self.variables.iter().map(|v| v.0).collect());
                self.add(IrInstruction::BranchOnValue {
                    value,
                    branches,
                    else_branch
                });
                self.insert_phi(&branch_scopes);
                Ok(None)
            }
            AstNodeVariant::CaseVariant { value, branches: branch_nodes, else_body } => {
                let value = lower_node!(value, None);
                let mut branches = Vec::new();
                let mut branch_scopes = Vec::new();
                for branch in branch_nodes {
                    let mut branch_variables = named_variables.clone();
                    let branch_variant_variable = if let Some((branch_var_variable, _, branch_var_type)) = &branch.1 {
                        let variant_val_type = *branch_var_type.as_ref().expect("should have type");
                        let variant_var = self.allocate(variant_val_type);
                        branch_variables.insert(*branch_var_variable, variant_var.index);
                        Some(variant_var)
                    } else { None };
                    let branch_body = self.lower_nodes(
                        &branch.2, captured, types, type_scope,
                        branch_variables, symbols, strings, external_backings, call_parameters,
                        interpreter, ir_symbols
                    )?;
                    branches.push((branch.0, branch_variant_variable, branch_body));
                    branch_scopes.push(self.variables.iter().map(|v| v.0).collect());
                }
                let else_branch = if let Some(else_body) = else_body {
                    self.lower_nodes(
                        &else_body, captured, types, type_scope,
                        named_variables.clone(), symbols, strings, external_backings,
                        call_parameters, interpreter, ir_symbols
                    )?
                } else { Vec::new() };
                branch_scopes.push(self.variables.iter().map(|v| v.0).collect());
                self.add(IrInstruction::BranchOnVariant {
                    value,
                    branches,
                    else_branch
                });
                self.insert_phi(&branch_scopes);
                Ok(None)
            }
            AstNodeVariant::Assignment { variable, value } => {
                match variable.node_variant() {
                    AstNodeVariant::ObjectAccess { object, member } => {
                        let accessed = lower_node!(&*object, None);
                        let value = lower_node!(&*value, None);
                        self.add(IrInstruction::SetObjectMember { value, accessed, member: *member });
                    }
                    AstNodeVariant::ArrayAccess { array, index } => {
                        let accessed = lower_node!(&*array, None);
                        let index = lower_node!(&*index, None);
                        let value = lower_node!(&*value, None);
                        self.add(IrInstruction::SetArrayElement {
                            value, accessed, index, source: node.source()
                        });
                    }
                    AstNodeVariant::VariableAccess { name } => {
                        if let Some(var_idx) = named_variables.get(name) {
                            let var = &mut self.variables[*var_idx];
                            var.0 += 1;
                            let var = *var;
                            lower_node!(&*value, Some(IrVariable {
                                index: *var_idx,
                                version: var.0
                            }));
                        } else {
                            let value = lower_node!(&*value, None);
                            self.add(IrInstruction::SetClosureCapture { value, name: *name });
                        }
                    }
                    _ => panic!("assignment to invalid node type")
                }
                Ok(None)
            }
            AstNodeVariant::Return { value } => {
                let value = lower_node!(&*value, None);
                self.add(IrInstruction::Return { value });
                Ok(None)
            }
            AstNodeVariant::Call { called, arguments } => {
                if let AstNodeVariant::ModuleAccess { path } = called.node_variant() {
                    if let Symbol::Procedure {
                        public: _, parameter_names, parameter_types, returns, body, source: _,
                        type_scope: symbol_type_scope
                    } = symbols.get(path).expect("symbol should exist") {
                        let call_type_scope = types.create_scope();
                        let mut call_parameter_types = Vec::new();
                        let mut parameter_values = Vec::new();
                        for argument_idx in 0..arguments.len() {
                            let symbol_param_type = parameter_types[argument_idx];
                            types.expand_group(symbol_param_type, *symbol_type_scope, call_type_scope);
                            let given_param_type = arguments[argument_idx].get_types();
                            types.expand_group(given_param_type, type_scope, call_type_scope);
                            types.try_merge_groups(symbol_param_type, given_param_type, call_type_scope);
                            call_parameter_types.push(symbol_param_type);
                            parameter_values.push(
                                lower_node!(&arguments[argument_idx], None)
                            );
                        }
                        let symbol_return_type = *returns;
                        types.expand_group(symbol_return_type, *symbol_type_scope, call_type_scope);
                        let expected_return_type = node.get_types();
                        types.expand_group(expected_return_type, type_scope, call_type_scope);
                        types.try_merge_groups(symbol_return_type, expected_return_type, call_type_scope);
                        let proc_variant = IrGenerator::find_procedure(
                            path, types, call_type_scope, call_parameter_types,
                            symbol_return_type, parameter_names, body, symbols, strings,
                            external_backings, interpreter, ir_symbols
                        )?;
                        let into = into_given_or_alloc!(expected_return_type);
                        self.add(IrInstruction::Call {
                            path: path.clone(),
                            variant: proc_variant,
                            arguments: parameter_values,
                            into,
                            source: node.source()
                        });
                        return Ok(Some(into));
                    }
                }
                let called = lower_node!(&*called, None);
                let mut parameters = Vec::new();
                for argument in arguments {
                    parameters.push(lower_node!(argument, None));
                }
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::CallClosure {
                    called, arguments: parameters, into, source: node.source()
                });
                Ok(Some(into))
            }
            AstNodeVariant::Object { values } => {
                let mut member_values = HashMap::new();
                for (member_name, member_value) in values {
                    let member_value = lower_node!(member_value, None);
                    member_values.insert(*member_name, member_value);
                }
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::LoadObject { member_values, into });
                Ok(Some(into))
            }
            AstNodeVariant::Array { values } => {
                let mut element_values = Vec::new();
                for value in values {
                    let value = lower_node!(value, None);
                    element_values.push(value);
                }
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::LoadArray { element_values, into });
                Ok(Some(into))
            }
            AstNodeVariant::ObjectAccess { object, member } => {
                let accessed = lower_node!(&*object, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::GetObjectMember { accessed, member: *member, into });
                Ok(Some(into))
            }
            AstNodeVariant::ArrayAccess { array, index } => {
                let accessed = lower_node!(&*array, None);
                let index = lower_node!(&*index, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::GetArrayElement {
                    accessed, index, into, source: node.source()
                });
                Ok(Some(into))
            }
            AstNodeVariant::VariableAccess { name } => {
                if let Some(var_idx) = named_variables.get(name) {
                    let var = &self.variables[*var_idx];
                    if let Some(into) = into {
                        self.add(IrInstruction::Move { 
                            from: IrVariable { 
                                index: *var_idx,
                                version: var.0
                            },
                            into 
                        });
                        Ok(Some(into))
                    } else {
                        Ok(Some(IrVariable { 
                            index: *var_idx,
                            version: var.0
                        }))
                    }
                } else if let Some(parameter_index) = call_parameters.0.get(name) {
                    let into = into_given_or_alloc!(call_parameters.1[*parameter_index]);
                    self.add(IrInstruction::LoadParameter { index: *parameter_index, into });
                    Ok(Some(into))
                } else {
                    let into = into_given_or_alloc!(
                        captured.get(name)
                            .expect("variable should be captured").group()
                    );
                    self.add(IrInstruction::GetClosureCapture { name: *name, into });
                    Ok(Some(into))
                }
            }
            AstNodeVariant::BooleanLiteral { value } => {
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::LoadBoolean { value: *value, into });
                Ok(Some(into))
            }
            AstNodeVariant::IntegerLiteral { value } => {
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::LoadInteger { value: *value, into });
                Ok(Some(into))
            }
            AstNodeVariant::FloatLiteral { value } => {
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::LoadFloat { value: *value, into });
                Ok(Some(into))
            }
            AstNodeVariant::StringLiteral { value } => {
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::LoadString { value: *value, into });
                Ok(Some(into))
            }
            AstNodeVariant::UnitLiteral => {
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::LoadUnit { into });
                Ok(Some(into))
            }
            AstNodeVariant::Add { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::Add { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Subtract { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::Subtract { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Multiply { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::Multiply { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Divide { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::Divide {
                    a, b, into, source: node.source()
                });
                Ok(Some(into))
            }
            AstNodeVariant::Modulo { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::Modulo {
                    a, b, into, source: node.source()
                });
                Ok(Some(into))
            }
            AstNodeVariant::Negate { x } => {
                let x = lower_node!(&*x, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::Negate { x, into });
                Ok(Some(into))
            }
            AstNodeVariant::LessThan { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::LessThan { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::GreaterThan { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::GreaterThan { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::LessThanEqual { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::LessThanEquals { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::GreaterThanEqual { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::GreaterThanEquals { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Equals { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::Equals { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::NotEquals { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::NotEquals { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Not { x } => {
                let x = lower_node!(&*x, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::Not { x, into });
                Ok(Some(into))
            }
            AstNodeVariant::Or { a, b } => {
                let into = into_given_or_alloc!(node.get_types());
                let a_var = lower_node!(&*a, Some(IrVariable {
                    index: into.index,
                    version: into.version
                }));
                self.enter();
                let b_var = lower_node!(&*b, Some(IrVariable {
                    index: into.index,
                    version: into.version + 1
                }));
                let b_body = self.exit();
                // no need to call 'insert_phi', as expressions passable to || can't mutate variables
                self.add(IrInstruction::BranchOnValue {
                    value: a_var,
                    branches: vec![
                        (Value::Boolean(false), b_body)
                    ],
                    else_branch: Vec::new()
                });
                let result = IrVariable {
                    index: into.index,
                    version: into.version + 2
                };
                self.variables[result.index].0 = result.version;
                self.add(IrInstruction::Phi {
                    options: vec![a_var, b_var],
                    into: result
                });
                Ok(Some(result))
            }
            AstNodeVariant::And { a, b } => {
                let into = into_given_or_alloc!(node.get_types());
                let a_var = lower_node!(&*a, Some(IrVariable {
                    index: into.index,
                    version: into.version
                }));
                self.enter();
                let b_var = lower_node!(&*b, Some(IrVariable {
                    index: into.index,
                    version: into.version + 1
                }));
                let b_body = self.exit();
                // no need to call 'insert_phi', as expressions passable to || can't mutate variables
                self.add(IrInstruction::BranchOnValue {
                    value: a_var,
                    branches: vec![
                        (Value::Boolean(true), b_body)
                    ],
                    else_branch: Vec::new()
                });
                let result = IrVariable {
                    index: into.index,
                    version: into.version + 2
                };
                self.variables[result.index].0 = result.version;
                self.add(IrInstruction::Phi {
                    options: vec![a_var, b_var],
                    into: result
                });
                Ok(Some(result))
            }
            AstNodeVariant::ModuleAccess { path } => {
                match symbols.get(path).expect("should exist") {
                    Symbol::Constant { .. } => {
                        let into = into_given_or_alloc!(node.get_types());
                        self.add(IrInstruction::LoadGlobalVariable { path: path.clone(), into });
                        Ok(Some(into))
                    }
                    Symbol::Procedure { .. } => {
                        panic!("should have been expanded by the type checker!");
                    }
                }
            }
            AstNodeVariant::Variant { name, value } => {
                let v = lower_node!(&*value, None);
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::LoadVariant { name: *name, v, into });
                Ok(Some(into))
            }
            AstNodeVariant::Static { value } => {
                let v = interpreter.evaluate_node(&*value, symbols, external_backings, strings)?;
                enforce_valid_constant_value(&v, value.source())?;
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::LoadValue { value: v, into });
                Ok(Some(into))
            }
            _ => panic!("this node type should not be in the AST at this point")
        }
    }

}