
use std::collections::HashMap;

use crate::util::{
    strings::StringIdx,
    error::Error,
    strings::StringMap
};
use crate::frontend::{
    ast::{TypedAstNode, HasAstNodeVariant, AstNodeVariant},
    types::{TypeScope, PossibleTypes, Type},
    type_checking::Symbol,
    modules::NamespacePath
};
use crate::backend::{
    ir::{IrInstruction, IrVariable, IrType, IrTypeBank, IrSymbol, IrIndirectTypeIdx},
    interpreter::{Interpreter, Value}
};


fn possible_types_to_ir_type(
    type_scope: &TypeScope,
    converted: &PossibleTypes,
    type_bank: &mut IrTypeBank,
    strings: &mut StringMap,
    encountered: &mut HashMap<usize, Option<IrIndirectTypeIdx>>
) -> IrType {
    match converted {
        PossibleTypes::Any => {
            IrType::Unit // If 'any' it reached this point, it's unused.
        }
        PossibleTypes::OfGroup(group_idx) => {
            let group_internal_idx = type_scope.get_group_internal_index(group_idx);
            if let Some(indirect_idx) = encountered.get_mut(&group_internal_idx) {
                if indirect_idx.is_none() {
                    *indirect_idx = Some(type_bank.insert_indirect(IrType::Unit));
                }
                return IrType::Indirect(indirect_idx.expect("should be inside, inserted above"));
            }
            encountered.insert(group_internal_idx, None);
            let ir_type = possible_types_to_ir_type(
                type_scope, type_scope.get_group_types(group_idx), type_bank, strings, encountered
            );
            let indirect_idx = encountered.get(&group_internal_idx).expect("should still be inside");
            return if let Some(indirect_idx) = indirect_idx {
                type_bank.overwrite_indirect(*indirect_idx, ir_type);
                IrType::Indirect(*indirect_idx)
            } else {
                ir_type
            };
        }
        PossibleTypes::OneOf(possible_types) => {
            if possible_types.len() != 1 {
                panic!("possible types should be concrete!");
            }
            type_to_ir_type(type_scope, &possible_types[0], type_bank, strings, encountered)
        }
    }
}

fn type_to_ir_type(
    type_scope: &TypeScope,
    converted: &Type,
    type_bank: &mut IrTypeBank,
    strings: &mut StringMap,
    encountered: &mut HashMap<usize, Option<IrIndirectTypeIdx>>
) -> IrType {
    match converted {
        Type::Unit => IrType::Unit,
        Type::Boolean => IrType::Boolean,
        Type::Integer => IrType::Integer,
        Type::Float => IrType::Float,
        Type::String => IrType::String,
        Type::Array(element_types) => {
            let element_type = possible_types_to_ir_type(
                type_scope, element_types, type_bank, strings, encountered
            );
            IrType::Array(type_bank.insert_array(element_type))
        }
        Type::Object(member_types, _) => {
            let member_types = member_types.iter().map(|(member_name, member_types)| {
                (*member_name, possible_types_to_ir_type(
                    type_scope, member_types, type_bank, strings, encountered
                ))
            }).collect();
            IrType::Object(type_bank.insert_object(member_types))
        }
        Type::ConcreteObject(members) => {
            let members = members.iter().map(|(member_name, member_type)| {
                (*member_name, type_to_ir_type(
                    type_scope, member_type, type_bank, strings, encountered
                ))
            }).collect();
            IrType::ConcreteObject(type_bank.insert_concrete_object(members))
        }
        Type::Closure(arguments, returns, captured) => {
            let captured = captured.as_ref().map(|captured| {
                captured.iter().map(|(capture_name, capture_type)| {
                    (*capture_name, possible_types_to_ir_type(
                        type_scope, &PossibleTypes::OfGroup(*capture_type),
                        type_bank, strings, encountered
                    ))
                }).collect()
            }).unwrap_or(HashMap::new());
            let capture_obj = type_bank.insert_object(captured);
            let mut procedure_signature = (
                arguments.iter().map(|arg| possible_types_to_ir_type(
                    type_scope, &PossibleTypes::OfGroup(*arg), type_bank, strings, encountered
                )).collect::<Vec<IrType>>(),
                possible_types_to_ir_type(
                    type_scope, &PossibleTypes::OfGroup(*returns), type_bank, strings, encountered
                )
            );
            procedure_signature.0.insert(0, IrType::Object(capture_obj));
            let procedure_ptr = type_bank.insert_procedure_ptr(procedure_signature);
            let closure_obj = type_bank.insert_object([
                (strings.insert("data"), IrType::Object(capture_obj)),
                (strings.insert("proc"), IrType::ProcedurePtr(procedure_ptr))
            ].into());
            IrType::Object(closure_obj)
        }
        Type::Variants(variant_types, _) => {
            let variant_types = variant_types.iter().map(|(variant_name, variant_types)| {
                (*variant_name, possible_types_to_ir_type(
                    type_scope, variant_types, type_bank, strings, encountered
                ))
            }).collect();
            IrType::Variants(type_bank.insert_variants(variant_types))
        }
    }
}


pub fn lower_typed_ast(
    strings: &mut StringMap,
    typed_symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
    external_backings: &HashMap<NamespacePath, StringIdx>,
    main_procedure: (&NamespacePath, &Symbol<TypedAstNode>)
) -> Result<(Vec<IrSymbol>, IrTypeBank), Error> {
    let mut interpreter = Interpreter::new();
    let mut type_bank = IrTypeBank::new();
    let mut ir_symbols = Vec::new();
    if let Symbol::Procedure {
        parameter_names: _, scope,
        parameter_types: _, returns,
        body
    } = main_procedure.1 {
        let mut generator = IrGenerator::new();
        let body = generator.lower_nodes(
            body.as_ref().expect("should not be external"),
            (IrVariable { index: 0, version: 0 }, &HashMap::new()),
            scope, HashMap::new(), typed_symbols, strings, external_backings, &HashMap::new(),
            &mut interpreter, &mut type_bank, &mut ir_symbols
        )?;
        ir_symbols.push(IrSymbol::Procedure {
            path: main_procedure.0.clone(),
            variant: 0,
            parameter_types: Vec::new(), 
            return_type: possible_types_to_ir_type(
                scope,
                &PossibleTypes::OfGroup(*returns),
                &mut type_bank,
                strings,
                &mut HashMap::new()
            ),
            variables: generator.variables.into_iter()
                .map(|v| v.1)
                .collect(),
            body
        });
    } else { panic!("should be a procedure"); }
    for (symbol_path, typed_symbol) in typed_symbols {
        match typed_symbol {
            Symbol::Constant { value, value_types } => {
                if let Some(value) = value {
                    let v = if let Some(value) = interpreter.get_constant_value(symbol_path) {
                        value.clone()
                    } else {
                        interpreter.evaluate_node(value, typed_symbols, strings)?
                    };
                    ir_symbols.push(IrSymbol::Variable {
                        path: symbol_path.clone(),
                        value_type: possible_types_to_ir_type(
                            &TypeScope::new(), value_types, &mut type_bank, strings,
                            &mut HashMap::new()
                        ),
                        value: v
                    });
                } else {
                    ir_symbols.push(IrSymbol::ExternalVariable {
                        path: symbol_path.clone(),
                        backing: *external_backings.get(symbol_path).expect("should have backing"),
                        value_type: possible_types_to_ir_type(
                            &TypeScope::new(), value_types, &mut type_bank, strings,
                            &mut HashMap::new()
                        )
                    });
                }
            }
            Symbol::Procedure { .. } => {
                // IrGenerator will lower needed procedures
            }
        }
    }
    Ok((ir_symbols, type_bank))
}


struct IrGenerator {
    instructions: Vec<Vec<IrInstruction>>,
    variables: Vec<(usize, IrType)>,
    // reusable: Vec<usize>,
    closure_captured: Vec<StringIdx>
}

impl IrGenerator {
    fn new() -> IrGenerator {
        IrGenerator { 
            instructions: Vec::new(),
            variables: Vec::new(),
            // reusable: Vec::new(),
            closure_captured: Vec::new()
        }
    }

    fn lower_nodes(
        &mut self,
        nodes: &[TypedAstNode],
        captured: (IrVariable, &HashMap<StringIdx, IrType>),
        type_scope: &TypeScope,
        mut named_variables: HashMap<StringIdx, usize>,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &mut StringMap,
        external_backings: &HashMap<NamespacePath, StringIdx>,
        call_parameters: &HashMap<StringIdx, IrType>,
        interpreter: &mut Interpreter,
        type_bank: &mut IrTypeBank,
        ir_symbols: &mut Vec<IrSymbol>
    ) -> Result<Vec<IrInstruction>, Error> {
        self.enter();
        for node in nodes {
            self.lower_node(
                node, None, captured, type_scope, &mut named_variables, symbols, strings,
                external_backings, call_parameters, interpreter, type_bank, ir_symbols
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

    fn allocate(&mut self, variable_type: IrType) -> IrVariable {
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

    fn lower_node(
        &mut self,
        node: &TypedAstNode,
        into: Option<IrVariable>,
        captured: (IrVariable, &HashMap<StringIdx, IrType>),
        type_scope: &TypeScope,
        named_variables: &mut HashMap<StringIdx, usize>,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &mut StringMap,
        external_backings: &HashMap<NamespacePath, StringIdx>,
        call_parameters: &HashMap<StringIdx, IrType>,
        interpreter: &mut Interpreter,
        type_bank: &mut IrTypeBank,
        ir_symbols: &mut Vec<IrSymbol>
    ) -> Result<Option<IrVariable>, Error> {
        macro_rules! lower_node { ($node: expr, $into: expr) => {
            self.lower_node(
                $node, $into, captured, type_scope, named_variables, symbols, strings,
                external_backings, call_parameters, interpreter, type_bank, ir_symbols
            )?.expect("should result in a value")
        } }
        macro_rules! node_type { () => {
            possible_types_to_ir_type(
                type_scope, node.get_types(), type_bank, strings, &mut HashMap::new()
            )
        } }
        macro_rules! into_given_or_alloc { ($temp_type: expr) => {
            into.unwrap_or_else(|| self.allocate($temp_type))
        } }
        match node.node_variant() {
            AstNodeVariant::Function { arguments, body } => {
                let node_type = node_type!();
                let (body_proc_ptr, captures_obj)
                    = if let IrType::Object(closure_obj_idx) = node_type.direct(type_bank) { (
                    if let IrType::ProcedurePtr(proc_ptr_idx) = type_bank
                        .get_object(closure_obj_idx)
                        .get(&strings.insert("proc"))
                        .expect("closure object should have 'proc'-property")
                        .direct(type_bank) {
                        proc_ptr_idx
                    } else { panic!("lowered closure type should have a proc ptr"); },
                    if let IrType::Object(captures_obj) = type_bank
                        .get_object(closure_obj_idx)
                        .get(&strings.insert("data"))
                        .expect("closure object should have 'data'-property")
                        .direct(type_bank) {
                        captures_obj
                    } else { panic!("lowered closure type should have a data obj"); }
                ) } else { panic!("lowered closure type should be an object"); };
                let body_proc_signature = type_bank.get_procedure_ptr(body_proc_ptr).clone();
                let captures = type_bank.get_object(captures_obj).clone();
                let body_proc_path = NamespacePath::new(vec![
                    strings.insert(&ir_symbols.len().to_string())
                ]);
                let mut generator = IrGenerator::new();
                let mut parameters = HashMap::new();
                for param_idx in 0..body_proc_signature.0.len() {
                    parameters.insert(
                        if param_idx == 0 { strings.insert("") } else { arguments[param_idx - 1] },
                        body_proc_signature.0[param_idx]
                    );
                }
                let closure_obj = generator.allocate(body_proc_signature.0[0]);
                let mut body_proc_body = generator.lower_nodes(
                    body, (closure_obj, &captures),
                    type_scope, HashMap::new(), symbols, strings, external_backings,
                    &parameters, interpreter, type_bank, ir_symbols
                )?;
                body_proc_body.insert(0, IrInstruction::LoadParameter {
                    name: strings.insert(""),
                    into: closure_obj
                });
                ir_symbols.push(IrSymbol::Procedure {
                    path: body_proc_path.clone(),
                    variant: 0,
                    parameter_types: body_proc_signature.0.clone(),
                    return_type: body_proc_signature.1,
                    variables: generator.variables.into_iter()
                        .map(|v| v.1)
                        .collect(),
                    body: body_proc_body
                });
                let mut captured_values = HashMap::new();
                for (captured_name, _) in &captures {
                    let val_var;
                    if let Some(parameter_type) = call_parameters.get(captured_name) {
                        let into = into_given_or_alloc!(*parameter_type);
                        self.add(IrInstruction::LoadParameter { name: *captured_name, into });
                        val_var = into;
                    } else if let Some(var_idx) = named_variables.get(captured_name) {
                        val_var = IrVariable { 
                            index: *var_idx,
                            version: self.variables[*var_idx].0
                        };
                    } else {
                        let into = into_given_or_alloc!(
                            *captured.1.get(captured_name)
                                .expect("variable should be captured")
                        );
                        self.add(IrInstruction::GetObjectMember {
                            accessed: captured.0,
                            member: *captured_name,
                            into
                        });
                        val_var = into;
                    }
                    captured_values.insert(*captured_name, val_var);
                }
                let captured_data = self.allocate(IrType::Object(captures_obj));
                self.add(IrInstruction::LoadObject {
                    member_values: captured_values, into: captured_data
                });
                let proc_ptr = self.allocate(IrType::ProcedurePtr(body_proc_ptr));
                self.add(IrInstruction::LoadProcedurePtr { path: body_proc_path, into: proc_ptr });
                let into = into_given_or_alloc!(node_type);
                self.add(IrInstruction::LoadObject { member_values: [
                    (strings.insert("data"), captured_data),
                    (strings.insert("proc"), proc_ptr)
                ].into(), into });
                Ok(Some(into))
            }
            AstNodeVariant::Variable { public: _, mutable: _, name, value } => {
                if let Some(value) = value {
                    let var = self.allocate(possible_types_to_ir_type(
                        type_scope, value.get_types(), type_bank, strings, &mut HashMap::new()
                    ));
                    lower_node!(&*value, Some(var));
                    named_variables.insert(*name, var.index);
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
                    let branch_value = interpreter.evaluate_node(&branch.0, symbols, strings)?;
                    let branch_body = self.lower_nodes(
                        &branch.1, captured, type_scope, named_variables.clone(), symbols, strings,
                        external_backings, call_parameters, interpreter, type_bank, ir_symbols
                    )?;
                    branches.push((branch_value, branch_body));
                    branch_scopes.push(self.variables.iter().map(|v| v.0).collect());
                }
                let else_branch = self.lower_nodes(
                    &else_body, captured, type_scope, named_variables.clone(), symbols, strings,
                    external_backings, call_parameters, interpreter, type_bank, ir_symbols
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
                        &body, captured, type_scope, named_variables.clone(), symbols, strings,
                        external_backings, call_parameters, interpreter, type_bank, ir_symbols
                    )?)
                ];
                branch_scopes.push(self.variables.iter().map(|v| v.0).collect());
                let else_branch = self.lower_nodes(
                    &else_body, captured, type_scope, named_variables.clone(), symbols, strings,
                    external_backings, call_parameters, interpreter, type_bank, ir_symbols
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
                    let variant_val_type = possible_types_to_ir_type(
                        type_scope, branch.2.as_ref().expect("should have type"), type_bank,
                        strings, &mut HashMap::new()
                    );
                    let variant_var = self.allocate(variant_val_type);
                    let mut branch_variables = named_variables.clone();
                    branch_variables.insert(branch.1, variant_var.index);
                    let branch_body = self.lower_nodes(
                        &branch.3, captured, type_scope, branch_variables, symbols, strings,
                        external_backings, call_parameters, interpreter, type_bank, ir_symbols
                    )?;
                    branches.push((branch.0, variant_var, branch_body));
                    branch_scopes.push(self.variables.iter().map(|v| v.0).collect());
                }
                let else_branch = if let Some(else_body) = else_body {
                    self.lower_nodes(
                        &else_body, captured, type_scope, named_variables.clone(), symbols, strings,
                        external_backings, call_parameters, interpreter, type_bank, ir_symbols
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
                        self.add(IrInstruction::SetArrayElement { value, accessed, index });
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
                            self.add(IrInstruction::SetObjectMember {
                                value,
                                accessed: captured.0,
                                member: *name
                            });
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
                        parameter_names, scope, parameter_types, returns, body
                    } = symbols.get(path).expect("symbol should exist") {
                        let mut call_type_scope = type_scope.clone();
                        let inserted_proc_scope = call_type_scope.insert_type_scope(&scope);
                        let mut parameter_ir_types = Vec::new();
                        let mut parameter_values = Vec::new();
                        for argument_idx in 0..arguments.len() {
                            let concrete_param_type = call_type_scope.limit_possible_types(
                                &PossibleTypes::OfGroup(inserted_proc_scope.translate_group(
                                    &parameter_types[argument_idx]
                                )),
                                arguments[argument_idx].get_types()
                            ).expect("should have a possible value");
                            let param_ir_type = possible_types_to_ir_type(
                                &call_type_scope, &concrete_param_type, type_bank, strings,
                                &mut HashMap::new()
                            );
                            parameter_ir_types.push(param_ir_type);
                            parameter_values.push(
                                lower_node!(&arguments[argument_idx], None)
                            );
                        }
                        let return_ir_type = possible_types_to_ir_type(
                            &call_type_scope,
                            &inserted_proc_scope.translate(&PossibleTypes::OfGroup(*returns)),
                            type_bank, strings, &mut HashMap::new()
                        );
                        let mut exists = false;
                        let mut proc_variant = 0;
                        for symbol in &*ir_symbols {
                            if let IrSymbol::Procedure {
                                path: proc_path, variant, parameter_types, return_type, ..
                            } = symbol {
                                if path != proc_path { continue; }
                                proc_variant = proc_variant.max(*variant + 1);
                                if *parameter_types != parameter_ir_types { continue; }
                                if *return_type != return_ir_type { continue; }
                                exists = true;
                                break;
                            }
                            if let IrSymbol::ExternalProcedure { path: proc_path, .. } = symbol {
                                if path != proc_path { continue; }
                                exists = true;
                                break;
                            }
                        }
                        if !exists {
                            let mut generator = IrGenerator::new();
                            let mut call_parameters = HashMap::new();
                            for arg_idx in 0..parameter_names.len() {
                                call_parameters.insert(
                                    parameter_names[arg_idx],
                                    parameter_ir_types[arg_idx]
                                );
                            }
                            if let Some(body) = body {
                                let ir_symbol = ir_symbols.len();
                                ir_symbols.push(IrSymbol::Procedure {
                                    path: path.clone(),
                                    variant: proc_variant,
                                    parameter_types: parameter_ir_types,
                                    return_type: return_ir_type,
                                    variables: generator.variables.iter()
                                        .map(|v| v.1)
                                        .collect(),
                                    body: Vec::new()
                                });
                                let new_body = generator.lower_nodes(
                                    body, captured,
                                    &call_type_scope, HashMap::new(), symbols, strings,
                                    external_backings, &call_parameters, interpreter, type_bank, 
                                    ir_symbols
                                )?;
                                if let IrSymbol::Procedure { body, .. } = &mut ir_symbols[ir_symbol] {
                                    *body = new_body;
                                }
                            } else {
                                ir_symbols.push(IrSymbol::ExternalProcedure {
                                    path: path.clone(),
                                    backing: *external_backings
                                        .get(path)
                                        .expect("should have backing"), 
                                    parameter_types: parameter_ir_types,
                                    return_type: return_ir_type,
                                });
                            }
                        }
                        let into = into_given_or_alloc!(node_type!());
                        self.add(IrInstruction::Call {
                            path: path.clone(),
                            arguments: parameter_values,
                            into
                        });
                        return Ok(Some(into));
                    }
                }
                let (captured_data_obj, impl_closure_ptr) = if let IrType::Object(closure_obj)
                    = possible_types_to_ir_type(
                        type_scope, called.get_types(), type_bank, strings, &mut HashMap::new()
                    ).direct(type_bank) { (
                    if let IrType::Object(captured_data_obj) = type_bank
                        .get_object(closure_obj)
                        .get(&strings.insert("data"))
                        .expect("lowered closure type should be an object with a 'data'-property")
                        .direct(type_bank) {
                        captured_data_obj
                    } else { panic!("closure data should be an object"); },
                    if let IrType::ProcedurePtr(impl_closure_ptr) = type_bank
                        .get_object(closure_obj)
                        .get(&strings.insert("proc"))
                        .expect("lowered closure type should be an object with a 'proc'-property")
                        .direct(type_bank) {
                        impl_closure_ptr
                    } else { panic!("closure proc ptr should be an proc ptr"); }
                ) } else { panic!("lowered closure type should be an object"); };
                let closure = lower_node!(&*called, None);
                let called = self.allocate(IrType::ProcedurePtr(impl_closure_ptr));
                self.add(IrInstruction::GetObjectMember {
                    accessed: closure,
                    member: strings.insert("proc"),
                    into: called
                });
                let captured_data = self.allocate(IrType::Object(captured_data_obj));
                self.add(IrInstruction::GetObjectMember {
                    accessed: closure,
                    member: strings.insert("data"),
                    into: captured_data
                });
                let mut parameters = vec![captured_data];
                for argument in arguments {
                    parameters.push(lower_node!(argument, None));
                }
                let into = into_given_or_alloc!(type_bank.get_procedure_ptr(impl_closure_ptr).1);
                self.add(IrInstruction::CallPtr { called, arguments: parameters, into });
                Ok(Some(into))
            }
            AstNodeVariant::Object { values } => {
                let mut member_values = HashMap::new();
                for (member_name, member_value) in values {
                    let member_value = lower_node!(member_value, None);
                    member_values.insert(*member_name, member_value);
                }
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::LoadObject { member_values, into });
                Ok(Some(into))
            }
            AstNodeVariant::Array { values } => {
                let mut element_values = Vec::new();
                for value in values {
                    let value = lower_node!(value, None);
                    element_values.push(value);
                }
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::LoadArray { element_values, into });
                Ok(Some(into))
            }
            AstNodeVariant::ObjectAccess { object, member } => {
                let accessed = lower_node!(&*object, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::GetObjectMember { accessed, member: *member, into });
                Ok(Some(into))
            }
            AstNodeVariant::ArrayAccess { array, index } => {
                let accessed = lower_node!(&*array, None);
                let index = lower_node!(&*index, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::GetArrayElement { accessed, index, into });
                Ok(Some(into))
            }
            AstNodeVariant::VariableAccess { name } => {
                if let Some(parameter_type) = call_parameters.get(name) {
                    let into = into_given_or_alloc!(*parameter_type);
                    self.add(IrInstruction::LoadParameter { name: *name, into });
                    return Ok(Some(into));
                }
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
                } else {
                    let into = into_given_or_alloc!(
                        *captured.1.get(name)
                            .expect("variable should be captured")
                    );
                    self.add(IrInstruction::GetObjectMember {
                        accessed: captured.0,
                        member: *name,
                        into
                    });
                    Ok(Some(into))
                }
            }
            AstNodeVariant::BooleanLiteral { value } => {
                let into = into_given_or_alloc!(IrType::Boolean);
                self.add(IrInstruction::LoadBoolean { value: *value, into });
                Ok(Some(into))
            }
            AstNodeVariant::IntegerLiteral { value } => {
                let into = into_given_or_alloc!(IrType::Integer);
                self.add(IrInstruction::LoadInteger { value: *value, into });
                Ok(Some(into))
            }
            AstNodeVariant::FloatLiteral { value } => {
                let into = into_given_or_alloc!(IrType::Float);
                self.add(IrInstruction::LoadFloat { value: *value, into });
                Ok(Some(into))
            }
            AstNodeVariant::StringLiteral { value } => {
                let into = into_given_or_alloc!(IrType::String);
                self.add(IrInstruction::LoadString { value: *value, into });
                Ok(Some(into))
            }
            AstNodeVariant::UnitLiteral => {
                let into = into_given_or_alloc!(IrType::Unit);
                self.add(IrInstruction::LoadUnit { into });
                Ok(Some(into))
            }
            AstNodeVariant::Add { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::Add { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Subtract { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::Subtract { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Multiply { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::Multiply { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Divide { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::Divide { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Modulo { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::Modulo { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Negate { x } => {
                let x = lower_node!(&*x, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::Negate { x, into });
                Ok(Some(into))
            }
            AstNodeVariant::LessThan { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::LessThan { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::GreaterThan { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::GreaterThan { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::LessThanEqual { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::LessThanEquals { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::GreaterThanEqual { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::GreaterThanEquals { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Equals { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::Equals { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::NotEquals { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::NotEquals { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Not { x } => {
                let x = lower_node!(&*x, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::Not { x, into });
                Ok(Some(into))
            }
            AstNodeVariant::Or { a, b } => {
                let into = into_given_or_alloc!(node_type!());
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
                let into = into_given_or_alloc!(node_type!());
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
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::LoadGlobalVariable { path: path.clone(), into });
                Ok(Some(into))
            }
            AstNodeVariant::Variant { name, value } => {
                let v = lower_node!(&*value, None);
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::LoadVariant { name: *name, v, into });
                Ok(Some(into))
            }
            _ => panic!("this node type should not be in the AST at this point")
        }
    }

}