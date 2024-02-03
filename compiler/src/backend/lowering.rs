
use std::collections::HashMap;

use crate::backend::constants::ConstantValue;
use crate::util::{
    strings::StringIdx,
    error::Error,
    strings::StringMap,
    source::HasSource
};
use crate::frontend::{
    ast::{TypedAstNode, HasAstNodeVariant, AstNodeVariant},
    types::{TypeMap, TypeGroup, Type},
    type_checking::Symbol,
    modules::NamespacePath
};
use crate::backend::{
    ir::{IrInstruction, IrVariable, IrSymbol},
    interpreter::Interpreter,
    constants::ConstantPool
};


#[derive(Debug, Clone)]
pub struct TypeMapping(HashMap<usize, TypeGroup>);

impl TypeMapping {
    fn new() -> TypeMapping {
        TypeMapping(HashMap::new())
    }

    fn add(&mut self, from: TypeGroup, to: TypeGroup, types: &mut TypeMap) {
        if self.0.contains_key(&types.group_internal_id(from)) { return; }
        self.0.insert(types.group_internal_id(from), to);
        for from_t in types.group(from).collect::<Vec<Type>>() {
            let to_t = types.group_concrete(to);
            self.add_type(from_t, to_t, types);
        }
    }

    fn add_type(&mut self, from: Type, to: Type, types: &mut TypeMap) {
        match (from, to) {
            (Type::Any, _) => {}
            (_, Type::Any) => {}
            (Type::Array(from_arr), Type::Array(to_arr)) => {
                let from_element_t = types.array(from_arr);
                let to_element_t = types.array(to_arr);
                self.add(from_element_t, to_element_t, types);
            }
            (Type::Object(from_obj), Type::Object(to_obj)) => {
                let from_members = types.object(from_obj).0.clone();
                let to_members = types.object(to_obj).0.clone();
                for (from_member_n, from_member_t) in from_members {
                    if let Some(to_member_t) = to_members.get(&from_member_n) {
                        self.add(from_member_t, *to_member_t, types);
                    }                    
                }
            }
            (Type::ConcreteObject(from_obj), Type::ConcreteObject(to_obj)) => {
                let from_members = types.concrete_object(from_obj).clone();
                let to_members = types.concrete_object(to_obj).clone();
                for ((_, from_member_t), (_, to_member_t)) in from_members.into_iter().zip(to_members.into_iter()) {
                    self.add(from_member_t, to_member_t, types);
                }
            }
            (Type::Closure(from_clo), Type::Closure(to_clo)) => {
                let (from_params, from_return_t) = types.closure(from_clo).clone();
                let (to_params, to_return_t) = types.closure(to_clo).clone();
                for (from_param_t, to_param_t) in from_params.into_iter().zip(to_params.into_iter()) {
                    self.add(from_param_t, to_param_t, types);
                }
                self.add(from_return_t, to_return_t, types);
            }
            (Type::Variants(from_var), Type::Variants(to_var)) => {
                let from_variants = types.variants(from_var).0.clone();
                let to_variants = types.variants(to_var).0.clone();
                for (from_variant_n, from_variant_t) in from_variants {
                    if let Some(to_variant_t) = to_variants.get(&from_variant_n) {
                        self.add(from_variant_t, *to_variant_t, types);
                    }
                }
            }
            // _ if from == to => {}
            // _ => { panic!("invalid mapping!"); }
            _ => {}
        }
    } 

    fn map(&mut self, group: TypeGroup, types: &mut TypeMap) -> TypeGroup {
        if let Some(g) = self.0.get(&types.group_internal_id(group)) { return *g; }
        let new_group = types.insert_group(&[]);
        self.0.insert(types.group_internal_id(group), new_group);
        let new_types = types.group(group).collect::<Vec<Type>>().into_iter()
            .map(|t| self.map_type(t, types)).collect::<Vec<Type>>();
        types.set_group_types(new_group, &new_types);
        new_group
    }

    fn map_type(&mut self, t: Type, types: &mut TypeMap) -> Type {
        match t {
            Type::Any => Type::Any,
            Type::Unit | Type::Boolean | Type::Integer | Type::Float | Type::String => t,
            Type::Array(arr) => {
                let element_types = self.map(types.array(arr), types);
                Type::Array(types.insert_array(element_types))
            }
            Type::Object(obj) => {
                let (member_types, fixed) = types.object(obj).clone();
                let member_types = member_types.into_iter()
                    .map(|(mn, mt)| (mn, self.map(mt, types))).collect();
                Type::Object(types.insert_object(member_types, fixed))
            }
            Type::ConcreteObject(obj) => {
                let member_types = types.concrete_object(obj).clone();
                let member_types = member_types.into_iter()
                    .map(|(mn, mt)| (mn, self.map(mt, types))).collect();
                Type::ConcreteObject(types.insert_concrete_object(member_types))
            }
            Type::Closure(clo) => {
                let (param_types, return_type) = types.closure(clo).clone();
                let param_types = param_types.into_iter().map(|pt| self.map(pt, types)).collect();
                let return_type = self.map(return_type, types);
                Type::Closure(types.insert_closure(param_types, return_type))
            }
            Type::Variants(var) => {
                let (variant_types, fixed) = types.variants(var).clone();
                let variant_types = variant_types.into_iter()
                    .map(|(vn, vt)| (vn, self.map(vt, types))).collect();
                Type::Variants(types.insert_variants(variant_types, fixed))
            }
        }
    }
}

pub fn lower_typed_ast(
    strings: &mut StringMap,
    types: &mut TypeMap,
    typed_symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
    external_backings: &HashMap<NamespacePath, StringIdx>,
    main_procedure: (&NamespacePath, &Symbol<TypedAstNode>)
) -> Result<(Vec<IrSymbol>, ConstantPool), Error> {
    let mut interpreter = Interpreter::new(strings);
    let mut ir_symbols = Vec::new();
    let mut constants = ConstantPool::new();
    if let Symbol::Procedure {
        public: _,
        parameter_names: _,
        parameter_types: _, returns,
        body, source: _
    } = main_procedure.1 {
        let mut generator = IrGenerator::new();
        let body = generator.lower_nodes(
            body.as_ref().expect("should not be external"),
            &HashMap::new(),
            types, &mut TypeMapping::new(), HashMap::new(), typed_symbols, strings,
            external_backings, &(HashMap::new(), Vec::new()), &mut interpreter, &mut ir_symbols,
            &mut constants
        )?;
        ir_symbols.push(IrSymbol::Procedure {
            path: main_procedure.0.clone(),
            variant: 0,
            parameter_types: Vec::new(), 
            return_type: *returns,
            variables: generator.variables.into_iter()
                .map(|v| v.1)
                .collect(),
            body
        });
    } else { panic!("should be a procedure"); }
    for (symbol_path, typed_symbol) in typed_symbols {
        match typed_symbol {
            Symbol::Constant { public: _, value, value_types } => {
                if let Some(value) = value {
                    let v = if let Some(value) = interpreter.get_constant_value(symbol_path) {
                        value.clone()
                    } else {
                        interpreter.evaluate_node(value, typed_symbols, external_backings, types, strings)?
                    };
                    let vl = IrSymbol::Variable {
                        path: symbol_path.clone(),
                        value_type: *value_types,
                        value: constants.insert(
                            &v, *value_types, types, &mut TypeMapping::new(), typed_symbols, strings,
                            external_backings, &mut interpreter, &mut ir_symbols
                        )?
                    };
                    ir_symbols.push(vl);
                } else {
                    ir_symbols.push(IrSymbol::ExternalVariable {
                        path: symbol_path.clone(),
                        backing: *external_backings.get(symbol_path).expect("should have backing"),
                        value_type: *value_types
                    });
                }
            }
            Symbol::Procedure { .. } => {
                // IrGenerator will lower needed procedures
            }
        }
    }
    Ok((ir_symbols, constants))
}


pub struct IrGenerator {
    instructions: Vec<Vec<IrInstruction>>,
    variables: Vec<(usize, TypeGroup)>,
    // reusable: Vec<usize>
}

impl IrGenerator {
    pub fn new() -> IrGenerator {
        IrGenerator { 
            instructions: Vec::new(),
            variables: Vec::new(),
            // reusable: Vec::new()
        }
    }

    pub fn variable_types(&self) -> Vec<TypeGroup> {
        return self.variables.iter()
            .map(|v| v.1)
            .collect();
    }

    pub fn lower_nodes(
        &mut self,
        nodes: &[TypedAstNode],
        captured: &HashMap<StringIdx, TypeGroup>,
        types: &mut TypeMap,
        type_mapping: &mut TypeMapping,
        mut named_variables: HashMap<StringIdx, usize>,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &mut StringMap,
        external_backings: &HashMap<NamespacePath, StringIdx>,
        call_parameters: &(HashMap<StringIdx, usize>, Vec<TypeGroup>),
        interpreter: &mut Interpreter,
        ir_symbols: &mut Vec<IrSymbol>,
        constants: &mut ConstantPool
    ) -> Result<Vec<IrInstruction>, Error> {
        self.enter();
        for node in nodes {
            self.lower_node(
                node, None, captured, types, type_mapping, &mut named_variables,
                symbols, strings, external_backings, call_parameters, interpreter,
                ir_symbols, constants
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
        type_mapping: &mut TypeMapping,
        call_parameter_types: Vec<TypeGroup>,
        call_return_type: TypeGroup,
        parameter_names: &Vec<StringIdx>,
        body: &Option<Vec<TypedAstNode>>,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &mut StringMap,
        external_backings: &HashMap<NamespacePath, StringIdx>,
        interpreter: &mut Interpreter,
        ir_symbols: &mut Vec<IrSymbol>,
        constants: &mut ConstantPool
    ) -> Result<usize, Error> {
        println!(
            "looking up {}({}) -> {}",
            path.display(strings),
            call_parameter_types.iter().map(|t| types.display_types(strings, *t)).collect::<Vec<String>>().join(", "),
            types.display_types(strings, call_return_type)
        );
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
                    path: proc_path, variant: proc_variant, parameter_types, return_type, ..
                } | IrSymbol::BuiltInProcedure {
                    path: proc_path, variant: proc_variant, parameter_types, return_type,
                } => {
                    if path != proc_path { continue; }
                    println!(
                        "found {}({}) -> {}",
                        path.display(strings),
                        parameter_types.iter().map(|t| types.display_types(strings, *t)).collect::<Vec<String>>().join(", "),
                        types.display_types(strings, *return_type)
                    );
                    found_variant = found_variant.max(*proc_variant + 1);
                    if parameter_types.len() != call_parameter_types.len() { continue; }
                    let mut params_eq = true;
                    let mut temp_types = types.clone();
                    for param_idx in 0..parameter_types.len() {
                        if temp_types.try_merge_groups(
                            call_parameter_types[param_idx], parameter_types[param_idx]
                        ) { continue; }
                        params_eq = false;
                        break;
                    }
                    if !params_eq { continue; }
                    if !temp_types.try_merge_groups(call_return_type, *return_type) { continue; }
                    found_variant = *proc_variant;
                    exists = true;
                    for param_idx in 0..parameter_types.len() {
                        types.try_merge_groups(
                            call_parameter_types[param_idx], parameter_types[param_idx]
                        );
                    }
                    types.try_merge_groups(call_return_type, *return_type);
                    break;
                }
                _ => {}
            }
        }
        if !exists {
            if path.display(strings) == "std::coll::Vector::new" && found_variant != 0 {
                panic!("already exists, but didn't reuse existing variant!");
            }
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
                        body: Vec::new()
                    });
                    let new_body = generator.lower_nodes(
                        body, &HashMap::new(),
                        types, type_mapping, HashMap::new(), symbols, strings,
                        external_backings, &call_parameters, interpreter, 
                        ir_symbols, constants
                    )?;
                    if let IrSymbol::Procedure { body, variables, .. }
                        = &mut ir_symbols[ir_symbol] {
                        *body = new_body;
                        *variables = generator.variables.iter()
                            .map(|v| v.1)
                            .collect();
                    }
                } else {
                    panic!("procedure should exist");
                }
            } else if let Some(backing) = external_backings.get(path) {
                ir_symbols.push(IrSymbol::ExternalProcedure {
                    path: path.clone(),
                    backing: *backing, 
                    parameter_types: call_parameter_types,
                    return_type: call_return_type
                });
            } else {
                ir_symbols.push(IrSymbol::BuiltInProcedure {
                    path: path.clone(),
                    variant: found_variant,
                    parameter_types: call_parameter_types,
                    return_type: call_return_type
                });
            }
        }
        Ok(found_variant)
    }

    fn lower_node(
        &mut self,
        node: &TypedAstNode,
        into: Option<IrVariable>,
        captured: &HashMap<StringIdx, TypeGroup>,
        types: &mut TypeMap,
        type_mapping: &mut TypeMapping,
        named_variables: &mut HashMap<StringIdx, usize>,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &mut StringMap,
        external_backings: &HashMap<NamespacePath, StringIdx>,
        call_parameters: &(HashMap<StringIdx, usize>, Vec<TypeGroup>),
        interpreter: &mut Interpreter,
        ir_symbols: &mut Vec<IrSymbol>,
        constants: &mut ConstantPool
    ) -> Result<Option<IrVariable>, Error> {
        macro_rules! lower_node { ($node: expr, $into: expr) => {
            self.lower_node(
                $node, $into, captured, types, type_mapping, named_variables,
                symbols, strings, external_backings, call_parameters, interpreter, ir_symbols,
                constants
            )?.expect("should result in a value")
        } }
        macro_rules! into_given_or_alloc { ($temp_type: expr) => {
            into.unwrap_or_else(|| self.allocate(type_mapping.map($temp_type, types)))
        } }
        match node.node_variant() {
            AstNodeVariant::Function { arguments, captures: body_captures, body } => {
                let mapped_node_types = type_mapping.map(node.get_types(), types);
                let (parameter_types, return_type) =
                    if let Type::Closure(clo) = types.group_concrete(mapped_node_types) {
                    let t = types.closure(clo);
                    (t.0.clone(), t.1)
                } else { panic!("should be a closure!"); };
                let mut body_captured = HashMap::new();
                for capture_name in body_captures.as_ref().expect("captures should be known by now") {
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
                        let into = self.allocate(type_mapping.map(
                            *captured.get(capture_name)
                                .expect("variable should be captured"), types
                        ));
                        self.add(IrInstruction::GetClosureCapture { name: *capture_name, into });
                        body_captured.insert(*capture_name, into);
                    }
                }
                let mut generator = IrGenerator::new();
                let mut parameters = (HashMap::new(), Vec::new());
                for param_idx in 0..parameter_types.len() {
                    parameters.0.insert(arguments[param_idx].0, param_idx);
                    parameters.1.push(type_mapping.map(parameter_types[param_idx], types));
                }
                let body = generator.lower_nodes(
                    body, &body_captured.iter().map(|(cn, cv)| (*cn, self.variables[cv.index].1)).collect(),
                    types, type_mapping, HashMap::new(), symbols, strings,
                    external_backings, &parameters, interpreter, ir_symbols,
                    constants
                )?;
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::LoadClosure {
                    parameter_types: parameters.1,
                    return_type: type_mapping.map(return_type, types),
                    captured: body_captured,
                    variables: generator.variable_types(),
                    body,
                    into
                });
                Ok(Some(into))
            }
            AstNodeVariant::Variable { public: _, mutable: _, name, value_types, value } => {
                let var = self.allocate(type_mapping.map(
                    value_types.expect("should have type info"), types
                ));
                named_variables.insert(*name, var.index);
                if let Some(value) = value {
                    lower_node!(&*value, Some(var));
                    Ok(Some(var))
                } else {
                    Ok(None)
                }
            }
            AstNodeVariant::CaseBranches { value, branches: branch_nodes, else_body } => {
                let value_type = value.get_types();
                let value = lower_node!(value, None);
                let mut branches = Vec::new();
                let mut branch_scopes = Vec::new();
                for branch in branch_nodes {
                    let branch_value = interpreter.evaluate_node(
                        &branch.0, symbols, external_backings, types, strings
                    )?;
                    let branch_body = self.lower_nodes(
                        &branch.1, captured, types, type_mapping,
                        named_variables.clone(), symbols, strings, external_backings,
                        call_parameters, interpreter, ir_symbols, constants
                    )?;
                    branches.push((constants.insert(
                        &branch_value, value_type, types, type_mapping, symbols, strings,
                        external_backings, interpreter, ir_symbols
                    )?, branch_body));
                    branch_scopes.push(self.variables.iter().map(|v| v.0).collect());
                }
                let else_branch = self.lower_nodes(
                    &else_body, captured, types, type_mapping,
                    named_variables.clone(), symbols, strings, external_backings, call_parameters,
                    interpreter, ir_symbols, constants
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
                    (ConstantValue::Boolean(true), self.lower_nodes(
                        &body, captured, types, type_mapping,
                        named_variables.clone(), symbols, strings, external_backings,
                        call_parameters, interpreter, ir_symbols, constants
                    )?)
                ];
                branch_scopes.push(self.variables.iter().map(|v| v.0).collect());
                let else_branch = self.lower_nodes(
                    &else_body, captured, types, type_mapping,
                    named_variables.clone(), symbols, strings, external_backings, call_parameters,
                    interpreter, ir_symbols, constants
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
                        let variant_var = self.allocate(type_mapping.map(variant_val_type, types));
                        branch_variables.insert(*branch_var_variable, variant_var.index);
                        Some(variant_var)
                    } else { None };
                    let branch_body = self.lower_nodes(
                        &branch.2, captured, types, type_mapping,
                        branch_variables, symbols, strings, external_backings, call_parameters,
                        interpreter, ir_symbols, constants
                    )?;
                    branches.push((branch.0, branch_variant_variable, branch_body));
                    branch_scopes.push(self.variables.iter().map(|v| v.0).collect());
                }
                let else_branch = if let Some(else_body) = else_body {
                    self.lower_nodes(
                        &else_body, captured, types, type_mapping,
                        named_variables.clone(), symbols, strings, external_backings,
                        call_parameters, interpreter, ir_symbols, constants
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
                        public: _, parameter_names, parameter_types, returns, body, source: _
                    } = symbols.get(path).expect("symbol should exist") {
                        let mut call_type_mapping = TypeMapping::new();
                        let mut call_parameter_types = Vec::new(); 
                        let mut parameter_values = Vec::new();
                        for argument_idx in 0..arguments.len() {
                            let dup_symbol_param_type = types.duplicate_group(parameter_types[argument_idx]);
                            let expected_param_type = type_mapping.map(arguments[argument_idx].get_types(), types);
                            types.try_merge_groups(expected_param_type, dup_symbol_param_type);
                            call_type_mapping.add(parameter_types[argument_idx], expected_param_type, types);
                            call_parameter_types.push(expected_param_type);
                            parameter_values.push(
                                lower_node!(&arguments[argument_idx], None)
                            );
                        }
                        let dup_symbol_return_type = types.duplicate_group(*returns);
                        let expected_return_type = type_mapping.map(node.get_types(), types);
                        types.try_merge_groups(expected_return_type, dup_symbol_return_type);
                        call_type_mapping.add(*returns, expected_return_type, types);
                        let proc_variant = IrGenerator::find_procedure(
                            path, types, &mut call_type_mapping, call_parameter_types,
                            expected_return_type, parameter_names, body, symbols, strings,
                            external_backings, interpreter, ir_symbols, constants
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
                        *captured.get(name)
                            .expect("variable should be captured")
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
                        (ConstantValue::Boolean(false), b_body)
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
                        (ConstantValue::Boolean(true), b_body)
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
                let value_type = value.get_types();
                let v = interpreter.evaluate_node(&*value, symbols, external_backings, types, strings)?;
                let into = into_given_or_alloc!(node.get_types());
                self.add(IrInstruction::LoadValue {
                    value: constants.insert(
                        &v, value_type, types, type_mapping, symbols, strings, external_backings,
                        interpreter, ir_symbols
                    )?,
                    into
                });
                Ok(Some(into))
            }
            _ => panic!("this node type should not be in the AST at this point")
        }
    }

}