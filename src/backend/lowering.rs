
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
    ir::{IrInstruction, IrVariable, IrType, IrTypeBank, IrSymbol},
    interpreter::{Interpreter, Value}
};


fn possible_types_to_ir_type(
    type_scope: &TypeScope,
    converted: &PossibleTypes,
    type_bank: &mut IrTypeBank
) -> IrType {
    match converted {
        PossibleTypes::Any => {
            panic!("possible types should be concrete!");
        }
        PossibleTypes::OfGroup(group_idx) => {
            possible_types_to_ir_type(type_scope, type_scope.get_group_types(group_idx), type_bank)
        }
        PossibleTypes::OneOf(possible_types) => {
            if possible_types.len() != 1 {
                panic!("possible types should be concrete!");
            }
            type_to_ir_type(type_scope, &possible_types[0], type_bank)
        }
    }
}

fn type_to_ir_type(
    type_scope: &TypeScope,
    converted: &Type,
    type_bank: &mut IrTypeBank
) -> IrType {
    match converted {
        Type::Unit => IrType::Unit,
        Type::Boolean => IrType::Boolean,
        Type::Integer => IrType::Integer,
        Type::Float => IrType::Float,
        Type::String => IrType::String,
        Type::Array(element_types) => {
            let element_type = possible_types_to_ir_type(type_scope, element_types, type_bank);
            IrType::Array(type_bank.insert_array(element_type))
        }
        Type::Object(member_types, _) => {
            let member_types = member_types.iter().map(|(member_name, member_types)| {
                (*member_name, possible_types_to_ir_type(type_scope, member_types, type_bank))
            }).collect();
            IrType::Object(type_bank.insert_object(member_types))
        }
        Type::ConcreteObject(members) => {
            let members = members.iter().map(|(member_name, member_type)| {
                (*member_name, type_to_ir_type(type_scope, member_type, type_bank))
            }).collect();
            IrType::ConcreteObject(type_bank.insert_concrete_object(members))
        }
        Type::Closure(_, _) => {
            todo!("convert closure type");
        }
        Type::Variants(variant_types, _) => {
            let variant_types = variant_types.iter().map(|(variant_name, variant_types)| {
                (*variant_name, possible_types_to_ir_type(type_scope, variant_types, type_bank))
            }).collect();
            IrType::Variants(type_bank.insert_variants(variant_types))
        }
    }
}


pub fn lower_typed_ast(
    strings: &StringMap,
    typed_symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
    external_backings: &HashMap<NamespacePath, StringIdx>,
    main_procedure: (&NamespacePath, &Symbol<TypedAstNode>)
) -> Result<(Vec<IrSymbol>, IrTypeBank), Error> {
    let mut interpreter = Interpreter::new();
    let mut type_bank = IrTypeBank::new();
    let mut ir_symbols = Vec::new();
    if let Symbol::Procedure {
        parameter_names, scope,
        parameter_types: _, returns,
        body
    } = main_procedure.1 {
        let mut generator = IrGenerator::new();
        let body = generator.lower_nodes(
            body.as_ref().expect("should not be external"),
            scope, typed_symbols, strings, external_backings, parameter_names,
            &mut interpreter, &mut type_bank, &mut ir_symbols
        )?;
        ir_symbols.push(IrSymbol::Procedure {
            path: main_procedure.0.clone(),
            variant: 0,
            parameter_types: Vec::new(), 
            return_type: possible_types_to_ir_type(
                scope,
                &PossibleTypes::OfGroup(*returns),
                &mut type_bank
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
                            &TypeScope::new(), value_types, &mut type_bank
                        ),
                        value: v
                    });
                } else {
                    ir_symbols.push(IrSymbol::ExternalVariable {
                        path: symbol_path.clone(),
                        backing: *external_backings.get(symbol_path).expect("should have backing"),
                        value_type: possible_types_to_ir_type(
                            &TypeScope::new(), value_types, &mut type_bank
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
    named_variables: HashMap<StringIdx, IrVariable>,
}

impl IrGenerator {
    fn new() -> IrGenerator {
        IrGenerator { 
            instructions: vec![],
            variables: Vec::new(),
            // reusable: Vec::new(),
            named_variables: HashMap::new()
        }
    }

    fn lower_nodes(
        &mut self,
        nodes: &[TypedAstNode],
        type_scope: &TypeScope,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &StringMap,
        external_backings: &HashMap<NamespacePath, StringIdx>,
        parameter_names: &Vec<StringIdx>,
        interpreter: &mut Interpreter,
        type_bank: &mut IrTypeBank,
        ir_symbols: &mut Vec<IrSymbol>
    ) -> Result<Vec<IrInstruction>, Error> {
        self.enter();
        for node in nodes {
            self.lower_node(
                node, None, type_scope, symbols, strings, external_backings, parameter_names,
                interpreter, type_bank, ir_symbols
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
            version: 0,
            variable_type
        }
    }

    // fn allocate_temporary(&mut self, variable_type: IrType) -> IrVariable {
    //     let v = self.allocate(variable_type);
    //     self.reusable.push(v.index);
    //     v
    // }

    fn lower_node(
        &mut self,
        node: &TypedAstNode,
        into: Option<IrVariable>,
        type_scope: &TypeScope,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &StringMap,
        external_backings: &HashMap<NamespacePath, StringIdx>,
        parameter_names: &Vec<StringIdx>,
        interpreter: &mut Interpreter,
        type_bank: &mut IrTypeBank,
        ir_symbols: &mut Vec<IrSymbol>
    ) -> Result<Option<IrVariable>, Error> {
        macro_rules! lower_node { ($node: expr, $into: expr) => {
            self.lower_node(
                $node, $into, type_scope, symbols, strings, external_backings, parameter_names,
                interpreter, type_bank, ir_symbols
            )?.expect("should result in a value")
        } }
        macro_rules! node_type { () => {
            possible_types_to_ir_type(type_scope, node.get_types(), type_bank)
        } }
        macro_rules! into_given_or_alloc { ($temp_type: expr) => {
            into.unwrap_or_else(|| self.allocate($temp_type))
        } }
        match node.node_variant() {
            AstNodeVariant::Function { arguments, body } => {
                todo!("generate IR for closures")
            }
            AstNodeVariant::Variable { public: _, mutable: _, name, value } => {
                if let Some(value) = value {
                    let var = self.allocate(possible_types_to_ir_type(type_scope, value.get_types(), type_bank));
                    lower_node!(&*value, Some(var));
                    self.named_variables.insert(*name, var);
                    Ok(Some(var))
                } else {
                    Ok(None)
                }
            }
            AstNodeVariant::CaseBranches { value, branches, else_body } => {
                todo!("generate IR for branching 'case'")
            }
            AstNodeVariant::CaseConditon { condition, body, else_body } => {
                todo!("generate IR for conditional 'case'");
            }
            AstNodeVariant::CaseVariant { value, branches, else_body } => {
                todo!("generate IR for variant 'case'");
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
                        let var = self.named_variables.get_mut(name).expect("variable should be registered");
                        var.version += 1;
                        let var = *var;
                        lower_node!(&*value, Some(var));
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
                                &inserted_proc_scope.translate(
                                    &PossibleTypes::OfGroup(parameter_types[argument_idx])
                                ),
                                arguments[argument_idx].get_types()
                            ).expect("should have a possible value");
                            let param_ir_type = possible_types_to_ir_type(
                                &call_type_scope, &concrete_param_type, type_bank
                            );
                            parameter_ir_types.push(param_ir_type);
                            parameter_values.push(
                                lower_node!(&arguments[argument_idx], None)
                            );
                        }
                        let return_ir_type = possible_types_to_ir_type(
                            &call_type_scope,
                            &inserted_proc_scope.translate(&PossibleTypes::OfGroup(*returns)),
                            type_bank
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
                            let body = generator.lower_nodes(
                                body.as_ref().expect("should not be external"),
                                &call_type_scope, symbols, strings, external_backings, parameter_names,
                                interpreter, type_bank, ir_symbols
                            )?;
                            ir_symbols.push(IrSymbol::Procedure {
                                path: path.clone(),
                                variant: proc_variant,
                                parameter_types: parameter_ir_types,
                                return_type: return_ir_type,
                                variables: generator.variables.into_iter()
                                    .map(|v| v.1)
                                    .collect(),
                                body
                            });
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
                todo!("generate IR for closure calls")
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
                if parameter_names.contains(name) {
                    let into = into_given_or_alloc!(node_type!());
                    self.add(IrInstruction::LoadParameter { name: *name, into });
                    Ok(Some(into))
                } else {
                    let var = *self.named_variables.get(name).expect("variable should be registered");
                    if let Some(into) = into {
                        self.add(IrInstruction::Move { from: var, into });
                        Ok(Some(into))
                    } else {
                        Ok(Some(var))
                    }
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
                let into = into_given_or_alloc!(a.variable_type);
                self.add(IrInstruction::Add { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Subtract { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(a.variable_type);
                self.add(IrInstruction::Subtract { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Multiply { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(a.variable_type);
                self.add(IrInstruction::Multiply { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Divide { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(a.variable_type);
                self.add(IrInstruction::Divide { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Modulo { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(a.variable_type);
                self.add(IrInstruction::Modulo { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Negate { x } => {
                let x = lower_node!(&*x, None);
                let into = into_given_or_alloc!(x.variable_type);
                self.add(IrInstruction::Negate { x, into });
                Ok(Some(into))
            }
            AstNodeVariant::LessThan { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(a.variable_type);
                self.add(IrInstruction::LessThan { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::GreaterThan { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(a.variable_type);
                self.add(IrInstruction::GreaterThan { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::LessThanEqual { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(a.variable_type);
                self.add(IrInstruction::LessThanEquals { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::GreaterThanEqual { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(a.variable_type);
                self.add(IrInstruction::GreaterThanEquals { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Equals { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(a.variable_type);
                self.add(IrInstruction::Equals { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::NotEquals { a, b } => {
                let a = lower_node!(&*a, None);
                let b = lower_node!(&*b, None);
                let into = into_given_or_alloc!(a.variable_type);
                self.add(IrInstruction::NotEquals { a, b, into });
                Ok(Some(into))
            }
            AstNodeVariant::Not { x } => {
                let x = lower_node!(&*x, None);
                let into = into_given_or_alloc!(x.variable_type);
                self.add(IrInstruction::Not { x, into });
                Ok(Some(into))
            }
            AstNodeVariant::Or { a, b } => {
                todo!("generate IR for logical OR");
            }
            AstNodeVariant::And { a, b } => {
                todo!("generate IR for logical AND");
            }
            AstNodeVariant::ModuleAccess { path } => {
                let into = into_given_or_alloc!(node_type!());
                self.add(IrInstruction::LoadGlobalVariable { path: path.clone(), into });
                Ok(Some(into))
            }
            AstNodeVariant::Variant { name, value } => {
                let v = lower_node!(&*value, None);
                let into = into_given_or_alloc!(v.variable_type);
                self.add(IrInstruction::LoadVariant { name: *name, v, into });
                Ok(Some(into))
            }
            _ => panic!("this node type should not be in the AST at this point")
        }
    }

}