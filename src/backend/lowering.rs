
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
        parameter_names: _, scope,
        parameter_types: _, returns,
        body
    } = main_procedure.1 {
        let mut generator = IrGenerator::new();
        let body = generator.lower_nodes(
            body.as_ref().expect("should not be external"),
            scope, typed_symbols, strings, external_backings,
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
    Ok((ir_symbols, type_bank))
}


struct IrGenerator {
    instructions: Vec<Vec<IrInstruction>>,
    variables: Vec<(usize, IrType)>,
    //reusable: Vec<usize>,
    named_variables: HashMap<StringIdx, IrVariable>,
}

impl IrGenerator {
    fn new() -> IrGenerator {
        IrGenerator { 
            instructions: vec![],
            variables: Vec::new(),
            //reusable: Vec::new(),
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
        interpreter: &mut Interpreter,
        type_bank: &mut IrTypeBank,
        ir_symbols: &mut Vec<IrSymbol>
    ) -> Result<Vec<IrInstruction>, Error> {
        self.enter();
        for node in nodes {
            let r = self.allocate(possible_types_to_ir_type(type_scope, node.get_types(), type_bank));
            self.lower_node(node, r, type_scope, symbols, strings, external_backings, interpreter, type_bank, ir_symbols)?;
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

    // fn mark_as_reusable(&mut self, variable: IrVariable) {
    //     self.reusable.push(variable.index);
    // }

    fn lower_node(
        &mut self,
        node: &TypedAstNode,
        into: IrVariable,
        type_scope: &TypeScope,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &StringMap,
        external_backings: &HashMap<NamespacePath, StringIdx>,
        interpreter: &mut Interpreter,
        type_bank: &mut IrTypeBank,
        ir_symbols: &mut Vec<IrSymbol>
    ) -> Result<(), Error> {
        macro_rules! lower_node { ($node: expr) => { {
            let r = self.allocate(possible_types_to_ir_type(type_scope, $node.get_types(), type_bank));
            self.lower_node($node, r, type_scope, symbols, strings, external_backings, interpreter, type_bank, ir_symbols)?;
            r
        } }; }
        match node.node_variant() {
            AstNodeVariant::Function { arguments, body } => {
                todo!("generate IR for closures")
            }
            AstNodeVariant::Variable { public: _, mutable: _, name, value } => {
                if let Some(value) = value {
                    let v = self.allocate(possible_types_to_ir_type(type_scope, value.get_types(), type_bank));
                    self.named_variables.insert(*name, v);
                    self.lower_node(&*value, v, type_scope, symbols, strings, external_backings, interpreter, type_bank, ir_symbols)?;
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
                todo!("generate IR for assignments");
            }
            AstNodeVariant::Return { value } => {
                let value = lower_node!(&*value);
                self.add(IrInstruction::Return { value });
            }
            AstNodeVariant::Call { called, arguments } => todo!(),
            AstNodeVariant::Object { values } => {
                let mut member_values = HashMap::new();
                for (member_name, member_value) in values {
                    let member_value = lower_node!(member_value);
                    member_values.insert(*member_name, member_value);
                }
                self.add(IrInstruction::LoadObject { member_values, into });
            }
            AstNodeVariant::Array { values } => {
                let mut element_values = Vec::new();
                for value in values {
                    let value = lower_node!(value);
                    element_values.push(value);
                }
                self.add(IrInstruction::LoadArray { element_values, into });
            }
            AstNodeVariant::ObjectAccess { object, member } => {
                let accessed = lower_node!(&*object);
                self.add(IrInstruction::GetObjectMember { accessed, member: *member, into });
            }
            AstNodeVariant::ArrayAccess { array, index } => {
                let accessed = lower_node!(&*array);
                let index = lower_node!(&*index);
                self.add(IrInstruction::GetArrayElement { accessed, index, into });
            }
            AstNodeVariant::VariableAccess { name } => {
                todo!("generate IR for variable accesses");
            }
            AstNodeVariant::BooleanLiteral { value } => {
                self.add(IrInstruction::LoadBoolean { value: *value, into });
            }
            AstNodeVariant::IntegerLiteral { value } => {
                self.add(IrInstruction::LoadInteger { value: *value, into });
            }
            AstNodeVariant::FloatLiteral { value } => {
                self.add(IrInstruction::LoadFloat { value: *value, into });
            }
            AstNodeVariant::StringLiteral { value } => {
                self.add(IrInstruction::LoadString { value: *value, into });
            }
            AstNodeVariant::UnitLiteral => {
                self.add(IrInstruction::LoadUnit { into });
            }
            AstNodeVariant::Add { a, b } => {
                let a = lower_node!(&*a);
                let b = lower_node!(&*b);
                self.add(IrInstruction::Add { a, b, into });
            }
            AstNodeVariant::Subtract { a, b } => {
                let a = lower_node!(&*a);
                let b = lower_node!(&*b);
                self.add(IrInstruction::Subtract { a, b, into });
            }
            AstNodeVariant::Multiply { a, b } => {
                let a = lower_node!(&*a);
                let b = lower_node!(&*b);
                self.add(IrInstruction::Multiply { a, b, into });
            }
            AstNodeVariant::Divide { a, b } => {
                let a = lower_node!(&*a);
                let b = lower_node!(&*b);
                self.add(IrInstruction::Divide { a, b, into });
            }
            AstNodeVariant::Modulo { a, b } => {
                let a = lower_node!(&*a);
                let b = lower_node!(&*b);
                self.add(IrInstruction::Modulo { a, b, into });
            }
            AstNodeVariant::Negate { x } => {
                let x = lower_node!(&*x);
                self.add(IrInstruction::Negate { x, into });
            }
            AstNodeVariant::LessThan { a, b } => {
                let a = lower_node!(&*a);
                let b = lower_node!(&*b);
                self.add(IrInstruction::LessThan { a, b, into });
            }
            AstNodeVariant::GreaterThan { a, b } => {
                let a = lower_node!(&*a);
                let b = lower_node!(&*b);
                self.add(IrInstruction::GreaterThan { a, b, into });
            }
            AstNodeVariant::LessThanEqual { a, b } => {
                let a = lower_node!(&*a);
                let b = lower_node!(&*b);
                self.add(IrInstruction::LessThanEquals { a, b, into });
            }
            AstNodeVariant::GreaterThanEqual { a, b } => {
                let a = lower_node!(&*a);
                let b = lower_node!(&*b);
                self.add(IrInstruction::GreaterThanEquals { a, b, into });
            }
            AstNodeVariant::Equals { a, b } => {
                let a = lower_node!(&*a);
                let b = lower_node!(&*b);
                self.add(IrInstruction::Equals { a, b, into });
            }
            AstNodeVariant::NotEquals { a, b } => {
                let a = lower_node!(&*a);
                let b = lower_node!(&*b);
                self.add(IrInstruction::NotEquals { a, b, into });
            }
            AstNodeVariant::Not { x } => {
                let x = lower_node!(&*x);
                self.add(IrInstruction::Not { x, into });
            }
            AstNodeVariant::Or { a, b } => {
                todo!("generate IR for logical OR");
            }
            AstNodeVariant::And { a, b } => {
                todo!("generate IR for logical AND");
            }
            AstNodeVariant::ModuleAccess { path } => {
                todo!("generate IR for module accesses");
            }
            AstNodeVariant::Variant { name, value } => {
                let v = lower_node!(&*value);
                self.add(IrInstruction::LoadVariant { name: *name, v, into })
            }
            _ => panic!("this node type should not be in the AST at this point")
        }
        Ok(())
    }

}