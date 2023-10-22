
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
    interpreter::Interpreter
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
    external_backings: &HashMap<NamespacePath, StringIdx>
) -> Result<(Vec<IrSymbol>, IrTypeBank), Error> {
    let mut interpreter = Interpreter::new();
    let mut type_bank = IrTypeBank::new();
    let mut ir_symbols = Vec::new();
    todo!("start with lowering the main procedure");
    Ok((ir_symbols, type_bank))
}


struct IrGenerator {
    instructions: Vec<Vec<IrInstruction>>,
    variables: Vec<(usize, IrType)>,
    reusable: Vec<usize>,
    named_variables: HashMap<StringIdx, IrVariable>,
}

impl IrGenerator {
    fn new() -> IrGenerator {
        IrGenerator { 
            instructions: vec![],
            variables: Vec::new(),
            reusable: Vec::new(),
            named_variables: HashMap::new()
        }
    }

    fn lower_nodes(
        &mut self,
        nodes: &[TypedAstNode],
        type_scope: &TypeScope,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &StringMap,
        interpreter: &mut Interpreter,
        type_bank: &mut IrTypeBank
    ) -> Result<Vec<IrInstruction>, Error> {
        self.enter();
        for node in nodes {
            let r = self.allocate(possible_types_to_ir_type(type_scope, node.get_types(), type_bank));
            self.mark_as_reusable(r);
            self.lower_node(node, r, type_scope, symbols, strings, interpreter, type_bank)?;
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
        for reusable_entry_idx in 0..self.reusable.len() {
            let variable_idx = self.reusable[reusable_entry_idx];
            let variable = &mut self.variables[variable_idx];
            if variable.1 != variable_type { continue; }
            self.reusable.remove(reusable_entry_idx);
            variable.0 += 1;
            return IrVariable {
                index: variable_idx,
                version: 0,
                variable_type
            }
        }
        let variable_idx = self.variables.len();
        self.variables.push((0, variable_type));
        return IrVariable {
            index: variable_idx,
            version: 0,
            variable_type
        }
    }

    fn mark_as_reusable(&mut self, variable: IrVariable) {
        self.reusable.push(variable.index);
    }

    fn lower_node(
        &mut self,
        node: &TypedAstNode,
        into: IrVariable,
        type_scope: &TypeScope,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &StringMap,
        interpreter: &mut Interpreter,
        type_bank: &mut IrTypeBank
    ) -> Result<(), Error> {
        macro_rules! lower_node_reusable { ($node: expr) => { {
            let r = self.allocate(possible_types_to_ir_type(type_scope, $node.get_types(), type_bank));
            self.mark_as_reusable(r);
            self.lower_node($node, r, type_scope, symbols, strings, interpreter, type_bank)?;
            r
        } }; }
        match node.node_variant() {
            AstNodeVariant::Function { arguments, body } => {
                todo!("generate ir for closures")
            }
            AstNodeVariant::Variable { public: _, mutable: _, name, value } => {
                if let Some(value) = value {
                    let v = self.allocate(possible_types_to_ir_type(type_scope, value.get_types(), type_bank));
                    self.named_variables.insert(*name, v);
                    self.lower_node(&*value, v, type_scope, symbols, strings, interpreter, type_bank)?;
                }
            }
            AstNodeVariant::CaseBranches { value, branches, else_body } => todo!(),
            AstNodeVariant::CaseConditon { condition, body, else_body } => todo!(),
            AstNodeVariant::CaseVariant { value, branches, else_body } => todo!(),
            AstNodeVariant::Assignment { variable, value } => todo!(),
            AstNodeVariant::Return { value } => todo!(),
            AstNodeVariant::Call { called, arguments } => todo!(),
            AstNodeVariant::Object { values } => todo!(),
            AstNodeVariant::Array { values } => todo!(),
            AstNodeVariant::ObjectAccess { object, member } => todo!(),
            AstNodeVariant::ArrayAccess { array, index } => todo!(),
            AstNodeVariant::VariableAccess { name } => todo!(),
            AstNodeVariant::BooleanLiteral { value } => todo!(),
            AstNodeVariant::IntegerLiteral { value } => todo!(),
            AstNodeVariant::FloatLiteral { value } => todo!(),
            AstNodeVariant::StringLiteral { value } => todo!(),
            AstNodeVariant::UnitLiteral => todo!(),
            AstNodeVariant::Add { a, b } => {
                let a = lower_node_reusable!(&*a);
                let b = lower_node_reusable!(&*b);
                self.add(IrInstruction::Add { a, b, into });
            }
            AstNodeVariant::Subtract { a, b } => {
                let a = lower_node_reusable!(&*a);
                let b = lower_node_reusable!(&*b);
                self.add(IrInstruction::Subtract { a, b, into });
            }
            AstNodeVariant::Multiply { a, b } => {
                let a = lower_node_reusable!(&*a);
                let b = lower_node_reusable!(&*b);
                self.add(IrInstruction::Multiply { a, b, into });
            }
            AstNodeVariant::Divide { a, b } => {
                let a = lower_node_reusable!(&*a);
                let b = lower_node_reusable!(&*b);
                self.add(IrInstruction::Divide { a, b, into });
            }
            AstNodeVariant::Modulo { a, b } => {
                let a = lower_node_reusable!(&*a);
                let b = lower_node_reusable!(&*b);
                self.add(IrInstruction::Modulo { a, b, into });
            }
            AstNodeVariant::Negate { x } => {
                let x = lower_node_reusable!(&*x);
                self.add(IrInstruction::Negate { x, into });
            }
            AstNodeVariant::LessThan { a, b } => {
                let a = lower_node_reusable!(&*a);
                let b = lower_node_reusable!(&*b);
                self.add(IrInstruction::LessThan { a, b, into });
            }
            AstNodeVariant::GreaterThan { a, b } => {
                let a = lower_node_reusable!(&*a);
                let b = lower_node_reusable!(&*b);
                self.add(IrInstruction::GreaterThan { a, b, into });
            }
            AstNodeVariant::LessThanEqual { a, b } => {
                let a = lower_node_reusable!(&*a);
                let b = lower_node_reusable!(&*b);
                self.add(IrInstruction::LessThanEquals { a, b, into });
            }
            AstNodeVariant::GreaterThanEqual { a, b } => {
                let a = lower_node_reusable!(&*a);
                let b = lower_node_reusable!(&*b);
                self.add(IrInstruction::GreaterThanEquals { a, b, into });
            }
            AstNodeVariant::Equals { a, b } => {
                let a = lower_node_reusable!(&*a);
                let b = lower_node_reusable!(&*b);
                self.add(IrInstruction::Equals { a, b, into });
            }
            AstNodeVariant::NotEquals { a, b } => {
                let a = lower_node_reusable!(&*a);
                let b = lower_node_reusable!(&*b);
                self.add(IrInstruction::NotEquals { a, b, into });
            }
            AstNodeVariant::Not { x } => {
                let x = lower_node_reusable!(&*x);
                self.add(IrInstruction::Not { x, into });
            }
            AstNodeVariant::Or { a, b } => todo!(),
            AstNodeVariant::And { a, b } => todo!(),
            AstNodeVariant::ModuleAccess { path } => todo!(),
            AstNodeVariant::Variant { name, value } => {
                let v = lower_node_reusable!(&*value);
                self.add(IrInstruction::LoadVariant { name: *name, v, into })
            }
            _ => panic!("node type should not be in the AST at this point")
        }
        Ok(())
    }

}