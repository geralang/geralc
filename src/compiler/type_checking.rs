
use std::collections::HashMap;

use crate::util::{
    strings::{StringMap, StringIdx},
    error::{Error, ErrorSection, ErrorType},
    source::HasSource
};

use crate::compiler::{
    ast::{TypedAstNode, AstNode, HasAstNodeVariant, AstNodeVariant},
    types::{TypeScope, PossibleTypes, VarTypeIdx, Type},
    modules::{NamespacePath, Module}
};


pub fn type_check_modules(modules: HashMap<NamespacePath, Module<AstNode>>, strings: &StringMap) -> Result<(TypeScope, Vec<TypedAstNode>, HashMap<NamespacePath, usize>), Vec<Error>> {
    todo!("check modules")
}

fn type_check_nodes(strings: &StringMap, type_scope: &mut TypeScope, variables: &mut HashMap<StringIdx, VarTypeIdx>, mut nodes: Vec<AstNode>, limited_to: &PossibleTypes) -> Result<Vec<TypedAstNode>, Error> {
    let mut typed_nodes = Vec::new();
    while nodes.len() > 0 {
        match type_check_node(strings, type_scope, variables, nodes.remove(0), limited_to) {
            Ok(typed_node) => typed_nodes.push(typed_node),
            Err(error) => return Err(error)
        }
    }
    Ok(typed_nodes)
}

fn type_check_node(strings: &StringMap, type_scope: &mut TypeScope, variables: &mut HashMap<StringIdx, VarTypeIdx>, mut node: AstNode, limited_to: &PossibleTypes) -> Result<TypedAstNode, Error> {
    macro_rules! type_check_node { ($node: expr, $limited_to: expr) => {
        match type_check_node(strings, type_scope, variables, $node, $limited_to) {
            Ok(typed_node) => typed_node,
            Err(error) => return Err(error)
        }
    } }
    macro_rules! type_check_nodes { ($nodes: expr, $limited_to: expr) => {
        match type_check_nodes(strings, type_scope, variables, $nodes, $limited_to) {
            Ok(typed_node) => typed_node,
            Err(error) => return Err(error)
        }
    } }
    let node_source = node.source();
    match node.move_node() {
        AstNodeVariant::Procedure { public: _, name: _, arguments: _, body } => {
            todo!("type check procedure")
        }
        AstNodeVariant::Function { arguments: _, body } => {
            todo!("type check function")
        }
        AstNodeVariant::Variable { public, mutable, name: _, value } => {
            todo!("type check variable")
        }
        AstNodeVariant::CaseBranches { value, branches } => {
            todo!("type check branching case")
        }
        AstNodeVariant::CaseConditon { condition, body } => {
            todo!("type check conditional case")
        }
        AstNodeVariant::Assignment { variable, value } => {
            todo!("type check assignment")
        }
        AstNodeVariant::Return { value } => {
            todo!("type check return")
        }
        AstNodeVariant::Call { called, arguments } => {
            todo!("type check calls")
        }
        AstNodeVariant::Object { values } => {
            todo!("type check object literals")
        }
        AstNodeVariant::Array { values } => {
            todo!("type check array literals")
        }
        AstNodeVariant::ObjectAccess { object, member: _ } => {
            todo!("type check object accesses")
        }
        AstNodeVariant::ArrayAccess { array, index } => {
            todo!("type check array accesses")
        }
        AstNodeVariant::VariableAccess { name } => {
            if let Some(group_idx) = variables.get(&name) {
                Ok(TypedAstNode::new(
                    AstNodeVariant::VariableAccess { name },
                    PossibleTypes::OfGroup(*group_idx),
                    node_source
                ))
            } else {
                Err(Error::new([
                    ErrorSection::Error(ErrorType::VariableDoesNotExist(name)),
                    ErrorSection::Code(node_source)
                ].into()))
            }
        }
        AstNodeVariant::BooleanLiteral { value } => {
            Ok(TypedAstNode::new(
                AstNodeVariant::BooleanLiteral { value },
                PossibleTypes::OneOf(vec![Type::Boolean]),
                node_source
            ))
        }
        AstNodeVariant::IntegerLiteral { value } => {
            Ok(TypedAstNode::new(
                AstNodeVariant::IntegerLiteral { value },
                PossibleTypes::OneOf(vec![Type::Integer]),
                node_source
            ))
        }
        AstNodeVariant::FloatLiteral { value } => {
            Ok(TypedAstNode::new(
                AstNodeVariant::FloatLiteral { value },
                PossibleTypes::OneOf(vec![Type::Float]),
                node_source
            ))
        }
        AstNodeVariant::StringLiteral { value } => {
            Ok(TypedAstNode::new(
                AstNodeVariant::StringLiteral { value },
                PossibleTypes::OneOf(vec![Type::String]),
                node_source
            ))
        }
        AstNodeVariant::UnitLiteral => {
            Ok(TypedAstNode::new(
                AstNodeVariant::UnitLiteral,
                PossibleTypes::OneOf(vec![Type::Unit]),
                node_source
            ))
        }
        AstNodeVariant::Add { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Add {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::Subtract { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Subtract {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::Multiply { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Multiply {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::Divide { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Divide {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::Modulo { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Modulo {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::Negate { x } => {
            let x_typed = type_check_node!(*x, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let node_type = x_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Negate {
                x: Box::new(x_typed),
            }, node_type, node_source))
        }
        AstNodeVariant::LessThan { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, a_typed.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::LessThan {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::LessThanEqual { a , b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, a_typed.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::LessThanEqual {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::GreaterThan { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, a_typed.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::GreaterThan {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::GreaterThanEqual { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, a_typed.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::GreaterThanEqual {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::Equals { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::Any);
            let b_typed = type_check_node!(*b, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Equals {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::NotEquals { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::Any);
            let b_typed = type_check_node!(*b, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::NotEquals {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::And { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Boolean]));
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Boolean]));
            Ok(TypedAstNode::new(AstNodeVariant::And {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::Or { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Boolean]));
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Boolean]));
            Ok(TypedAstNode::new(AstNodeVariant::Or {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::Not { x } => {
            let x_typed = type_check_node!(*x, &PossibleTypes::OneOf(vec![Type::Boolean]));
            Ok(TypedAstNode::new(AstNodeVariant::Not {
                x: Box::new(x_typed),
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::Module { path } => {
            Ok(TypedAstNode::new(AstNodeVariant::Module {
                path
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source))
        }
        AstNodeVariant::ModuleAccess { path: _ } => {
            todo!("type check module access")
        }
        AstNodeVariant::Use { paths } => {
            Ok(TypedAstNode::new(AstNodeVariant::Use {
                paths
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source))
        }
    }
}