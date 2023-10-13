
use std::collections::HashMap;

use crate::util::{
    strings::{StringMap, StringIdx},
    error::{Error, ErrorSection, ErrorType},
    source::HasSource
};

use crate::compiler::{
    ast::{TypedAstNode, AstNode, HasAstNodeVariant, AstNodeVariant},
    types::{TypeScope, PossibleTypes, Type, VarTypeIdx},
    modules::{NamespacePath, Module}
};


#[derive(Debug)]
pub enum Symbol<T: Clone + HasSource + HasAstNodeVariant<T>> {
    Constant { value: T },
    Procedure { scope: TypeScope, parameters: Vec<VarTypeIdx>, return_types: PossibleTypes, body: Vec<T> }
}

pub fn type_check_modules(modules: HashMap<NamespacePath, Module<AstNode>>, strings: &StringMap) -> Result<HashMap<NamespacePath, Symbol<TypedAstNode>>, Vec<Error>> {
    let mut global_scope = TypeScope::new();
    let mut symbols = HashMap::new();
    let mut errors = Vec::new();
    let mut old_symbols = HashMap::new();
    for (module_path, module) in modules {
        for (symbol_name, symbol_node) in module.symbols() {
            let mut symbol_path_segments = module_path.get_segments().clone();
            symbol_path_segments.push(symbol_name);
            old_symbols.insert(NamespacePath::new(symbol_path_segments), symbol_node);
        }
    }
    let old_symbol_paths = old_symbols.keys().map(|p| p.clone()).collect::<Vec<NamespacePath>>();
    for symbol_path in old_symbol_paths {
        if let Err(error) = type_check_symbol(
            &mut global_scope,
            &mut old_symbols,
            &mut symbols,
            &symbol_path
        ) { errors.push(error); }
    }
    if errors.len() > 0 { Err(errors) }
        else { Ok(symbols) }
}

fn type_check_symbol<'s>(
    type_scope: &mut TypeScope,
    untyped_symbols: &mut HashMap<NamespacePath, AstNode>,
    symbols: &'s mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    name: &NamespacePath
) -> Result<&'s Symbol<TypedAstNode>, Error> {
    if let Some(symbol) = untyped_symbols.remove(name) {
        match symbol.move_node() {
            AstNodeVariant::Procedure { public: _, name: _, arguments, body } => {
                let mut procedure_scope = TypeScope::new();
                let mut argument_vars = Vec::new();
                let mut procedure_variables = HashMap::new();
                for argument_idx in 0..arguments.len() {
                    let var_type_idx = procedure_scope.register_variable();
                    argument_vars.push(var_type_idx);
                    procedure_variables.insert(arguments[argument_idx], (PossibleTypes::OfGroup(var_type_idx), false));
                }
                let mut return_types = PossibleTypes::Any;
                let body_typed = match type_check_nodes(
                    &mut procedure_scope,
                    &mut procedure_variables,
                    untyped_symbols,
                    symbols,
                    body,
                    &mut return_types
                ) {
                    Ok(typed_nodes) => typed_nodes,
                    Err(error) => return Err(error),
                };
                symbols.insert(name.clone(), Symbol::Procedure {
                    scope: procedure_scope,
                    parameters: argument_vars,
                    return_types: return_types,
                    body: body_typed
                });
            }
            AstNodeVariant::Variable { public: _, mutable: _, name: _, value } => {
                let value_typed = match type_check_node(
                    type_scope,
                    &mut HashMap::new(),
                    untyped_symbols,
                    symbols,
                    *value,
                    &mut PossibleTypes::Any,
                    &PossibleTypes::Any,
                    false
                ) {
                    Ok(typed_node) => typed_node,
                    Err(error) => return Err(error),
                };
                symbols.insert(name.clone(), Symbol::Constant {
                    value: value_typed
                });
            }
            other => panic!("Unhandled symbol type checking for {:?}!", other)
        }
    }
    if let Some(symbol) = symbols.get(name) {
        return Ok(symbol);
    }
    // The symbol wasn't found!
    // The module system already enforced that the symbol exists.
    // This means that it's currently getting generated lower down on the stack.
    todo!("recursive symbol check :flushed:")
}

fn type_check_nodes(
    type_scope: &mut TypeScope,
    variables: &mut HashMap<StringIdx, (PossibleTypes, bool)>,
    untyped_symbols: &mut HashMap<NamespacePath, AstNode>,
    symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    mut nodes: Vec<AstNode>,
    return_types: &mut PossibleTypes
) -> Result<Vec<TypedAstNode>, Error> {
    let mut typed_nodes = Vec::new();
    while nodes.len() > 0 {
        match type_check_node(type_scope, variables, untyped_symbols, symbols, nodes.remove(0), return_types, &PossibleTypes::Any, false) {
            Ok(typed_node) => typed_nodes.push(typed_node),
            Err(error) => return Err(error)
        }
    }
    Ok(typed_nodes)
}

fn type_check_node(
    type_scope: &mut TypeScope,
    variables: &mut HashMap<StringIdx, (PossibleTypes, bool)>,
    untyped_symbols: &mut HashMap<NamespacePath, AstNode>,
    symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    node: AstNode,
    return_types: &mut PossibleTypes,
    limited_to: &PossibleTypes,
    assignment: bool
) -> Result<TypedAstNode, Error> {
    macro_rules! type_check_node { ($node: expr, $limited_to: expr) => {
        match type_check_node(type_scope, variables, untyped_symbols, symbols, $node, return_types, $limited_to, assignment) {
            Ok(typed_node) => typed_node,
            Err(error) => return Err(error)
        }
    }; ($node: expr, $limited_to: expr, $assignment: expr) => {
        match type_check_node(type_scope, variables, untyped_symbols, symbols, $node, return_types, $limited_to, $assignment) {
            Ok(typed_node) => typed_node,
            Err(error) => return Err(error)
        }
    } }
    macro_rules! type_check_nodes { ($nodes: expr) => {
        match type_check_nodes(type_scope, variables, untyped_symbols, symbols, $nodes, return_types) {
            Ok(typed_node) => typed_node,
            Err(error) => return Err(error)
        }
    } }
    macro_rules! limit { ($a: expr, $b: expr) => {
        match type_scope.limit_possible_types($a, $b) {
            Ok(result) => result,
            Err(error) => return Err(error),
        }
    } }
    macro_rules! limit_typed_node { ($node: expr, $limited_to: expr) => {
        if let Some(error) = limit_typed_node(type_scope, variables, $node, $limited_to) {
            return Err(error);
        }
    } }
    let node_source = node.source();
    match node.move_node() {
        AstNodeVariant::Procedure { public: _, name: _, arguments: _, body } => panic!("The grammar checker failed to see a procedure inside another!"),
        AstNodeVariant::Function { arguments: _, body } => {
            todo!("type check function")
        }
        AstNodeVariant::Variable { public, mutable, name, value } => {
            let typed_value = type_check_node!(*value, &PossibleTypes::Any);
            variables.insert(name, (typed_value.get_types().clone(), mutable));
            Ok(TypedAstNode::new(AstNodeVariant::Variable {
                public,
                mutable,
                name,
                value: Box::new(typed_value)
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source))
        }
        AstNodeVariant::CaseBranches { value, branches } => {
            let typed_value = type_check_node!(*value, &PossibleTypes::Any);
            let mut typed_branches = Vec::new();
            for (branch_value, branch_body) in branches {
                typed_branches.push((type_check_node!(branch_value, typed_value.get_types()), type_check_nodes!(branch_body)));
            }
            Ok(TypedAstNode::new(AstNodeVariant::CaseBranches {
                value: Box::new(typed_value),
                branches: typed_branches
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source))
        }
        AstNodeVariant::CaseConditon { condition, body } => {
            let typed_condition = type_check_node!(*condition, &PossibleTypes::OneOf(vec![Type::Boolean]));
            let typed_body = type_check_nodes!(body);
            Ok(TypedAstNode::new(AstNodeVariant::CaseConditon {
                condition: Box::new(typed_condition),
                body: typed_body
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source))
        }
        AstNodeVariant::Assignment { variable, value } => {
            let typed_value = type_check_node!(*value, &PossibleTypes::Any);
            let typed_variable = type_check_node!(*variable, typed_value.get_types(), true);
            Ok(TypedAstNode::new(AstNodeVariant::Assignment {
                variable: Box::new(typed_variable),
                value: Box::new(typed_value)
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source))
        }
        AstNodeVariant::Return { value } => {
            let typed_value = type_check_node!(match value {
                Some(value) => *value,
                None => AstNode::new(AstNodeVariant::UnitLiteral, node_source)
            }, &return_types.clone());
            *return_types = limit!(return_types, typed_value.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::Return {
                value: Some(Box::new(typed_value))
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source))
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
            if let Some((variable_types, variable_mutable)) = variables.get_mut(&name) {
                if assignment && !*variable_mutable {
                    Err(Error::new([
                        ErrorSection::Error(ErrorType::ImmutableAssignmant(name)),
                        ErrorSection::Code(node_source)
                    ].into()))
                } else {
                    *variable_types = limit!(&variable_types, limited_to);
                    Ok(TypedAstNode::new(
                        AstNodeVariant::VariableAccess { name },
                        variable_types.clone(),
                        node_source
                    ))
                }
            } else {
                Err(Error::new([
                    ErrorSection::Error(ErrorType::VariableDoesNotExist(name)),
                    ErrorSection::Code(node_source)
                ].into()))
            }
        }
        AstNodeVariant::BooleanLiteral { value } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Boolean]));
            Ok(TypedAstNode::new(
                AstNodeVariant::BooleanLiteral { value },
                PossibleTypes::OneOf(vec![Type::Boolean]),
                node_source
            ))
        }
        AstNodeVariant::IntegerLiteral { value } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer]));
            Ok(TypedAstNode::new(
                AstNodeVariant::IntegerLiteral { value },
                PossibleTypes::OneOf(vec![Type::Integer]),
                node_source
            ))
        }
        AstNodeVariant::FloatLiteral { value } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Float]));
            Ok(TypedAstNode::new(
                AstNodeVariant::FloatLiteral { value },
                PossibleTypes::OneOf(vec![Type::Float]),
                node_source
            ))
        }
        AstNodeVariant::StringLiteral { value } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::String]));
            Ok(TypedAstNode::new(
                AstNodeVariant::StringLiteral { value },
                PossibleTypes::OneOf(vec![Type::String]),
                node_source
            ))
        }
        AstNodeVariant::UnitLiteral => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Unit]));
            Ok(TypedAstNode::new(
                AstNodeVariant::UnitLiteral,
                PossibleTypes::OneOf(vec![Type::Unit]),
                node_source
            ))
        }
        AstNodeVariant::Add { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Add {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::Subtract { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Subtract {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::Multiply { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Multiply {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::Divide { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Divide {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::Modulo { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
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
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::LessThan {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::LessThanEqual { a , b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::LessThanEqual {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::GreaterThan { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::GreaterThan {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::GreaterThanEqual { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::GreaterThanEqual {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::Equals { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::Any);
            let b_typed = type_check_node!(*b, &PossibleTypes::Any);
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::Equals {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::NotEquals { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::Any);
            let b_typed = type_check_node!(*b, &PossibleTypes::Any);
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::NotEquals {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
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

fn limit_typed_node(
    type_scope: &mut TypeScope,
    variables: &mut HashMap<StringIdx, (PossibleTypes, bool)>,
    node: &TypedAstNode,
    limited_to: &PossibleTypes
) -> Option<Error> {
    macro_rules! limit_typed_node { ($node: expr) => {
        if let Some(error) = limit_typed_node(type_scope, variables, $node, limited_to) {
            return Some(error);
        }
    } }
    if let Err(error) = type_scope.limit_possible_types(node.get_types(), limited_to) {
        return Some(error);
    }
    match node.node_variant() {
        AstNodeVariant::Procedure { public: _, name: _, arguments: _, body: _ } |
        AstNodeVariant::Function { arguments: _, body: _ } |
        AstNodeVariant::Variable { public: _, mutable: _, name: _, value: _ } |
        AstNodeVariant::CaseBranches { value: _, branches: _ } |
        AstNodeVariant::CaseConditon { condition: _, body: _ } |
        AstNodeVariant::Assignment { variable: _, value: _ } |
        AstNodeVariant::Return { value: _ } => {
            // result of these operations is 'unit', or there is nothing to infer (function)
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
            if let Some((variable_types, variable_mutable)) = variables.get_mut(&name) {
                *variable_types = match type_scope.limit_possible_types(variable_types, limited_to) {
                    Err(error) => return Some(error),
                    Ok(new_type) => new_type
                }
            }
        }
        AstNodeVariant::BooleanLiteral { value: _ } |
        AstNodeVariant::IntegerLiteral { value: _ } |
        AstNodeVariant::FloatLiteral { value: _ } |
        AstNodeVariant::StringLiteral { value: _ } |
        AstNodeVariant::UnitLiteral => {
            // fixed type depending on literal type
        }
        AstNodeVariant::Add { a, b } |
        AstNodeVariant::Subtract { a, b } |
        AstNodeVariant::Multiply { a, b } |
        AstNodeVariant::Divide { a, b } |
        AstNodeVariant::Modulo { a, b } => {
            limit_typed_node!(&**a);
            limit_typed_node!(&**b);
        }
        AstNodeVariant::Negate { x } => {
            limit_typed_node!(&**x);
        }
        AstNodeVariant::LessThan { a: _, b: _ } |
        AstNodeVariant::LessThanEqual { a: _, b: _ } |
        AstNodeVariant::GreaterThan { a: _, b: _ } |
        AstNodeVariant::GreaterThanEqual { a: _, b: _ } |
        AstNodeVariant::Equals { a: _, b: _ } |
        AstNodeVariant::NotEquals { a: _, b: _ } |
        AstNodeVariant::And { a: _, b: _ } |
        AstNodeVariant::Or { a: _, b: _ } |
        AstNodeVariant::Not { x: _ } => {
            // result of these operations is 'bool'
        }
        AstNodeVariant::Module { path } => {
            // result of this operation is 'unit'
        }
        AstNodeVariant::ModuleAccess { path: _ } => {
            todo!("type check module access")
        }
        AstNodeVariant::Use { paths } => {
            // result of this operation is 'unit'
        }
    }
    None
}