
use std::collections::{HashMap, HashSet};

use crate::util::{
    strings::{StringMap, StringIdx},
    error::{Error, ErrorSection, ErrorType},
    source::{HasSource, SourceRange}
};

use crate::compiler::{
    ast::{TypedAstNode, AstNode, HasAstNodeVariant, AstNodeVariant},
    types::{TypeScope, PossibleTypes, Type, VarTypeIdx},
    modules::{NamespacePath, Module}
};


#[derive(Debug, Clone)]
pub enum Symbol<T: Clone + HasSource + HasAstNodeVariant<T>> {
    Constant { value: T },
    Procedure { scope: TypeScope, parameters: Vec<VarTypeIdx>, returns: VarTypeIdx, body: Vec<T> }
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
            strings,
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
    strings: &StringMap,
    global_scope: &mut TypeScope,
    untyped_symbols: &mut HashMap<NamespacePath, AstNode>,
    symbols: &'s mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    name: &NamespacePath
) -> Result<&'s Symbol<TypedAstNode>, Error> {
    if let Some(symbol) = untyped_symbols.remove(name) {
        match symbol.move_node() {
            AstNodeVariant::Procedure { public: _, name: _, arguments, body } => {
                let untyped_body = body;
                let mut procedure_scope = TypeScope::new();
                let mut argument_vars = Vec::new();
                let mut procedure_variables = HashMap::new();
                for argument_idx in 0..arguments.len() {
                    let var_type_idx = procedure_scope.register_variable();
                    argument_vars.push(var_type_idx);
                    procedure_variables.insert(arguments[argument_idx], (PossibleTypes::OfGroup(var_type_idx), false));
                }
                let return_types = procedure_scope.register_variable();
                symbols.insert(name.clone(), Symbol::Procedure {
                    scope: procedure_scope,
                    parameters: argument_vars,
                    returns: return_types,
                    body: Vec::new()
                } );
                let typed_body = match type_check_nodes(
                    strings,
                    global_scope,
                    Some(name),
                    &mut procedure_variables,
                    untyped_symbols,
                    symbols,
                    untyped_body,
                    &PossibleTypes::OfGroup(return_types)
                ) {
                    Ok(typed_nodes) => typed_nodes,
                    Err(error) => return Err(error),
                };
                if let Some(Symbol::Procedure { scope: _, parameters: _, returns: _, body }) = symbols.get_mut(name) {
                    *body = typed_body;
                } else { panic!("procedure was illegally modified!"); }
            }
            AstNodeVariant::Variable { public: _, mutable: _, name: _, value } => {
                let value_typed = match type_check_node(
                    strings,
                    global_scope,
                    None,
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
        Ok(symbol)
    } else {
        Err(Error::new([
            ErrorSection::Error(ErrorType::RecursiveConstant(name.display(strings)))
        ].into()))
    }
}

fn type_check_nodes(
    strings: &StringMap,
    global_scope: &mut TypeScope,
    procedure_name: Option<&NamespacePath>,
    variables: &mut HashMap<StringIdx, (PossibleTypes, bool)>,
    untyped_symbols: &mut HashMap<NamespacePath, AstNode>,
    symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    mut nodes: Vec<AstNode>,
    return_types: &PossibleTypes
) -> Result<Vec<TypedAstNode>, Error> {
    let mut typed_nodes = Vec::new();
    while nodes.len() > 0 {
        match type_check_node(strings, global_scope, procedure_name, variables, untyped_symbols, symbols, nodes.remove(0), return_types, &PossibleTypes::Any, false) {
            Ok(typed_node) => typed_nodes.push(typed_node),
            Err(error) => return Err(error)
        }
    }
    Ok(typed_nodes)
}

fn error_from_type_limit(
    strings: &StringMap,
    type_scope: &TypeScope,
    variables: &HashMap<StringIdx, (PossibleTypes, bool)>,
    source: SourceRange,
    a: &PossibleTypes,
    b: &PossibleTypes
) -> Error {
    Error::new([
        ErrorSection::Error(ErrorType::NoPossibleTypes(display_types(strings, type_scope, variables, a), display_types(strings, type_scope, variables, b))),
        ErrorSection::Code(source)
    ].into())
}

fn type_check_node(
    strings: &StringMap,
    global_scope: &mut TypeScope,
    procedure_name: Option<&NamespacePath>,
    variables: &mut HashMap<StringIdx, (PossibleTypes, bool)>,
    untyped_symbols: &mut HashMap<NamespacePath, AstNode>,
    symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    node: AstNode,
    return_types: &PossibleTypes,
    limited_to: &PossibleTypes,
    assignment: bool
) -> Result<TypedAstNode, Error> {
    let node_source = node.source();
    macro_rules! type_scope { () => {
        procedure_name.map(|procedure_name| {
            if let Symbol::Procedure { scope, parameters: _, returns: _, body: _ } = symbols.get_mut(procedure_name).expect("procedure name should be valid") {
                scope
            } else { panic!("procedure name should be valid"); }
        }).unwrap_or(global_scope)
    }}
    macro_rules! type_check_node { ($node: expr, $limited_to: expr) => {
        match type_check_node(strings, global_scope, procedure_name, variables, untyped_symbols, symbols, $node, return_types, $limited_to, assignment) {
            Ok(typed_node) => typed_node,
            Err(error) => return Err(error)
        }
    }; ($node: expr, $limited_to: expr, $assignment: expr) => {
        match type_check_node(strings, global_scope, procedure_name, variables, untyped_symbols, symbols, $node, return_types, $limited_to, $assignment) {
            Ok(typed_node) => typed_node,
            Err(error) => return Err(error)
        }
    } }
    macro_rules! type_check_nodes { ($nodes: expr, $variables: expr) => {
        match type_check_nodes(strings, global_scope, procedure_name, $variables, untyped_symbols, symbols, $nodes, return_types) {
            Ok(typed_node) => typed_node,
            Err(error) => return Err(error)
        }
    } }
    macro_rules! limit { ($a: expr, $b: expr) => { {
        match type_scope!().limit_possible_types($a, $b) {
            Some(result) => result,
            None => return Err(error_from_type_limit(strings, type_scope!(), variables, node_source, $a, $b)),
        }
    } } }
    macro_rules! limit_typed_node { ($node: expr, $limited_to: expr) => {
        if let Some(error) = limit_typed_node(strings, type_scope!(), variables, node_source, $node, $limited_to) {
            return Err(error);
        }
    } }
    match node.move_node() {
        AstNodeVariant::Procedure { public: _, name: _, arguments: _, body: _ } => panic!("The grammar checker failed to see a procedure inside another!"),
        AstNodeVariant::Function { arguments, body } => {
            let mut closure_variables = variables.clone();
            let mut closure_args = Vec::new();
            for argument in &arguments {
                let var_idx = type_scope!().register_variable();
                closure_args.push(var_idx);
                closure_variables.insert(*argument, (PossibleTypes::OfGroup(var_idx), false));
            }
            let return_types = type_scope!().register_variable();
            let typed_body = match type_check_nodes(
                strings,
                global_scope,
                procedure_name,
                &mut closure_variables,
                untyped_symbols,
                symbols,
                body,
                &PossibleTypes::OfGroup(return_types)
            ) {
                Ok(typed_nodes) => typed_nodes,
                Err(error) => return Err(error),
            };
            let closure_type = PossibleTypes::OneOf(vec![Type::Closure(closure_args, return_types)]);
            Ok(TypedAstNode::new(AstNodeVariant::Function {
                arguments,
                body: typed_body
            }, limit!(&closure_type, limited_to), node_source))
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
                typed_branches.push((type_check_node!(branch_value, typed_value.get_types()), type_check_nodes!(branch_body, &mut variables.clone())));
            }
            Ok(TypedAstNode::new(AstNodeVariant::CaseBranches {
                value: Box::new(typed_value),
                branches: typed_branches
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source))
        }
        AstNodeVariant::CaseConditon { condition, body } => {
            let typed_condition = type_check_node!(*condition, &PossibleTypes::OneOf(vec![Type::Boolean]));
            let typed_body = type_check_nodes!(body, &mut variables.clone());
            Ok(TypedAstNode::new(AstNodeVariant::CaseConditon {
                condition: Box::new(typed_condition),
                body: typed_body
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source))
        }
        AstNodeVariant::Assignment { variable, value } => {
            let typed_value = type_check_node!(*value, &PossibleTypes::Any);
            let typed_variable = type_check_node!(*variable, typed_value.get_types(), true);
            limit_typed_node!(&typed_variable, typed_value.get_types());
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
            limit!(return_types, typed_value.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::Return {
                value: Some(Box::new(typed_value))
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source))
        }
        AstNodeVariant::Call { called, mut arguments } => {
            if let AstNodeVariant::ModuleAccess { path } = called.node_variant() {
                match type_check_symbol(strings, global_scope, untyped_symbols, symbols, &path).map(|s| s.clone()) {
                    Ok(Symbol::Procedure { scope, parameters, returns, body: _ }) => {
                        if arguments.len() != parameters.len() { return Err(Error::new([
                            ErrorSection::Error(ErrorType::InvalidParameterCount(path.display(strings), parameters.len(), arguments.len())),
                            ErrorSection::Code(node_source)
                        ].into())) }
                        let inserted_proc_scope = type_scope!().insert_type_scope(&scope);
                        let mut typed_arguments = Vec::new();
                        for argument_idx in 0..arguments.len() {
                            let argument_type = inserted_proc_scope.translate(&PossibleTypes::OfGroup(parameters[argument_idx]));
                            typed_arguments.push(type_check_node!(arguments.remove(0), &argument_type));
                        }
                        let return_types = inserted_proc_scope.translate(&PossibleTypes::OfGroup(returns));
                        return Ok(TypedAstNode::new(AstNodeVariant::Call {
                            called: Box::new(type_check_node!(*called, &PossibleTypes::Any)),
                            arguments: typed_arguments
                        }, return_types, node_source));
                    }
                    Ok(_) => {}
                    Err(error) => return Err(error)
                }
            }
            let mut typed_arguments = Vec::new();
            let mut passed_arg_vars = Vec::new();
            for argument in arguments {
                let typed_argument = type_check_node!(argument, &PossibleTypes::Any);
                if let PossibleTypes::OfGroup(group) = typed_argument.get_types() {
                    passed_arg_vars.push(*group);
                } else {
                    let group = type_scope!().register_variable();
                    *type_scope!().get_group_types_mut(&group) = typed_argument.get_types().clone();
                    passed_arg_vars.push(group);
                }
                typed_arguments.push(typed_argument);
            }
            let passed_return_type = type_scope!().register_variable();
            limit!(limited_to, &PossibleTypes::OfGroup(passed_return_type));
            let typed_called = type_check_node!(*called, &PossibleTypes::OneOf(vec![Type::Closure(passed_arg_vars, passed_return_type)]));
            let mut closure_types = typed_called.get_types().clone();
            while let PossibleTypes::OfGroup(group_idx) = closure_types {
                closure_types = type_scope!().get_group_types(&group_idx).clone();
            }
            let mut result_type: PossibleTypes = PossibleTypes::Any;
            if let PossibleTypes::OneOf(possible_types) = closure_types {
                for possible_type in possible_types {
                    if let Type::Closure(_, return_types) = possible_type {
                        result_type = limit!(&result_type, &PossibleTypes::OfGroup(return_types));
                    } else {
                        panic!("We called something that's not a closure! Shouln't the first call to 'type_check_node!' have already enforced this?");
                    }
                }
            }
            Ok(TypedAstNode::new(AstNodeVariant::Call {
                called: Box::new(typed_called),
                arguments: typed_arguments
            }, result_type, node_source))
        }
        AstNodeVariant::Object { values } => {
            let mut member_types = HashMap::new();
            let mut typed_values = Vec::new();
            for (member_name, member_value) in values {
                let typed_member_value = type_check_node!(member_value, &PossibleTypes::Any);
                member_types.insert(member_name, typed_member_value.get_types().clone());
                typed_values.push((member_name, typed_member_value));
            }
            let object_type = PossibleTypes::OneOf(vec![Type::Object(member_types, true)]);
            Ok(TypedAstNode::new(AstNodeVariant::Object {
                values: typed_values
            }, limit!(limited_to, &object_type), node_source))
        }
        AstNodeVariant::Array { values } => {
            let mut array_type = PossibleTypes::Any;
            let mut typed_values = Vec::new();
            for value in values {
                let typed_value = type_check_node!(value, &array_type);
                array_type = limit!(&array_type, typed_value.get_types());
                typed_values.push(typed_value);
            }
            for typed_value in &typed_values {
                limit_typed_node!(&typed_value, &array_type);
            }
            let full_array_type = limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Array(array_type.clone())]));
            Ok(TypedAstNode::new(AstNodeVariant::Array {
                values: typed_values
            }, full_array_type, node_source))
        }
        AstNodeVariant::ObjectAccess { object, member } => {
            let typed_object = type_check_node!(*object, &PossibleTypes::OneOf(vec![Type::Object([(member, limited_to.clone())].into(), false)]), false);
            let mut typed_object_types = typed_object.get_types().clone();
            while let PossibleTypes::OfGroup(group_idx) = typed_object_types {
                typed_object_types = type_scope!().get_group_types(&group_idx).clone();
            }
            let mut result_type: PossibleTypes = PossibleTypes::Any;
            if let PossibleTypes::OneOf(possible_types) = typed_object_types {
                for possible_type in possible_types {
                    if let Type::Object(member_types, _) = possible_type {
                        result_type = limit!(&result_type, member_types.get(&member).expect("We accessed an invalid member! Shouln't the first call to 'type_check_node!' have already enforced this?"));
                    } else {
                        panic!("We accessed a member of something that's not an object! Shouln't the first call to 'type_check_node!' have already enforced this?");
                    }
                }
            }
            Ok(TypedAstNode::new(AstNodeVariant::ObjectAccess {
                object: Box::new(typed_object),
                member
            }, result_type, node_source))
        }
        AstNodeVariant::ArrayAccess { array, index } => {
            let typed_array = type_check_node!(*array, &PossibleTypes::OneOf(vec![Type::Array(limited_to.clone())]), false);
            let typed_index = type_check_node!(*index, &PossibleTypes::OneOf(vec![Type::Integer]), false);
            let mut typed_array_types = typed_array.get_types().clone();
            while let PossibleTypes::OfGroup(group_idx) = typed_array_types {
                typed_array_types = type_scope!().get_group_types(&group_idx).clone();
            }
            let mut result_type = PossibleTypes::Any;
            if let PossibleTypes::OneOf(possible_types) = typed_array_types {
                for possible_type in possible_types {
                    if let Type::Array(element_type) = possible_type {
                        result_type = limit!(&result_type, &element_type);
                    } else {
                        panic!("We indexed into something that's not an array! Shouln't the first call to 'type_check_node!' have already enforced this?");
                    }
                }
            }
            Ok(TypedAstNode::new(AstNodeVariant::ArrayAccess {
                array: Box::new(typed_array),
                index: Box::new(typed_index)
            }, result_type, node_source))
        }
        AstNodeVariant::VariableAccess { name } => {
            if let Some((variable_types, variable_mutable)) = variables.get_mut(&name) {
                let variable_types_cloned = variable_types.clone();
                if assignment && !*variable_mutable {
                    Err(Error::new([
                        ErrorSection::Error(ErrorType::ImmutableAssignmant(name)),
                        ErrorSection::Code(node_source)
                    ].into()))
                } else {
                    *variable_types = limit!(&variable_types_cloned, limited_to);
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
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to);
            let b_typed = type_check_node!(*b, limited_to);
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Add {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::Subtract { a, b } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to);
            let b_typed = type_check_node!(*b, limited_to);
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Subtract {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::Multiply { a, b } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to);
            let b_typed = type_check_node!(*b, limited_to);
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Multiply {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::Divide { a, b } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to);
            let b_typed = type_check_node!(*b, limited_to);
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Divide {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::Modulo { a, b } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to);
            let b_typed = type_check_node!(*b, limited_to);
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Modulo {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source))
        }
        AstNodeVariant::Negate { x } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let x_typed = type_check_node!(*x, limited_to);
            let node_type = x_typed.get_types().clone();
            Ok(TypedAstNode::new(AstNodeVariant::Negate {
                x: Box::new(x_typed),
            }, node_type, node_source))
        }
        AstNodeVariant::LessThan { a, b } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to);
            let b_typed = type_check_node!(*b, limited_to);
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::LessThan {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::LessThanEqual { a , b } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to);
            let b_typed = type_check_node!(*b, limited_to);
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::LessThanEqual {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::GreaterThan { a, b } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to);
            let b_typed = type_check_node!(*b, limited_to);
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::GreaterThan {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::GreaterThanEqual { a, b } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to);
            let b_typed = type_check_node!(*b, limited_to);
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::GreaterThanEqual {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::Equals { a, b } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to);
            let b_typed = type_check_node!(*b, limited_to);
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok(TypedAstNode::new(AstNodeVariant::Equals {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source))
        }
        AstNodeVariant::NotEquals { a, b } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to);
            let b_typed = type_check_node!(*b, limited_to);
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
        AstNodeVariant::ModuleAccess { path } => {
            match type_check_symbol(strings, global_scope, untyped_symbols, symbols, &path) {
                Ok(Symbol::Constant { value }) => {
                    Ok(TypedAstNode::new(AstNodeVariant::ModuleAccess {
                        path
                    }, value.get_types().clone(), node_source))
                }
                Ok(Symbol::Procedure { scope: _, parameters: _, returns: _, body: _ }) => {
                    Ok(TypedAstNode::new(AstNodeVariant::ModuleAccess {
                        path
                    }, PossibleTypes::OneOf(vec![Type::Unit]), node_source))
                }
                Err(error) => return Err(error)
            }
        }
        AstNodeVariant::Use { paths } => {
            Ok(TypedAstNode::new(AstNodeVariant::Use {
                paths
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source))
        }
    }
}

fn limit_typed_node(
    strings: &StringMap,
    type_scope: &mut TypeScope,
    variables: &mut HashMap<StringIdx, (PossibleTypes, bool)>,
    source: SourceRange,
    node: &TypedAstNode,
    limited_to: &PossibleTypes
) -> Option<Error> {
    macro_rules! limit_typed_node { ($node: expr) => {
        if let Some(error) = limit_typed_node(strings, type_scope, variables, source, $node, limited_to) {
            return Some(error);
        }
    }; ($node: expr, $limited_to: expr) => {
        if let Some(error) = limit_typed_node(strings, type_scope, variables, source, $node, $limited_to) {
            return Some(error);
        }
    } }
    if let None = type_scope.limit_possible_types(node.get_types(), limited_to) {
        return Some(error_from_type_limit(strings, type_scope, variables, source, node.get_types(), limited_to));
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
        AstNodeVariant::Call { called: _, arguments: _ } => {
            //todo!("type check calls")
            // this might not be needed
        }
        AstNodeVariant::Object { values: _ } => {
            //todo!("type check object literals")
            // this might not be needed
        }
        AstNodeVariant::Array { values: _ } => {
            //todo!("type check array literals")
            // this might not be needed
        }
        AstNodeVariant::ObjectAccess { object, member } => {
            limit_typed_node!(&**object, &PossibleTypes::OneOf(vec![Type::Object([(*member, limited_to.clone())].into(), false)]));
        }
        AstNodeVariant::ArrayAccess { array, index: _ } => {
            limit_typed_node!(&**array, &PossibleTypes::OneOf(vec![Type::Array(limited_to.clone())]));
        }
        AstNodeVariant::VariableAccess { name } => {
            if let Some((variable_types, _)) = variables.get_mut(&name) {
                let variable_types_cloned = variable_types.clone();
                match type_scope.limit_possible_types(variable_types, limited_to) {
                    None => return Some(error_from_type_limit(strings, type_scope, variables, source, &variable_types_cloned, limited_to)),
                    Some(new_type) => *variable_types = new_type
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
        AstNodeVariant::Module { path: _ } => {
            // result of this operation is 'unit'
        }
        AstNodeVariant::ModuleAccess { path: _ } => {
            // nothing to infer
        }
        AstNodeVariant::Use { paths: _ } => {
            // result of this operation is 'unit'
        }
    }
    None
}

fn display_types(
    strings: &StringMap,
    type_scope: &TypeScope,
    variables: &HashMap<StringIdx, (PossibleTypes, bool)>,
    types: &PossibleTypes
) -> String {
    fn display_type(
        strings: &StringMap,
        type_scope: &TypeScope,
        variables: &HashMap<StringIdx, (PossibleTypes, bool)>,
        displayed_type: &Type
    ) -> String {
        match displayed_type {
            Type::Unit => String::from("unit"),
            Type::Boolean => String::from("boolean"),
            Type::Integer => String::from("integer"),
            Type::Float => String::from("float"),
            Type::String => String::from("string"),
            Type::Array(element_type) => format!(
                "array ({})",
                display_types(strings, type_scope, variables, element_type)
            ),
            Type::Object(member_types, fixed) => format!(
                "object ({}{})",
                member_types.iter().map(|(member_name, member_type)| { format!(
                    "{} = {}",
                    strings.get(*member_name),
                    display_types(strings, type_scope, variables, member_type)
                ) }).collect::<Vec<String>>().join(", "),
                if *fixed { "" } else { ", ..." }
            ),
            Type::Closure(arg_groups, returned_group) => {
                let mut result: String = String::from("function (takes ");
                let mut used_internal_group_indices = HashSet::new();
                for a in 0..arg_groups.len().max(1) - 1 {
                    if a > 0 { result.push_str(", "); }
                    result.push_str("<");
                    let internal_idx = type_scope.get_group_internal_index(&arg_groups[a]);
                    used_internal_group_indices.insert(internal_idx);
                    result.push_str(&internal_idx.to_string());
                    result.push_str(">");
                }
                if arg_groups.len() > 1 { result.push_str(" and "); }
                result.push_str("<");
                let last_arg_internal_idx = type_scope.get_group_internal_index(&arg_groups[arg_groups.len() - 1]);
                used_internal_group_indices.insert(last_arg_internal_idx);
                result.push_str(&last_arg_internal_idx.to_string());
                result.push_str("> and returns ");
                let returned_internal_idx = type_scope.get_group_internal_index(returned_group);
                used_internal_group_indices.insert(returned_internal_idx);
                result.push_str("<");
                result.push_str(&returned_internal_idx.to_string());
                result.push_str(">, where ");
                let used_internal_group_indices = used_internal_group_indices.into_iter().collect::<Vec<usize>>();
                for i in 0..used_internal_group_indices.len() - 1 {
                    if i > 0 { result.push_str(", "); }
                    result.push_str("<");
                    result.push_str(&used_internal_group_indices[i].to_string());
                    result.push_str("> = ");
                    result.push_str(&display_types(strings, type_scope, variables, type_scope.get_group_types_from_internal_index(used_internal_group_indices[i])));
                }
                if used_internal_group_indices.len() > 1 { result.push_str(" and "); }
                result.push_str("<");
                result.push_str(&used_internal_group_indices[used_internal_group_indices.len() - 1].to_string());
                result.push_str("> = ");
                result.push_str(&display_types(strings, type_scope, variables, type_scope.get_group_types_from_internal_index(used_internal_group_indices[used_internal_group_indices.len() - 1])));
                result.push_str(")");
                result
            }
        }
    }
    match types {
        PossibleTypes::Any => String::from("any"),
        PossibleTypes::OneOf(possible_types) => {
            let mut result = String::new();
            for i in 0..possible_types.len() - 1 {
                if i > 0 { result.push_str(", "); }
                result.push_str(&display_type(strings, type_scope, variables, &possible_types[i]));
            }
            if possible_types.len() > 1 { result.push_str(" or "); }
            result.push_str(&display_type(strings, type_scope, variables, &possible_types[possible_types.len() - 1]));
            result
        }
        PossibleTypes::OfGroup(group_idx) => {
            display_types(strings, type_scope, variables, type_scope.get_group_types(group_idx))
        }
    }
}