
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
        let symbol_source = symbol.source();
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
                let (typed_body, returns) = match type_check_nodes(
                    strings,
                    global_scope,
                    Some(name),
                    &mut procedure_variables,
                    &mut HashMap::new(),
                    untyped_symbols,
                    symbols,
                    untyped_body,
                    &PossibleTypes::OfGroup(return_types)
                ) {
                    Ok(typed_nodes) => typed_nodes,
                    Err(error) => return Err(error),
                };
                if let Some(Symbol::Procedure { scope, parameters: _, returns: _, body }) = symbols.get_mut(name) {
                    if match scope.get_group_types(&return_types) {
                        PossibleTypes::OneOf(types) => types.len() == 1 && match &types[0] {
                            Type::Unit => false,
                            _ => true
                        },
                        _ => true 
                    } && returns.0 && !returns.1 { return Err(Error::new([
                        ErrorSection::Error(ErrorType::DoesNotAlwaysReturn("Procedure")),
                        ErrorSection::Code(symbol_source)
                    ].into())); }
                    if !returns.0 {
                        scope.limit_possible_types(&PossibleTypes::OfGroup(return_types), &PossibleTypes::OneOf(vec![Type::Unit]));
                    }
                    *body = typed_body;
                } else { panic!("procedure was illegally modified!"); }
            }
            AstNodeVariant::Variable { public: _, mutable: _, name: _, value } => {
                let value_typed = if let Some(value) = value {
                    match type_check_node(
                        strings,
                        global_scope,
                        None,
                        &mut HashMap::new(),
                        &mut HashMap::new(),
                        untyped_symbols,
                        symbols,
                        *value,
                        &mut PossibleTypes::Any,
                        &PossibleTypes::Any,
                        false
                    ) {
                        Ok((typed_node, _)) => typed_node,
                        Err(error) => return Err(error),
                    }
                } else { panic!("grammar checker failed to see a constant without a value"); };
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

type SometimesReturns = bool;
type AlwaysReturns = bool;

fn type_check_nodes(
    strings: &StringMap,
    global_scope: &mut TypeScope,
    procedure_name: Option<&NamespacePath>,
    variables: &mut HashMap<StringIdx, (PossibleTypes, bool)>,
    uninitialized_variables: &mut HashMap<StringIdx, bool>,
    untyped_symbols: &mut HashMap<NamespacePath, AstNode>,
    symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    mut nodes: Vec<AstNode>,
    return_types: &PossibleTypes
) -> Result<(Vec<TypedAstNode>, (SometimesReturns, AlwaysReturns)), Error> {
    let mut typed_nodes = Vec::new();
    let mut returns = (false, false);
    while nodes.len() > 0 {
        match type_check_node(
            strings,
            global_scope,
            procedure_name,
            variables,
            uninitialized_variables,
            untyped_symbols,
            symbols,
            nodes.remove(0),
            return_types,
            &PossibleTypes::Any,
            false
        ) {
            Ok((typed_node, node_returns)) => {
                if node_returns.0 { returns.0 = true; }
                if node_returns.1 { returns.1 = true; }
                typed_nodes.push(typed_node);
            }
            Err(error) => return Err(error)
        }
    }
    Ok((typed_nodes, returns))
}

fn error_from_type_limit(
    strings: &StringMap,
    type_scope: &TypeScope,
    source: SourceRange,
    a: &PossibleTypes,
    b: &PossibleTypes
) -> Error {
    Error::new([
        ErrorSection::Error(ErrorType::NoPossibleTypes(display_types(strings, type_scope, a), display_types(strings, type_scope, b))),
        ErrorSection::Code(source)
    ].into())
}

fn initalize_variables(
    strings: &StringMap,
    type_scope: &mut TypeScope,
    source: SourceRange,
    variables: &mut HashMap<StringIdx, (PossibleTypes, bool)>,
    uninitialized_variables: &mut HashMap<StringIdx, bool>,
    scopes_variables: &[HashMap<StringIdx, (PossibleTypes, bool)>],
    scopes_uninitialized_variables: &[HashMap<StringIdx, bool>]
) -> Option<Error> {
    for variable_name in uninitialized_variables.keys().map(|s| *s).collect::<Vec<StringIdx>>() {
        let mut always_has_value = true;
        let mut variable_types = PossibleTypes::Any;
        for scope_i in 0..scopes_uninitialized_variables.len() {
            if let Some(_) = scopes_uninitialized_variables[scope_i].get(&variable_name) {
                always_has_value = false;
                break;
            }
            if let Some((scope_variable_types, _)) = scopes_variables[scope_i].get(&variable_name) {
                match type_scope.limit_possible_types(
                    scope_variable_types, 
                    &variable_types
                ) {
                    Some(new_type) => variable_types = new_type,
                    None => return Some(Error::new([
                        ErrorSection::Error(ErrorType::NoPossibleTypes(display_types(strings, type_scope, scope_variable_types), display_types(strings, type_scope, &variable_types))),
                        ErrorSection::Code(source),
                        ErrorSection::Info(format!("While computing the type of the variable '{}'", strings.get(variable_name)))
                    ].into()))
                };
                continue;
            }
            panic!("the variable should exist either in 'variables' or in 'uninitialized_variables'");
        }
        if !always_has_value { continue; }
        let variable_mutable = uninitialized_variables.remove(&variable_name).expect("should still be in the map");
        variables.insert(variable_name, (variable_types, variable_mutable));
    }
    None
}

fn type_check_node(
    strings: &StringMap,
    global_scope: &mut TypeScope,
    procedure_name: Option<&NamespacePath>,
    variables: &mut HashMap<StringIdx, (PossibleTypes, bool)>,
    uninitialized_variables: &mut HashMap<StringIdx, bool>,
    untyped_symbols: &mut HashMap<NamespacePath, AstNode>,
    symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    node: AstNode,
    return_types: &PossibleTypes,
    limited_to: &PossibleTypes,
    assignment: bool
) -> Result<(TypedAstNode, (SometimesReturns, AlwaysReturns)), Error> {
    let node_source = node.source();
    macro_rules! type_scope { () => {
        procedure_name.map(|procedure_name| {
            if let Symbol::Procedure { scope, parameters: _, returns: _, body: _ } = symbols.get_mut(procedure_name).expect("procedure name should be valid") {
                scope
            } else { panic!("procedure name should be valid"); }
        }).unwrap_or(global_scope)
    }}
    macro_rules! type_check_node { ($node: expr, $limited_to: expr) => {
        match type_check_node(strings, global_scope, procedure_name, variables, uninitialized_variables, untyped_symbols, symbols, $node, return_types, $limited_to, assignment) {
            Ok(typed_node) => typed_node,
            Err(error) => return Err(error)
        }
    }; ($node: expr, $limited_to: expr, $assignment: expr) => {
        match type_check_node(strings, global_scope, procedure_name, variables, uninitialized_variables, untyped_symbols, symbols, $node, return_types, $limited_to, $assignment) {
            Ok(typed_node) => typed_node,
            Err(error) => return Err(error)
        }
    } }
    macro_rules! type_check_nodes { ($nodes: expr, $variables: expr, $uninitialized_variables: expr) => {
        match type_check_nodes(strings, global_scope, procedure_name, $variables, $uninitialized_variables, untyped_symbols, symbols, $nodes, return_types) {
            Ok(typed_node) => typed_node,
            Err(error) => return Err(error)
        }
    } }
    macro_rules! limit { ($a: expr, $b: expr) => { {
        match type_scope!().limit_possible_types($a, $b) {
            Some(result) => result,
            None => return Err(error_from_type_limit(strings, type_scope!(), node_source, $a, $b)),
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
            let (typed_body, returns) = match type_check_nodes(
                strings,
                global_scope,
                procedure_name,
                &mut closure_variables,
                &mut uninitialized_variables.clone(),
                untyped_symbols,
                symbols,
                body,
                &PossibleTypes::OfGroup(return_types)
            ) {
                Ok(typed_nodes) => typed_nodes,
                Err(error) => return Err(error),
            };
            if match type_scope!().get_group_types(&return_types) {
                PossibleTypes::OneOf(types) => types.len() == 1 && match &types[0] {
                    Type::Unit => false,
                    _ => true
                },
                _ => true 
            } && returns.0 && !returns.1 { return Err(Error::new([
                ErrorSection::Error(ErrorType::DoesNotAlwaysReturn("Function")),
                ErrorSection::Code(node_source)
            ].into())); }
            if !returns.0 {
                type_scope!().limit_possible_types(&PossibleTypes::OfGroup(return_types), &PossibleTypes::OneOf(vec![Type::Unit]));
            }
            let closure_type = PossibleTypes::OneOf(vec![Type::Closure(closure_args, return_types)]);
            Ok((TypedAstNode::new(AstNodeVariant::Function {
                arguments,
                body: typed_body
            }, limit!(&closure_type, limited_to), node_source), (false, false)))
        }
        AstNodeVariant::Variable { public, mutable, name, value } => {
            let typed_value = if let Some(value) = value {
                let typed_value = type_check_node!(*value, &PossibleTypes::Any).0;
                variables.insert(name, (typed_value.get_types().clone(), mutable));
                Some(Box::new(typed_value))
            } else {
                uninitialized_variables.insert(name, mutable);
                None
            };
            Ok((TypedAstNode::new(AstNodeVariant::Variable {
                public,
                mutable,
                name,
                value: typed_value
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source), (false, false)))
        }
        AstNodeVariant::CaseBranches { value, branches, else_body } => {
            let typed_value = type_check_node!(*value, &PossibleTypes::Any).0;
            let mut typed_branches = Vec::new();
            let mut branches_return = (false, branches.len() != 0);
            let mut branches_variables = Vec::new();
            let mut branches_uninitialized_variables = Vec::new();
            for (branch_value, branch_body) in branches {
                let mut branch_variables = variables.clone();
                let mut branch_uninitialized_variables = uninitialized_variables.clone();
                let (branch_body, branch_returns) = type_check_nodes!(branch_body, &mut branch_variables, &mut branch_uninitialized_variables);
                if branch_returns.0 { branches_return.0 = true; }
                if branches_return.1 && !branch_returns.1 { branches_return.1 = false; }
                typed_branches.push((type_check_node!(branch_value, typed_value.get_types()).0, branch_body));
                branches_variables.push(branch_variables);
                branches_uninitialized_variables.push(branch_uninitialized_variables);
            }
            let mut else_body_variables = variables.clone();
            let mut else_body_uninitialized_variables = uninitialized_variables.clone();
            let (typed_else_body, else_returns) = type_check_nodes!(else_body, &mut else_body_variables, &mut else_body_uninitialized_variables);
            branches_variables.push(else_body_variables);
            branches_uninitialized_variables.push(else_body_uninitialized_variables);
            if let Some(error) = initalize_variables(
                strings, type_scope!(), node_source, variables, uninitialized_variables,
                &branches_variables,
                &branches_uninitialized_variables
            ) { return Err(error); }
            Ok((TypedAstNode::new(AstNodeVariant::CaseBranches {
                value: Box::new(typed_value),
                branches: typed_branches,
                else_body: typed_else_body
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source), (branches_return.0 || else_returns.0, branches_return.1 && else_returns.1)))
        }
        AstNodeVariant::CaseConditon { condition, body, else_body } => {
            let typed_condition = type_check_node!(*condition, &PossibleTypes::OneOf(vec![Type::Boolean])).0;
            let mut body_variables = variables.clone();
            let mut body_uninitialized_variables = uninitialized_variables.clone();
            let (typed_body, body_returns) = type_check_nodes!(body, &mut body_variables, &mut body_uninitialized_variables);
            let mut else_body_variables = variables.clone();
            let mut else_body_uninitialized_variables = uninitialized_variables.clone();
            let (typed_else_body, else_returns) = type_check_nodes!(else_body, &mut else_body_variables, &mut else_body_uninitialized_variables);
            if let Some(error) = initalize_variables(
                strings, type_scope!(), node_source, variables, uninitialized_variables,
                &[body_variables, else_body_variables],
                &[body_uninitialized_variables, else_body_uninitialized_variables]
            ) { return Err(error); }
            Ok((TypedAstNode::new(AstNodeVariant::CaseConditon {
                condition: Box::new(typed_condition),
                body: typed_body,
                else_body: typed_else_body
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source), (body_returns.0 || else_returns.0, body_returns.1 && else_returns.1)))
        }
        AstNodeVariant::Assignment { variable, value } => {
            let typed_value = type_check_node!(*value, &PossibleTypes::Any).0;
            let typed_variable = type_check_node!(*variable, typed_value.get_types(), true).0;
            limit_typed_node!(&typed_variable, typed_value.get_types());
            Ok((TypedAstNode::new(AstNodeVariant::Assignment {
                variable: Box::new(typed_variable),
                value: Box::new(typed_value)
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source), (false, false)))
        }
        AstNodeVariant::Return { value } => {
            let typed_value = type_check_node!(*value, &return_types.clone()).0;
            limit!(return_types, typed_value.get_types());
            Ok((TypedAstNode::new(AstNodeVariant::Return {
                value: Box::new(typed_value)
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source), (true, true)))
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
                            typed_arguments.push(type_check_node!(arguments.remove(0), &argument_type).0);
                        }
                        let return_types = inserted_proc_scope.translate(&PossibleTypes::OfGroup(returns));
                        return Ok((TypedAstNode::new(AstNodeVariant::Call {
                            called: Box::new(type_check_node!(*called, &PossibleTypes::Any).0),
                            arguments: typed_arguments
                        }, return_types, node_source), (false, false)));
                    }
                    Ok(_) => {}
                    Err(error) => return Err(error)
                }
            }
            let mut typed_arguments = Vec::new();
            let mut passed_arg_vars = Vec::new();
            for argument in arguments {
                let typed_argument = type_check_node!(argument, &PossibleTypes::Any).0;
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
            let typed_called = type_check_node!(*called, &PossibleTypes::OneOf(vec![Type::Closure(passed_arg_vars, passed_return_type)])).0;
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
            Ok((TypedAstNode::new(AstNodeVariant::Call {
                called: Box::new(typed_called),
                arguments: typed_arguments
            }, result_type, node_source), (false, false)))
        }
        AstNodeVariant::Object { values } => {
            let mut member_types = HashMap::new();
            let mut typed_values = Vec::new();
            for (member_name, member_value) in values {
                let typed_member_value = type_check_node!(member_value, &PossibleTypes::Any).0;
                member_types.insert(member_name, typed_member_value.get_types().clone());
                typed_values.push((member_name, typed_member_value));
            }
            let object_type = PossibleTypes::OneOf(vec![Type::Object(member_types, true)]);
            Ok((TypedAstNode::new(AstNodeVariant::Object {
                values: typed_values
            }, limit!(limited_to, &object_type), node_source), (false, false)))
        }
        AstNodeVariant::Array { values } => {
            let mut array_type = PossibleTypes::Any;
            let mut typed_values = Vec::new();
            for value in values {
                let typed_value = type_check_node!(value, &array_type).0;
                array_type = limit!(&array_type, typed_value.get_types());
                typed_values.push(typed_value);
            }
            for typed_value in &typed_values {
                limit_typed_node!(&typed_value, &array_type);
            }
            let full_array_type = limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Array(array_type.clone())]));
            Ok((TypedAstNode::new(AstNodeVariant::Array {
                values: typed_values
            }, full_array_type, node_source), (false, false)))
        }
        AstNodeVariant::ObjectAccess { object, member } => {
            let typed_object = type_check_node!(*object, &PossibleTypes::OneOf(vec![Type::Object([(member, limited_to.clone())].into(), false)]), false).0;
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
            Ok((TypedAstNode::new(AstNodeVariant::ObjectAccess {
                object: Box::new(typed_object),
                member
            }, result_type, node_source), (false, false)))
        }
        AstNodeVariant::ArrayAccess { array, index } => {
            let typed_array = type_check_node!(*array, &PossibleTypes::OneOf(vec![Type::Array(limited_to.clone())]), false).0;
            let typed_index = type_check_node!(*index, &PossibleTypes::OneOf(vec![Type::Integer]), false).0;
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
            Ok((TypedAstNode::new(AstNodeVariant::ArrayAccess {
                array: Box::new(typed_array),
                index: Box::new(typed_index)
            }, result_type, node_source), (false, false)))
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
                    Ok((TypedAstNode::new(
                        AstNodeVariant::VariableAccess { name },
                        variable_types.clone(),
                        node_source
                    ), (false, false)))
                }
            } else if let Some(variable_mutable) = uninitialized_variables.get(&name) {
                if assignment {
                    let variable_mutable = *variable_mutable;
                    uninitialized_variables.remove(&name);
                    variables.insert(name, (PossibleTypes::Any, variable_mutable));
                    Ok((TypedAstNode::new(
                        AstNodeVariant::VariableAccess { name },
                        PossibleTypes::Any,
                        node_source
                    ), (false, false)))
                } else {
                    Err(Error::new([
                        ErrorSection::Error(ErrorType::VariableWithoutValue(name)),
                        ErrorSection::Code(node_source)
                    ].into()))
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
            Ok((TypedAstNode::new(
                AstNodeVariant::BooleanLiteral { value },
                PossibleTypes::OneOf(vec![Type::Boolean]),
                node_source
            ), (false, false)))
        }
        AstNodeVariant::IntegerLiteral { value } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer]));
            Ok((TypedAstNode::new(
                AstNodeVariant::IntegerLiteral { value },
                PossibleTypes::OneOf(vec![Type::Integer]),
                node_source
            ), (false, false)))
        }
        AstNodeVariant::FloatLiteral { value } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Float]));
            Ok((TypedAstNode::new(
                AstNodeVariant::FloatLiteral { value },
                PossibleTypes::OneOf(vec![Type::Float]),
                node_source
            ), (false, false)))
        }
        AstNodeVariant::StringLiteral { value } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::String]));
            Ok((TypedAstNode::new(
                AstNodeVariant::StringLiteral { value },
                PossibleTypes::OneOf(vec![Type::String]),
                node_source
            ), (false, false)))
        }
        AstNodeVariant::UnitLiteral => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Unit]));
            Ok((TypedAstNode::new(
                AstNodeVariant::UnitLiteral,
                PossibleTypes::OneOf(vec![Type::Unit]),
                node_source
            ), (false, false)))
        }
        AstNodeVariant::Add { a, b } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to).0;
            let b_typed = type_check_node!(*b, limited_to).0;
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok((TypedAstNode::new(AstNodeVariant::Add {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source), (false, false)))
        }
        AstNodeVariant::Subtract { a, b } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to).0;
            let b_typed = type_check_node!(*b, limited_to).0;
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok((TypedAstNode::new(AstNodeVariant::Subtract {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source), (false, false)))
        }
        AstNodeVariant::Multiply { a, b } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to).0;
            let b_typed = type_check_node!(*b, limited_to).0;
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok((TypedAstNode::new(AstNodeVariant::Multiply {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source), (false, false)))
        }
        AstNodeVariant::Divide { a, b } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to).0;
            let b_typed = type_check_node!(*b, limited_to).0;
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok((TypedAstNode::new(AstNodeVariant::Divide {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source), (false, false)))
        }
        AstNodeVariant::Modulo { a, b } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let a_typed = type_check_node!(*a, limited_to).0;
            let b_typed = type_check_node!(*b, limited_to).0;
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            let node_type = b_typed.get_types().clone();
            Ok((TypedAstNode::new(AstNodeVariant::Modulo {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, node_type, node_source), (false, false)))
        }
        AstNodeVariant::Negate { x } => {
            limit!(limited_to, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float]));
            let x_typed = type_check_node!(*x, limited_to).0;
            let node_type = x_typed.get_types().clone();
            Ok((TypedAstNode::new(AstNodeVariant::Negate {
                x: Box::new(x_typed),
            }, node_type, node_source), (false, false)))
        }
        AstNodeVariant::LessThan { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float])).0;
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float])).0;
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok((TypedAstNode::new(AstNodeVariant::LessThan {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source), (false, false)))
        }
        AstNodeVariant::LessThanEqual { a , b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float])).0;
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float])).0;
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok((TypedAstNode::new(AstNodeVariant::LessThanEqual {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source), (false, false)))
        }
        AstNodeVariant::GreaterThan { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float])).0;
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float])).0;
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok((TypedAstNode::new(AstNodeVariant::GreaterThan {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source), (false, false)))
        }
        AstNodeVariant::GreaterThanEqual { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float])).0;
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float])).0;
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok((TypedAstNode::new(AstNodeVariant::GreaterThanEqual {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source), (false, false)))
        }
        AstNodeVariant::Equals { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float])).0;
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float])).0;
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok((TypedAstNode::new(AstNodeVariant::Equals {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source), (false, false)))
        }
        AstNodeVariant::NotEquals { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float])).0;
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Integer, Type::Float])).0;
            limit_typed_node!(&a_typed, b_typed.get_types());
            limit_typed_node!(&b_typed, a_typed.get_types());
            Ok((TypedAstNode::new(AstNodeVariant::NotEquals {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source), (false, false)))
        }
        AstNodeVariant::And { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Boolean])).0;
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Boolean])).0;
            Ok((TypedAstNode::new(AstNodeVariant::And {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source), (false, false)))
        }
        AstNodeVariant::Or { a, b } => {
            let a_typed = type_check_node!(*a, &PossibleTypes::OneOf(vec![Type::Boolean])).0;
            let b_typed = type_check_node!(*b, &PossibleTypes::OneOf(vec![Type::Boolean])).0;
            Ok((TypedAstNode::new(AstNodeVariant::Or {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source), (false, false)))
        }
        AstNodeVariant::Not { x } => {
            let x_typed = type_check_node!(*x, &PossibleTypes::OneOf(vec![Type::Boolean])).0;
            Ok((TypedAstNode::new(AstNodeVariant::Not {
                x: Box::new(x_typed),
            }, PossibleTypes::OneOf(vec![Type::Boolean]), node_source), (false, false)))
        }
        AstNodeVariant::Module { path } => {
            Ok((TypedAstNode::new(AstNodeVariant::Module {
                path
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source), (false, false)))
        }
        AstNodeVariant::ModuleAccess { path } => {
            match type_check_symbol(strings, global_scope, untyped_symbols, symbols, &path) {
                Ok(Symbol::Constant { value }) => {
                    Ok((TypedAstNode::new(AstNodeVariant::ModuleAccess {
                        path
                    }, value.get_types().clone(), node_source), (false, false)))
                }
                Ok(Symbol::Procedure { scope: _, parameters: _, returns: _, body: _ }) => {
                    Ok((TypedAstNode::new(AstNodeVariant::ModuleAccess {
                        path
                    }, PossibleTypes::OneOf(vec![Type::Unit]), node_source), (false, false)))
                }
                Err(error) => return Err(error)
            }
        }
        AstNodeVariant::Use { paths } => {
            Ok((TypedAstNode::new(AstNodeVariant::Use {
                paths
            }, PossibleTypes::OneOf(vec![Type::Unit]), node_source), (false, false)))
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
        return Some(error_from_type_limit(strings, type_scope, source, node.get_types(), limited_to));
    }
    match node.node_variant() {
        AstNodeVariant::Procedure { public: _, name: _, arguments: _, body: _ } |
        AstNodeVariant::Function { arguments: _, body: _ } |
        AstNodeVariant::Variable { public: _, mutable: _, name: _, value: _ } |
        AstNodeVariant::CaseBranches { value: _, branches: _, else_body: _ } |
        AstNodeVariant::CaseConditon { condition: _, body: _, else_body: _ } |
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
                    None => return Some(error_from_type_limit(strings, type_scope, source, &variable_types_cloned, limited_to)),
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
    types: &PossibleTypes
) -> String {
    fn display_type(
        strings: &StringMap,
        type_scope: &TypeScope,
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
                display_types(strings, type_scope, element_type)
            ),
            Type::Object(member_types, fixed) => format!(
                "object ({}{})",
                member_types.iter().map(|(member_name, member_type)| { format!(
                    "{} = {}",
                    strings.get(*member_name),
                    display_types(strings, type_scope, member_type)
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
                    result.push_str(&display_types(strings, type_scope, type_scope.get_group_types_from_internal_index(used_internal_group_indices[i])));
                }
                if used_internal_group_indices.len() > 1 { result.push_str(" and "); }
                result.push_str("<");
                result.push_str(&used_internal_group_indices[used_internal_group_indices.len() - 1].to_string());
                result.push_str("> = ");
                result.push_str(&display_types(strings, type_scope, type_scope.get_group_types_from_internal_index(used_internal_group_indices[used_internal_group_indices.len() - 1])));
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
                result.push_str(&display_type(strings, type_scope, &possible_types[i]));
            }
            if possible_types.len() > 1 { result.push_str(" or "); }
            result.push_str(&display_type(strings, type_scope, &possible_types[possible_types.len() - 1]));
            result
        }
        PossibleTypes::OfGroup(group_idx) => {
            display_types(strings, type_scope, type_scope.get_group_types(group_idx))
        }
    }
}