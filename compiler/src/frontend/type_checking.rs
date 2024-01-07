
use std::collections::{HashMap, HashSet};

use crate::util::{
    strings::{StringMap, StringIdx},
    error::{Error, ErrorSection, ErrorType},
    source::{HasSource, SourceRange}
};

use crate::frontend::{
    ast::{TypedAstNode, AstNode, HasAstNodeVariant, AstNodeVariant},
    types::{TypeScope, Type, TypeGroup},
    modules::{NamespacePath, Module}
};


#[derive(Debug, Clone)]
pub enum Symbol<T: Clone + HasSource + HasAstNodeVariant<T>> {
    Constant {
        public: bool,
        value: Option<T>,
        value_types: TypeGroup
    },
    Procedure {
        public: bool,
        parameter_names: Vec<StringIdx>, 
        parameter_types: Vec<TypeGroup>,
        returns: TypeGroup,
        body: Option<Vec<T>>,
        source: SourceRange,
        type_scope: TypeScope
    }
}

pub fn type_check_modules(modules: HashMap<NamespacePath, Module<AstNode>>, strings: &StringMap, type_scope: &mut TypeScope, typed_symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>) -> Result<(), Vec<Error>> {
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
            type_scope,
            &mut Vec::new(),
            &mut old_symbols,
            typed_symbols,
            &symbol_path
        ) { errors.push(error); }
    }
    if errors.len() > 0 { Err(errors) }
        else { Ok(()) }
}

#[derive(Debug, Clone)]
struct TypeAssertion {
    limited_to: TypeGroup,
    from: SourceRange,
    reason: String
}

impl TypeAssertion {
    fn unexplained(variable_types: TypeGroup) -> TypeAssertion {
        TypeAssertion {
            limited_to: variable_types,
            from: SourceRange::new(StringIdx(0), StringIdx(0), 0, 0),
            reason: String::from("if you see this something went terribly wrong, I am sorry")
        }
    }
    fn variable(variable_source: SourceRange, variable_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: variable_types,
            from: variable_source,
            reason: format!(
                "This variable is of type {}",
                display_types(strings, type_scope, variable_types)
            )
        }
    }
    fn literal(literal_kind: &'static str, literal_source: SourceRange, literal_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: literal_types,
            from: literal_source,
            reason: format!(
                "This {} literal is of type {}",
                literal_kind,
                display_types(strings, type_scope, literal_types)
            )
        }
    }
    fn condition(source: SourceRange, condition_type: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: condition_type,
            from: source,
            reason: format!(
                "Used as a condition here, meaning it must be of type {}",
                display_types(strings, type_scope, condition_type)
            )
        }
    }
    fn assigned_value(value_source: SourceRange, value_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: value_types,
            from: value_source,
            reason: format!(
                "The assigned value is of type {}",
                display_types(strings, type_scope, value_types)
            )
        }
    }
    fn returned_values(procedure_source: SourceRange, returned_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: returned_types,
            from: procedure_source,
            reason: format!(
                "Previous return values were of type {}",
                display_types(strings, type_scope, returned_types)
            )
        }
    }
    fn implicit_unit_return(procedure_source: SourceRange, type_scope: &mut TypeScope, strings: &StringMap) -> TypeAssertion {
        let asserted_type = type_scope.insert_group(&[Type::Unit]);
        TypeAssertion {
            limited_to: asserted_type,
            from: procedure_source,
            reason: format!(
                "Does not always return early, therefore implicitly returns {} at the end of its body",
                display_types(strings, type_scope, asserted_type)
            )
        }
    }
    fn call_parameter(call_source: SourceRange, parameter_name: StringIdx, parameter_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: parameter_types,
            from: call_source,
            reason: format!(
                "This call expects the parameter '{}' to be of type {}",
                strings.get(parameter_name),
                display_types(strings, type_scope, parameter_types)
            )
        }
    }
    fn call_return_value(call_source: SourceRange, return_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: return_types,
            from: call_source,
            reason: format!(
                "This call returns a value of type {}",
                display_types(strings, type_scope, return_types)
            )
        }
    }
    fn called_closure(call_source: SourceRange, called_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: called_types,
            from: call_source,
            reason: format!(
                "This call expects the called closure to be of type {}",
                display_types(strings, type_scope, called_types)
            )
        }
    }
    fn arithmetic_result(op_source: SourceRange, result_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: result_types,
            from: op_source,
            reason: format!(
                "This arithmetic operation results in a value of type {}",
                display_types(strings, type_scope, result_types)
            )
        }
    }
    fn arithmetic_argument(op_source: SourceRange, argument_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: argument_types,
            from: op_source,
            reason: format!(
                "This arithmetic operation requires a value of type {}",
                display_types(strings, type_scope, argument_types)
            )
        }
    }
    fn comparison_result(op_source: SourceRange, result_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: result_types,
            from: op_source,
            reason: format!(
                "This comparison results in a value of type {}",
                display_types(strings, type_scope, result_types)
            )
        }
    }
    fn comparison_argument(op_source: SourceRange, argument_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: argument_types,
            from: op_source,
            reason: format!(
                "This comparison requires a value of type {}",
                display_types(strings, type_scope, argument_types)
            )
        }
    }
    fn logical_result(op_source: SourceRange, result_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: result_types,
            from: op_source,
            reason: format!(
                "This logical operation results in a value of type {}",
                display_types(strings, type_scope, result_types)
            )
        }
    }
    fn logical_argument(op_source: SourceRange, argument_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: argument_types,
            from: op_source,
            reason: format!(
                "This logical operation requires a value of type {}",
                display_types(strings, type_scope, argument_types)
            )
        }
    }
    fn constant(access_source: SourceRange, constant_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: constant_types,
            from: access_source,
            reason: format!(
                "This constant has type {}",
                display_types(strings, type_scope, constant_types)
            )
        }
    }
    fn array_values(array_source: SourceRange, element_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: element_types,
            from: array_source,
            reason: format!(
                "Previous array values were of type {}",
                display_types(strings, type_scope, element_types)
            )
        }
    }
    fn accessed_object(access_source: SourceRange, accessed_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: accessed_types,
            from: access_source,
            reason: format!(
                "This access requires the accessed object to be of type {}",
                display_types(strings, type_scope, accessed_types)
            )
        }
    }
    fn access_result(access_source: SourceRange, result_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: result_types,
            from: access_source,
            reason: format!(
                "This access results in a value of type {}",
                display_types(strings, type_scope, result_types)
            )
        }
    }
    fn accessed_array(access_source: SourceRange, accessed_types: TypeGroup) -> TypeAssertion {
        TypeAssertion {
            limited_to: accessed_types,
            from: access_source,
            reason: String::from("This access requires the accessed thing to be an array")
        }
    }
    fn array_index(access_source: SourceRange, index_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: index_types,
            from: access_source,
            reason: format!(
                "Used as an array index here, meaning it must be of type {}",
                display_types(strings, type_scope, index_types)
            )
        }
    }
    fn branch_variants(branch_source: SourceRange, variant_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: variant_types,
            from: branch_source,
            reason: format!(
                "Branches require the matched value to be of type {}",
                display_types(strings, type_scope, variant_types)
            )
        }
    }
    fn matched_value(branch_source: SourceRange, matched_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: matched_types,
            from: branch_source,
            reason: format!(
                "This matched value is of type {}",
                display_types(strings, type_scope, matched_types)
            )
        }
    }
    fn procedure_parameter(procedure_source: SourceRange, parameter_name: StringIdx, parameter_types: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: parameter_types,
            from: procedure_source,
            reason: format!(
                "The called procedure expects the parameter '{}' to be of type {}",
                strings.get(parameter_name),
                display_types(strings, type_scope, parameter_types)
            )
        }
    }
    fn call_parameter_value(param_source: SourceRange, given_type: TypeGroup, type_scope: &TypeScope, strings: &StringMap) -> TypeAssertion {
        TypeAssertion {
            limited_to: given_type,
            from: param_source,
            reason: format!(
                "The call provides a parameter value of type {}",
                display_types(strings, type_scope, given_type)
            )
        }
    }
}

fn type_check_symbol<'s>(
    strings: &StringMap,
    global_scope: &mut TypeScope,
    rec_procedures: &mut Vec<(NamespacePath, Vec<Vec<(TypeGroup, SourceRange)>>, TypeScope)>,
    untyped_symbols: &mut HashMap<NamespacePath, AstNode>,
    symbols: &'s mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    name: &NamespacePath
) -> Result<&'s Symbol<TypedAstNode>, Error> {
    if let Some(symbol) = untyped_symbols.remove(name) {
        let symbol_source = symbol.source();
        match symbol.move_node() {
            AstNodeVariant::Procedure { public, name: _, arguments, body } => {
                let mut type_scope = TypeScope::new();
                let untyped_body = body;
                let mut argument_vars = Vec::new();
                let mut procedure_variables = HashMap::new();
                let mut procedure_scope_variables = HashSet::new();
                for argument_idx in 0..arguments.len() {
                    let arg_type = type_scope.insert_group(&[Type::Any]);
                    argument_vars.push(arg_type);
                    procedure_variables.insert(arguments[argument_idx].0, (arg_type, false, arguments[argument_idx].1));
                    procedure_scope_variables.insert(arguments[argument_idx].0);
                }
                let return_types = type_scope.insert_group(&[Type::Any]);
                symbols.insert(name.clone(), Symbol::Procedure {
                    public,
                    parameter_names: arguments.iter().map(|p| p.0).collect(),
                    parameter_types: argument_vars,
                    returns: return_types,
                    body: Some(Vec::new()),
                    source: symbol_source,
                    type_scope: TypeScope::new()
                } );
                rec_procedures.push((name.clone(), vec![Vec::new(); arguments.len()], type_scope));
                let (typed_body, returns) = match type_check_nodes(
                    strings,
                    global_scope,
                    rec_procedures,
                    symbol_source,
                    &mut procedure_variables,
                    &mut procedure_scope_variables,
                    &mut HashMap::new(),
                    &mut HashSet::new(),
                    untyped_symbols,
                    symbols,
                    untyped_body,
                    return_types
                ) {
                    Ok(typed_nodes) => typed_nodes,
                    Err(error) => return Err(error),
                };
                if let Some(Symbol::Procedure { public: _, parameter_names: _, parameter_types, returns: _, body, source, type_scope: symbol_type_scope }) = symbols.get_mut(name) {
                    if let Some((_, arg_groups, mut type_scope)) = rec_procedures.pop() {
                        fn copy_arg_type_group(t: TypeGroup, mapped: &mut HashMap<usize, TypeGroup>, arg_groups: &Vec<Vec<(TypeGroup, SourceRange)>>, type_scope: &mut TypeScope) -> TypeGroup {
                            if let Some(n) = mapped.get(&type_scope.group_internal_id(t)) {
                                return *n;
                            }
                            for arg in arg_groups {
                                for (a, _) in arg {
                                    if t == *a { return t; }
                                }
                            }
                            let new_group = type_scope.insert_group(&[Type::Any]);
                            mapped.insert(type_scope.group_internal_id(t), new_group);
                            let og_group_types = type_scope.group(t).collect::<Vec<Type>>();
                            let new_group_types = og_group_types.iter().map(|t|
                                copy_arg_types(t, mapped, arg_groups, type_scope)
                            ).collect::<Vec<Type>>();
                            type_scope.set_group_types(
                                new_group,
                                &new_group_types
                            );
                            return new_group;
                        }
                        fn copy_arg_types(t: &Type, mapped: &mut HashMap<usize, TypeGroup>, arg_groups: &Vec<Vec<(TypeGroup, SourceRange)>>, type_scope: &mut TypeScope) -> Type {
                            match t {
                                Type::Any | Type::Unit | Type::Boolean | Type::Integer | Type::Float | Type::String => *t,
                                Type::Array(arr) => {
                                    let element_type = copy_arg_type_group(type_scope.array(*arr), mapped, arg_groups, type_scope);
                                    Type::Array(type_scope.insert_array(element_type))
                                }
                                Type::Object(obj) => {
                                    let (member_types, fixed) = type_scope.object(*obj).clone();
                                    let new_member_types = member_types.into_iter().map(|(member_name, member_types)| (
                                        member_name,
                                        copy_arg_type_group(member_types, mapped, arg_groups, type_scope)
                                    )).collect();
                                    Type::Object(type_scope.insert_object(
                                        new_member_types,
                                        fixed
                                    ))
                                }
                                Type::ConcreteObject(obj) => {
                                    let member_types = type_scope.concrete_object(*obj).clone();
                                    let new_member_types = member_types.into_iter().map(|(member_name, member_type)| (
                                        member_name,
                                        copy_arg_type_group(member_type, mapped, arg_groups, type_scope)
                                    )).collect();
                                    Type::ConcreteObject(type_scope.insert_concrete_object(new_member_types))
                                }
                                Type::Closure(clo) => {
                                    let (parameter_types, return_types, captured) = type_scope.closure(*clo).clone();
                                    let new_parameter_types = parameter_types.into_iter().map(|p| copy_arg_type_group(p, mapped, arg_groups, type_scope)).collect();
                                    let new_return_types = copy_arg_type_group(return_types, mapped, arg_groups, type_scope);
                                    let new_captured = captured.as_ref().map(|captured| captured.iter().map(|(capture_name, capture_types)| (
                                        *capture_name,
                                        copy_arg_type_group(*capture_types, mapped, arg_groups, type_scope)
                                    )).collect::<HashMap<StringIdx, TypeGroup>>());
                                    Type::Closure(type_scope.insert_closure(
                                        new_parameter_types,
                                        new_return_types,
                                        new_captured
                                    ))
                                }
                                Type::Variants(var) => {
                                    let (variant_types, fixed) = type_scope.variants(*var).clone();
                                    let new_variant_types = variant_types.into_iter().map(|(variant_name, variant_types)| (
                                        variant_name,
                                        copy_arg_type_group(variant_types, mapped, arg_groups, type_scope)
                                    )).collect();
                                    Type::Variants(type_scope.insert_variants(
                                        new_variant_types,
                                        fixed
                                    ))
                                }
                            }
                        }
                        for argument_idx in 0..arguments.len() {
                            let argument_types = copy_arg_type_group(parameter_types[argument_idx], &mut HashMap::new(), &arg_groups, &mut type_scope);
                            for (call_param_types, call_param_source) in &arg_groups[argument_idx] {
                                assert_types(
                                    TypeAssertion::procedure_parameter(symbol_source, arguments[argument_idx].0, argument_types, &mut type_scope, strings),
                                    TypeAssertion::call_parameter_value(*call_param_source, *call_param_types, &mut type_scope, strings),
                                    &mut type_scope
                                )?;
                            }
                        }
                        if !returns.1 {
                            let assertion_a = TypeAssertion::returned_values(*source, return_types, &type_scope, strings);
                            let assertion_b = TypeAssertion::implicit_unit_return(*source, &mut type_scope, strings);
                            assert_types(assertion_a, assertion_b, &mut type_scope)?;
                        }
                        *body = Some(typed_body);
                        *symbol_type_scope = type_scope;
                    } else { panic!("procedure stack was illegally modified!"); }
                } else { panic!("procedure was illegally modified!"); }
            }
            AstNodeVariant::Variable { public, mutable: _, name: _, value_types: _, value } => {
                rec_procedures.push((NamespacePath::new(Vec::new()), vec![], TypeScope::new()));
                let return_types = global_scope.insert_group(&[Type::Any]);
                let value_typed = if let Some(value) = value {
                    match type_check_node(
                        strings,
                        global_scope,
                        rec_procedures,
                        symbol_source,
                        &mut HashMap::new(),
                        &mut HashSet::new(),
                        &mut HashMap::new(),
                        &mut HashSet::new(),
                        untyped_symbols,
                        symbols,
                        *value,
                        return_types,
                        None,
                        false
                    ) {
                        Ok((typed_node, _)) => typed_node,
                        Err(error) => return Err(error),
                    }
                } else { panic!("grammar checker failed to see a constant without a value"); };
                let variable_types = rec_procedures.pop().expect("should still be in there")
                    .2.transfer_group(value_typed.get_types(), global_scope);
                symbols.insert(name.clone(), Symbol::Constant {
                    public,
                    value: Some(value_typed),
                    value_types: variable_types
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
    global_type_scope: &mut TypeScope,
    rec_procedures: &mut Vec<(NamespacePath, Vec<Vec<(TypeGroup, SourceRange)>>, TypeScope)>,
    procedure_source: SourceRange,
    variables: &mut HashMap<StringIdx, (TypeGroup, bool, SourceRange)>,
    scope_variables: &mut HashSet<StringIdx>,
    uninitialized_variables: &mut HashMap<StringIdx, (TypeGroup, bool, SourceRange)>,
    captured_variables: &mut HashSet<StringIdx>,
    untyped_symbols: &mut HashMap<NamespacePath, AstNode>,
    symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    mut nodes: Vec<AstNode>,
    return_types: TypeGroup
) -> Result<(Vec<TypedAstNode>, (SometimesReturns, AlwaysReturns)), Error> {
    let mut typed_nodes = Vec::new();
    let mut returns = (false, false);
    while nodes.len() > 0 {
        match type_check_node(
            strings,
            global_type_scope,
            rec_procedures,
            procedure_source,
            variables,
            scope_variables,
            uninitialized_variables,
            captured_variables,
            untyped_symbols,
            symbols,
            nodes.remove(0),
            return_types,
            None,
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

fn error_from_type_assertions(
    a: TypeAssertion,
    b: TypeAssertion
) -> Error {
    Error::new([
        ErrorSection::Error(ErrorType::NoPossibleTypes),
        ErrorSection::Info(a.reason),
        ErrorSection::Code(a.from),
        ErrorSection::Info(b.reason),
        ErrorSection::Code(b.from)
    ].into())
}

fn assert_types(
    a: TypeAssertion,
    b: TypeAssertion,
    type_scope: &mut TypeScope
) -> Result<(), Error> {
    if type_scope.try_merge_groups(a.limited_to, b.limited_to) {
        Ok(())
    } else {
        Err(error_from_type_assertions(a, b))
    }
}

fn initalize_variables(
    strings: &StringMap,
    type_scope: &mut TypeScope,
    variables: &mut HashMap<StringIdx, (TypeGroup, bool, SourceRange)>,
    uninitialized_variables: &mut HashMap<StringIdx, (TypeGroup, bool, SourceRange)>,
    scopes_variables: &[HashMap<StringIdx, (TypeGroup, bool, SourceRange)>],
    scopes_uninitialized_variables: &[HashMap<StringIdx, (TypeGroup, bool, SourceRange)>],
    scopes_returns: &[(SometimesReturns, AlwaysReturns)],
) -> Result<(), Error> {
    for variable_name in uninitialized_variables.keys().map(|s| *s).collect::<Vec<StringIdx>>() {
        let uninitialized_var = uninitialized_variables.get(&variable_name).expect("should exist");
        let (variable_types, variable_mutable, variable_source) = *uninitialized_var;
        let mut always_has_value = true;
        for scope_i in 0..scopes_uninitialized_variables.len() {
            if let Some(_) = scopes_uninitialized_variables[scope_i].get(&variable_name) {
                let branch_always_returns = scopes_returns[scope_i].1;
                if !branch_always_returns {
                    always_has_value = false;
                    break;
                }
                continue;
            }
            if let Some((scope_variable_types, _, scope_variable_source)) = scopes_variables[scope_i].get(&variable_name) {
                assert_types(
                    TypeAssertion::variable(variable_source, variable_types, type_scope, strings),
                    TypeAssertion::variable(*scope_variable_source, *scope_variable_types, type_scope, strings),
                    type_scope
                )?;
                continue;
            }
            panic!("the variable should exist either in 'variables' or in 'uninitialized_variables'");
        }
        if !always_has_value { continue; }
        uninitialized_variables.remove(&variable_name);
        variables.insert(variable_name, (variable_types, variable_mutable, variable_source));
    }
    Ok(())
}

fn type_check_node(
    strings: &StringMap,
    global_type_scope: &mut TypeScope,
    rec_procedures: &mut Vec<(NamespacePath, Vec<Vec<(TypeGroup, SourceRange)>>, TypeScope)>,
    procedure_source: SourceRange,
    variables: &mut HashMap<StringIdx, (TypeGroup, bool, SourceRange)>,
    scope_variables: &mut HashSet<StringIdx>,
    uninitialized_variables: &mut HashMap<StringIdx, (TypeGroup, bool, SourceRange)>,
    captured_variables: &mut HashSet<StringIdx>,
    untyped_symbols: &mut HashMap<NamespacePath, AstNode>,
    symbols: &mut HashMap<NamespacePath, Symbol<TypedAstNode>>,
    node: AstNode,
    return_types: TypeGroup,
    limited_to: Option<TypeAssertion>,
    assignment: bool
) -> Result<(TypedAstNode, (SometimesReturns, AlwaysReturns)), Error> {
    let node_source = node.source();
    macro_rules! type_scope { () => { {
        let idx = rec_procedures.len() - 1;
        &mut rec_procedures[idx].2
    } } }
    macro_rules! type_check_node { ($node: expr, $limited_to: expr) => {
        match type_check_node(strings, global_type_scope, rec_procedures, procedure_source, variables, scope_variables, uninitialized_variables, captured_variables, untyped_symbols, symbols, $node, return_types, $limited_to, assignment) {
            Ok(typed_node) => typed_node,
            Err(error) => return Err(error)
        }
    }; ($node: expr, $limited_to: expr, $assignment: expr) => {
        match type_check_node(strings, global_type_scope, rec_procedures, procedure_source, variables, scope_variables, uninitialized_variables, captured_variables, untyped_symbols, symbols, $node, return_types, $limited_to, $assignment) {
            Ok(typed_node) => typed_node,
            Err(error) => return Err(error)
        }
    }; ($node: expr, $limited_to: expr, $assignment: expr, $variables: expr) => {
        match type_check_node(strings, global_type_scope, rec_procedures, procedure_source, $variables, scope_variables, uninitialized_variables, captured_variables, untyped_symbols, symbols, $node, return_types, $limited_to, $assignment) {
            Ok(typed_node) => typed_node,
            Err(error) => return Err(error)
        }
    } }
    macro_rules! type_check_nodes { ($nodes: expr, $variables: expr, $scope_variables: expr, $uninitialized_variables: expr) => {
        match type_check_nodes(strings, global_type_scope, rec_procedures, procedure_source, $variables, $scope_variables, $uninitialized_variables, captured_variables, untyped_symbols, symbols, $nodes, return_types) {
            Ok(typed_node) => typed_node,
            Err(error) => return Err(error)
        }
    } }
    match node.move_node() {
        AstNodeVariant::Procedure { public: _, name: _, arguments: _, body: _ } => panic!("The grammar checker failed to see a procedure inside another!"),
        AstNodeVariant::Function { arguments, body } => {
            let mut closure_variables = variables.clone();
            let mut closure_scope_variables = HashSet::new();
            let mut closure_args = Vec::new();
            for argument in &arguments {
                let var_idx = type_scope!().insert_group(&[Type::Any]);
                closure_args.push(var_idx);
                closure_variables.insert(argument.0, (var_idx, false, argument.1));
                closure_scope_variables.insert(argument.0);
            }
            let return_types = type_scope!().insert_group(&[Type::Any]);
            let mut captured = HashSet::new();
            let (typed_body, returns) = match type_check_nodes(
                strings,
                global_type_scope,
                rec_procedures,
                procedure_source,
                &mut closure_variables,
                &mut closure_scope_variables,
                &mut uninitialized_variables.clone(),
                &mut captured,
                untyped_symbols,
                symbols,
                body,
                return_types
            ) {
                Ok(typed_nodes) => typed_nodes,
                Err(error) => return Err(error),
            };
            for capture in &captured {
                if !scope_variables.contains(capture) {
                    captured_variables.insert(*capture);
                }
            }
            if !returns.1 {
                assert_types(
                    TypeAssertion::returned_values(node_source, return_types, type_scope!(), strings),
                    TypeAssertion::implicit_unit_return(node_source, type_scope!(), strings),
                    type_scope!()
                )?;
            }
            let closure_tidx = type_scope!().insert_closure(
                closure_args,
                return_types,
                Some(captured.into_iter().map(|captured_name| (
                    captured_name,
                    variables.get(&captured_name).expect("variable should exist").0.clone()
                )).collect())
            );
            let closure_type = type_scope!().insert_group(&[Type::Closure(closure_tidx)]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::literal("closure", node_source, closure_type, type_scope!(), strings),
                    limited_to,
                    type_scope!()
                )?;
            }
            Ok((TypedAstNode::new(
                AstNodeVariant::Function {
                    arguments,
                    body: typed_body
                },
                closure_type,
                node_source
            ), (false, false)))
        }
        AstNodeVariant::Variable { public, mutable, name, value_types: _, value } => {
            let value_types = type_scope!().insert_group(&[Type::Any]);
            let typed_value = if let Some(value) = value {
                let typed_value = type_check_node!(*value, Some(TypeAssertion::unexplained(value_types))).0;
                variables.insert(name, (value_types, mutable, node_source));
                Some(Box::new(typed_value))
            } else {
                uninitialized_variables.insert(name, (value_types, mutable, node_source));
                None
            };
            scope_variables.insert(name);     
            Ok((TypedAstNode::new(AstNodeVariant::Variable {
                public,
                mutable,
                name,
                value_types: Some(value_types),
                value: typed_value
            }, type_scope!().insert_group(&[Type::Unit]), node_source), (false, false)))
        }
        AstNodeVariant::CaseBranches { value, branches, else_body } => {
            let typed_value = type_check_node!(*value, None).0;
            let mut typed_branches = Vec::new();
            let mut branches_return = Vec::new();
            let mut branches_variables = Vec::new();
            let mut branches_uninitialized_variables = Vec::new();
            for (branch_value, branch_body) in branches {
                let mut branch_variables = variables.clone();
                let mut branch_uninitialized_variables = uninitialized_variables.clone();
                let (branch_body, branch_returns) = type_check_nodes!(branch_body, &mut branch_variables, &mut scope_variables.clone(), &mut branch_uninitialized_variables);
                branches_return.push(branch_returns);
                let matched_assertion = TypeAssertion::matched_value(node_source, typed_value.get_types(), type_scope!(), strings);
                typed_branches.push((type_check_node!(branch_value, Some(matched_assertion), false, &mut HashMap::new()).0, branch_body));
                branches_variables.push(branch_variables);
                branches_uninitialized_variables.push(branch_uninitialized_variables);
            }
            let mut else_body_variables = variables.clone();
            let mut else_body_uninitialized_variables = uninitialized_variables.clone();
            let (typed_else_body, else_returns) = type_check_nodes!(else_body, &mut else_body_variables, &mut scope_variables.clone(), &mut else_body_uninitialized_variables);
            branches_return.push(else_returns);
            branches_variables.push(else_body_variables);
            branches_uninitialized_variables.push(else_body_uninitialized_variables);
            initalize_variables(
                strings, type_scope!(), variables, uninitialized_variables,
                &branches_variables,
                &branches_uninitialized_variables,
                &branches_return
            )?;
            Ok((
                TypedAstNode::new(AstNodeVariant::CaseBranches {
                    value: Box::new(typed_value),
                    branches: typed_branches,
                    else_body: typed_else_body
                },
                type_scope!().insert_group(&[Type::Unit]), node_source),
                (branches_return.iter().find(|r| r.0).is_some(), branches_return.iter().find(|r| !r.1).is_none())
            ))
        }
        AstNodeVariant::CaseConditon { condition, body, else_body } => {
            let boolean = type_scope!().insert_group(&[Type::Boolean]);
            let cond_assertion = TypeAssertion::condition(node_source, boolean, type_scope!(), strings);
            let typed_condition = type_check_node!(*condition, Some(cond_assertion)).0;
            let mut body_variables = variables.clone();
            let mut body_uninitialized_variables = uninitialized_variables.clone();
            let (typed_body, body_returns) = type_check_nodes!(body, &mut body_variables, &mut scope_variables.clone(), &mut body_uninitialized_variables);
            let mut else_body_variables = variables.clone();
            let mut else_body_uninitialized_variables = uninitialized_variables.clone();
            let (typed_else_body, else_returns) = type_check_nodes!(else_body, &mut else_body_variables, &mut scope_variables.clone(), &mut else_body_uninitialized_variables);
            initalize_variables(
                strings, type_scope!(), variables, uninitialized_variables,
                &[body_variables, else_body_variables],
                &[body_uninitialized_variables, else_body_uninitialized_variables],
                &[body_returns, else_returns]
            )?;
            Ok((TypedAstNode::new(AstNodeVariant::CaseConditon {
                condition: Box::new(typed_condition),
                body: typed_body,
                else_body: typed_else_body
            }, type_scope!().insert_group(&[Type::Unit]), node_source), (body_returns.0 || else_returns.0, body_returns.1 && else_returns.1)))
        }
        AstNodeVariant::CaseVariant { value, branches, else_body } => {
            let mut typed_branches = Vec::new();
            let mut branches_return = Vec::new();
            let mut branches_variables = Vec::new();
            let mut branches_uninitialized_variables = Vec::new();
            let mut variant_types = HashMap::new();
            for (branch_variant_name, branch_variant_variable, branch_body) in branches {
                let mut branch_variables = variables.clone();
                let branch_variant_variable_types = type_scope!().insert_group(&[Type::Any]);
                let mut branch_scope_variables = scope_variables.clone();
                if let Some(branch_variant_variable) = &branch_variant_variable {
                    branch_variables.insert(branch_variant_variable.0, (branch_variant_variable_types, false, branch_variant_variable.1));
                    branch_scope_variables.insert(branch_variant_variable.0);
                }
                let mut branch_uninitialized_variables = uninitialized_variables.clone();
                let (branch_body, branch_returns) = type_check_nodes!(branch_body, &mut branch_variables, &mut branch_scope_variables, &mut branch_uninitialized_variables);
                branches_return.push(branch_returns);
                typed_branches.push((branch_variant_name, branch_variant_variable.map(|v| (v.0, v.1, Some(branch_variant_variable_types))), branch_body));
                branches_variables.push(branch_variables);
                branches_uninitialized_variables.push(branch_uninitialized_variables);
                variant_types.insert(branch_variant_name, branch_variant_variable_types);
            }
            let variant_tidx = type_scope!().insert_variants(variant_types, else_body.is_none());
            let variant_types = type_scope!().insert_group(&[Type::Variants(variant_tidx)]);
            let variant_assertion = TypeAssertion::branch_variants(node_source, variant_types, type_scope!(), strings);
            let typed_value = type_check_node!(*value, Some(variant_assertion)).0;
            let typed_else_body = if let Some(else_body) = else_body {
                let mut else_body_variables = variables.clone();
                let mut else_body_uninitialized_variables = uninitialized_variables.clone();
                let (typed_else_body, else_returns) = type_check_nodes!(else_body, &mut else_body_variables, &mut scope_variables.clone(), &mut else_body_uninitialized_variables);
                branches_variables.push(else_body_variables);
                branches_uninitialized_variables.push(else_body_uninitialized_variables);
                branches_return.push(else_returns);
                Some(typed_else_body)
            } else { None };
            initalize_variables(
                strings, type_scope!(), variables, uninitialized_variables,
                &branches_variables,
                &branches_uninitialized_variables,
                &branches_return
            )?;
            Ok((
                TypedAstNode::new(AstNodeVariant::CaseVariant {
                    value: Box::new(typed_value),
                    branches: typed_branches,
                    else_body: typed_else_body
                },
                type_scope!().insert_group(&[Type::Unit]), node_source),
                (branches_return.iter().find(|r| r.0).is_some(), branches_return.iter().find(|r| !r.1).is_none())
            ))
        }
        AstNodeVariant::Assignment { variable, value } => {
            let typed_value = type_check_node!(*value, None).0;
            let assigned_val_assertion = TypeAssertion::assigned_value(typed_value.source(), typed_value.get_types(), type_scope!(), strings);
            let typed_variable = type_check_node!(*variable, Some(assigned_val_assertion), true).0;
            Ok((TypedAstNode::new(AstNodeVariant::Assignment {
                variable: Box::new(typed_variable),
                value: Box::new(typed_value)
            }, type_scope!().insert_group(&[Type::Unit]), node_source), (false, false)))
        }
        AstNodeVariant::Return { value } => {
            let ret_vals_assertion = TypeAssertion::returned_values(procedure_source, return_types, type_scope!(), strings);
            let typed_value = type_check_node!(
                *value,
                Some(ret_vals_assertion)
            ).0;
            Ok((TypedAstNode::new(AstNodeVariant::Return {
                value: Box::new(typed_value)
            }, type_scope!().insert_group(&[Type::Unit]), node_source), (true, true)))
        }
        AstNodeVariant::Call { called, mut arguments } => {
            if let AstNodeVariant::ModuleAccess { path } = called.node_variant() {
                match type_check_symbol(strings, global_type_scope, rec_procedures, untyped_symbols, symbols, &path).map(|s| s.clone()) {
                    Ok(Symbol::Procedure { public: _, parameter_names, parameter_types, returns, body: _, source: _, type_scope: symbol_type_scope }) => {
                        if arguments.len() != parameter_types.len() { return Err(Error::new([
                            ErrorSection::Error(ErrorType::InvalidParameterCount(path.display(strings), parameter_types.len(), arguments.len())),
                            ErrorSection::Code(node_source)
                        ].into())) }
                        if let Some(rec_proc_idx) = rec_procedures
                                .iter().position(|p| p.0 == *path) {
                            let mut typed_arguments = Vec::new();
                            for argument_idx in 0..arguments.len() {
                                let typed_arg = type_check_node!(arguments.remove(0), None).0;
                                let typed_arg_types = type_scope!().clone().transfer_group(typed_arg.get_types(), &mut rec_procedures[rec_proc_idx].2);
                                rec_procedures[rec_proc_idx].1[argument_idx].push(
                                    (typed_arg_types, typed_arg.source())
                                );
                                typed_arguments.push(typed_arg);
                            }
                            let returned_types = rec_procedures[rec_proc_idx].2.clone().transfer_group(returns, type_scope!());
                            if let Some(limited_to) = limited_to {
                                assert_types(
                                    TypeAssertion::call_return_value(node_source, returned_types, type_scope!(), strings),
                                    limited_to, type_scope!()
                                )?;
                            }
                            let called = TypedAstNode::new(
                                AstNodeVariant::ModuleAccess { path: path.clone() },
                                type_scope!().insert_group(&[Type::Unit]),
                                called.source()
                            );
                            return Ok((TypedAstNode::new(AstNodeVariant::Call {
                                called: Box::new(called),
                                arguments: typed_arguments
                            }, returned_types, node_source), (false, false)));
                        } else {
                            let mut typed_arguments = Vec::new();
                            for argument_idx in 0..arguments.len() {
                                let param_types = symbol_type_scope.transfer_group(parameter_types[argument_idx], type_scope!());
                                let call_param_assertion = TypeAssertion::call_parameter(
                                    node_source, parameter_names[argument_idx],
                                    param_types,
                                    type_scope!(), strings
                                );
                                typed_arguments.push(type_check_node!(
                                    arguments.remove(0),
                                    Some(call_param_assertion)
                                ).0);
                            }
                            let returned_types = symbol_type_scope.transfer_group(returns, type_scope!());
                            if let Some(limited_to) = limited_to {
                                assert_types(
                                    TypeAssertion::call_return_value(node_source, returned_types, type_scope!(), strings),
                                    limited_to, type_scope!()
                                )?;
                            }
                            let called = TypedAstNode::new(
                                AstNodeVariant::ModuleAccess { path: path.clone() },
                                type_scope!().insert_group(&[Type::Unit]),
                                called.source()
                            );
                            return Ok((TypedAstNode::new(AstNodeVariant::Call {
                                called: Box::new(called),
                                arguments: typed_arguments
                            }, returned_types, node_source), (false, false)));
                        }
                    }
                    Ok(_) => {}
                    Err(error) => return Err(error)
                }
            }
            let mut typed_arguments = Vec::new();
            let mut passed_arg_vars = Vec::new();
            for argument in arguments {
                let typed_argument = type_check_node!(argument, None).0;
                passed_arg_vars.push(typed_argument.get_types());
                typed_arguments.push(typed_argument);
            }
            let passed_return_type = type_scope!().insert_group(&[Type::Any]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::unexplained(passed_return_type),
                    limited_to, type_scope!()
                ).expect("should not fail");
            }
            let closure_tidx = type_scope!().insert_closure(
                passed_arg_vars, passed_return_type, None
            );
            let closure_types = type_scope!().insert_group(&[Type::Closure(closure_tidx)]);
            let called_closure_assertion = TypeAssertion::called_closure(node_source, closure_types, type_scope!(), strings);
            let typed_called = type_check_node!(
                *called,
                Some(called_closure_assertion)
            ).0;
            Ok((TypedAstNode::new(AstNodeVariant::Call {
                called: Box::new(typed_called),
                arguments: typed_arguments
            }, passed_return_type, node_source), (false, false)))
        }
        AstNodeVariant::Object { values } => {
            let mut member_types = HashMap::new();
            let mut typed_values = Vec::new();
            for (member_name, member_value) in values {
                let typed_member_value = type_check_node!(member_value, None).0;
                member_types.insert(member_name, typed_member_value.get_types().clone());
                typed_values.push((member_name, typed_member_value));
            }
            let object_tidx = type_scope!().insert_object(member_types, true);
            let object_type = type_scope!().insert_group(&[Type::Object(object_tidx)]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::literal("object", node_source, object_type, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            Ok((TypedAstNode::new(AstNodeVariant::Object {
                values: typed_values
            }, object_type, node_source), (false, false)))
        }
        AstNodeVariant::Array { values } => {
            let element_types = type_scope!().insert_group(&[Type::Any]);
            let mut typed_values = Vec::new();
            for value in values {
                let array_values_assertion = TypeAssertion::array_values(node_source, element_types, type_scope!(), strings);
                let typed_value = type_check_node!(value, Some(array_values_assertion)).0;
                typed_values.push(typed_value);
            }
            let array_tidx = type_scope!().insert_array(element_types);
            let array_type = type_scope!().insert_group(&[Type::Array(array_tidx)]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::literal("array", node_source, array_type, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            Ok((TypedAstNode::new(AstNodeVariant::Array {
                values: typed_values
            }, array_type, node_source), (false, false)))
        }
        AstNodeVariant::ObjectAccess { object, member } => {
            let accessed_object_member_types = type_scope!().insert_group(&[Type::Any]);
            let accessed_object_tidx = type_scope!().insert_object([(member, accessed_object_member_types)].into(), false);
            let accessed_object_types = type_scope!().insert_group(&[Type::Object(accessed_object_tidx)]);
            let accessed_object_assertion = TypeAssertion::accessed_object(node_source, accessed_object_types, type_scope!(), strings);
            let typed_object = type_check_node!(*object, Some(accessed_object_assertion), false).0;
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::access_result(node_source, accessed_object_member_types, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            Ok((TypedAstNode::new(AstNodeVariant::ObjectAccess {
                object: Box::new(typed_object),
                member
            }, accessed_object_member_types, node_source), (false, false)))
        }
        AstNodeVariant::ArrayAccess { array, index } => {
            let accessed_array_element_types = type_scope!().insert_group(&[Type::Any]);
            let accessed_array_tidx = type_scope!().insert_array(accessed_array_element_types);
            let accessed_array_types = type_scope!().insert_group(&[Type::Array(accessed_array_tidx)]);
            let typed_array = type_check_node!(*array, Some(TypeAssertion::accessed_array(node_source, accessed_array_types)), false).0;
            let index_types = type_scope!().insert_group(&[Type::Integer]);
            let array_index_assertion = TypeAssertion::array_index(node_source, index_types, type_scope!(), strings);
            let typed_index = type_check_node!(*index, Some(array_index_assertion), false).0;
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::access_result(node_source, accessed_array_element_types, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            Ok((TypedAstNode::new(AstNodeVariant::ArrayAccess {
                array: Box::new(typed_array),
                index: Box::new(typed_index)
            }, accessed_array_element_types, node_source), (false, false)))
        }
        AstNodeVariant::VariableAccess { name } => {
            if !scope_variables.contains(&name) {
                captured_variables.insert(name);
            }
            if let Some((variable_types, variable_mutable, variable_source)) = variables.get_mut(&name) {
                let variable_types = *variable_types;
                if assignment && !*variable_mutable {
                    Err(Error::new([
                        ErrorSection::Error(ErrorType::ImmutableAssignmant(name)),
                        ErrorSection::Code(node_source)
                    ].into()))
                } else {
                    if let Some(limited_to) = limited_to {
                        assert_types(
                            TypeAssertion::variable(*variable_source, variable_types, type_scope!(), strings), 
                            limited_to, type_scope!()
                        )?;
                    }
                    Ok((TypedAstNode::new(
                        AstNodeVariant::VariableAccess { name },
                        variable_types,
                        node_source
                    ), (false, false)))
                }
            } else if let Some((variable_types, variable_mutable, variable_source)) = uninitialized_variables.remove(&name) {
                if assignment {
                    if let Some(limited_to) = limited_to {
                        assert_types(
                            TypeAssertion::variable(variable_source, variable_types, type_scope!(), strings), 
                            limited_to, type_scope!()
                        )?;
                    }
                    variables.insert(name, (variable_types, variable_mutable, variable_source));
                    Ok((TypedAstNode::new(
                        AstNodeVariant::VariableAccess { name },
                        variable_types,
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
            let boolean = type_scope!().insert_group(&[Type::Boolean]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::literal("boolean", node_source, boolean, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            Ok((TypedAstNode::new(
                AstNodeVariant::BooleanLiteral { value },
                boolean,
                node_source
            ), (false, false)))
        }
        AstNodeVariant::IntegerLiteral { value } => {
            let integer = type_scope!().insert_group(&[Type::Integer]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::literal("integer", node_source, integer, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            Ok((TypedAstNode::new(
                AstNodeVariant::IntegerLiteral { value },
                integer,
                node_source
            ), (false, false)))
        }
        AstNodeVariant::FloatLiteral { value } => {
            let float = type_scope!().insert_group(&[Type::Float]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::literal("float", node_source, float, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            Ok((TypedAstNode::new(
                AstNodeVariant::FloatLiteral { value },
                float,
                node_source
            ), (false, false)))
        }
        AstNodeVariant::StringLiteral { value } => {
            let string = type_scope!().insert_group(&[Type::String]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::literal("string", node_source, string, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            Ok((TypedAstNode::new(
                AstNodeVariant::StringLiteral { value },
                string,
                node_source
            ), (false, false)))
        }
        AstNodeVariant::UnitLiteral => {
            let unit = type_scope!().insert_group(&[Type::Unit]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::literal("unit", node_source, unit, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            Ok((TypedAstNode::new(
                AstNodeVariant::UnitLiteral,
                unit,
                node_source
            ), (false, false)))
        }
        AstNodeVariant::Add { a, b } => {
            let op_type = type_scope!().insert_group(&[Type::Integer, Type::Float]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::arithmetic_result(node_source, op_type, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            let assertion = TypeAssertion::arithmetic_argument(node_source, op_type, type_scope!(), strings);
            let a_typed = type_check_node!(*a, Some(assertion.clone())).0;
            let b_typed = type_check_node!(*b, Some(assertion)).0;
            Ok((TypedAstNode::new(AstNodeVariant::Add {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, op_type, node_source), (false, false)))
        }
        AstNodeVariant::Subtract { a, b } => {
            let op_type = type_scope!().insert_group(&[Type::Integer, Type::Float]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::arithmetic_result(node_source, op_type, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            let assertion = TypeAssertion::arithmetic_argument(node_source, op_type, type_scope!(), strings);
            let a_typed = type_check_node!(*a, Some(assertion.clone())).0;
            let b_typed = type_check_node!(*b, Some(assertion)).0;
            Ok((TypedAstNode::new(AstNodeVariant::Subtract {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, op_type, node_source), (false, false)))
        }
        AstNodeVariant::Multiply { a, b } => {
            let op_type = type_scope!().insert_group(&[Type::Integer, Type::Float]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::arithmetic_result(node_source, op_type, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            let assertion = TypeAssertion::arithmetic_argument(node_source, op_type, type_scope!(), strings);
            let a_typed = type_check_node!(*a, Some(assertion.clone())).0;
            let b_typed = type_check_node!(*b, Some(assertion)).0;
            Ok((TypedAstNode::new(AstNodeVariant::Multiply {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, op_type, node_source), (false, false)))
        }
        AstNodeVariant::Divide { a, b } => {
            let op_type = type_scope!().insert_group(&[Type::Integer, Type::Float]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::arithmetic_result(node_source, op_type, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            let assertion = TypeAssertion::arithmetic_argument(node_source, op_type, type_scope!(), strings);
            let a_typed = type_check_node!(*a, Some(assertion.clone())).0;
            let b_typed = type_check_node!(*b, Some(assertion)).0;
            Ok((TypedAstNode::new(AstNodeVariant::Divide {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, op_type, node_source), (false, false)))
        }
        AstNodeVariant::Modulo { a, b } => {
            let op_type = type_scope!().insert_group(&[Type::Integer, Type::Float]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::arithmetic_result(node_source, op_type, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            let assertion = TypeAssertion::arithmetic_argument(node_source, op_type, type_scope!(), strings);
            let a_typed = type_check_node!(*a, Some(assertion.clone())).0;
            let b_typed = type_check_node!(*b, Some(assertion)).0;
            Ok((TypedAstNode::new(AstNodeVariant::Modulo {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, op_type, node_source), (false, false)))
        }
        AstNodeVariant::Negate { x } => {
            let op_type = type_scope!().insert_group(&[Type::Integer, Type::Float]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::arithmetic_result(node_source, op_type, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            let assertion = TypeAssertion::arithmetic_argument(node_source, op_type, type_scope!(), strings);
            let x_typed = type_check_node!(*x, Some(assertion)).0;
            Ok((TypedAstNode::new(AstNodeVariant::Negate {
                x: Box::new(x_typed),
            }, op_type, node_source), (false, false)))
        }
        AstNodeVariant::LessThan { a, b } => {
            let boolean = type_scope!().insert_group(&[Type::Boolean]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::comparison_result(node_source, boolean, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            let arg_types = type_scope!().insert_group(&[Type::Integer, Type::Float]);
            let assertion = TypeAssertion::comparison_argument(node_source, arg_types, type_scope!(), strings);
            let a_typed = type_check_node!(*a, Some(assertion.clone())).0;
            let b_typed = type_check_node!(*b, Some(assertion)).0;
            Ok((TypedAstNode::new(AstNodeVariant::LessThan {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, boolean, node_source), (false, false)))
        }
        AstNodeVariant::LessThanEqual { a , b } => {
            let boolean = type_scope!().insert_group(&[Type::Boolean]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::comparison_result(node_source, boolean, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            let arg_types = type_scope!().insert_group(&[Type::Integer, Type::Float]);
            let assertion = TypeAssertion::comparison_argument(node_source, arg_types, type_scope!(), strings);
            let a_typed = type_check_node!(*a, Some(assertion.clone())).0;
            let b_typed = type_check_node!(*b, Some(assertion)).0;
            Ok((TypedAstNode::new(AstNodeVariant::LessThanEqual {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, boolean, node_source), (false, false)))
        }
        AstNodeVariant::GreaterThan { a, b } => {
            let boolean = type_scope!().insert_group(&[Type::Boolean]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::comparison_result(node_source, boolean, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            let arg_types = type_scope!().insert_group(&[Type::Integer, Type::Float]);
            let assertion = TypeAssertion::comparison_argument(node_source, arg_types, type_scope!(), strings);
            let a_typed = type_check_node!(*a, Some(assertion.clone())).0;
            let b_typed = type_check_node!(*b, Some(assertion)).0;
            Ok((TypedAstNode::new(AstNodeVariant::GreaterThan {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, boolean, node_source), (false, false)))
        }
        AstNodeVariant::GreaterThanEqual { a, b } => {
            let boolean = type_scope!().insert_group(&[Type::Boolean]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::comparison_result(node_source, boolean, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            let arg_types = type_scope!().insert_group(&[Type::Integer, Type::Float]);
            let assertion = TypeAssertion::comparison_argument(node_source, arg_types, type_scope!(), strings);
            let a_typed = type_check_node!(*a, Some(assertion.clone())).0;
            let b_typed = type_check_node!(*b, Some(assertion)).0;
            Ok((TypedAstNode::new(AstNodeVariant::GreaterThanEqual {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, boolean, node_source), (false, false)))
        }
        AstNodeVariant::Equals { a, b } => {
            let boolean = type_scope!().insert_group(&[Type::Boolean]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::comparison_result(node_source, boolean, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            let arg_types = type_scope!().insert_group(&[Type::Any]);
            let assertion = TypeAssertion::comparison_argument(node_source, arg_types, type_scope!(), strings);
            let a_typed = type_check_node!(*a, Some(assertion.clone())).0;
            let b_typed = type_check_node!(*b, Some(assertion)).0;
            Ok((TypedAstNode::new(AstNodeVariant::Equals {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, boolean, node_source), (false, false)))
        }
        AstNodeVariant::NotEquals { a, b } => {
            let boolean = type_scope!().insert_group(&[Type::Boolean]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::comparison_result(node_source, boolean, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            let arg_types = type_scope!().insert_group(&[Type::Any]);
            let assertion = TypeAssertion::comparison_argument(node_source, arg_types, type_scope!(), strings);
            let a_typed = type_check_node!(*a, Some(assertion.clone())).0;
            let b_typed = type_check_node!(*b, Some(assertion)).0;
            Ok((TypedAstNode::new(AstNodeVariant::NotEquals {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, boolean, node_source), (false, false)))
        }
        AstNodeVariant::And { a, b } => {
            let boolean = type_scope!().insert_group(&[Type::Boolean]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::logical_result(node_source, boolean, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            let assertion = TypeAssertion::logical_argument(node_source, boolean, type_scope!(), strings);
            let a_typed = type_check_node!(*a, Some(assertion.clone())).0;
            let b_typed = type_check_node!(*b, Some(assertion)).0;
            Ok((TypedAstNode::new(AstNodeVariant::And {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, boolean, node_source), (false, false)))
        }
        AstNodeVariant::Or { a, b } => {
            let boolean = type_scope!().insert_group(&[Type::Boolean]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::logical_result(node_source, boolean, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            let assertion = TypeAssertion::logical_argument(node_source, boolean, type_scope!(), strings);
            let a_typed = type_check_node!(*a, Some(assertion.clone())).0;
            let b_typed = type_check_node!(*b, Some(assertion)).0;
            Ok((TypedAstNode::new(AstNodeVariant::Or {
                a: Box::new(a_typed),
                b: Box::new(b_typed)
            }, boolean, node_source), (false, false)))
        }
        AstNodeVariant::Not { x } => {
            let boolean = type_scope!().insert_group(&[Type::Boolean]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::logical_result(node_source, boolean, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            let assertion = TypeAssertion::logical_argument(node_source, boolean, type_scope!(), strings);
            let x_typed = type_check_node!(*x, Some(assertion)).0;
            Ok((TypedAstNode::new(AstNodeVariant::Not {
                x: Box::new(x_typed),
            }, boolean, node_source), (false, false)))
        }
        AstNodeVariant::Module { path } => {
            Ok((TypedAstNode::new(AstNodeVariant::Module {
                path
            }, type_scope!().insert_group(&[Type::Unit]), node_source), (false, false)))
        }
        AstNodeVariant::ModuleAccess { path } => {
            match type_check_symbol(strings, global_type_scope, rec_procedures, untyped_symbols, symbols, &path) {
                Ok(Symbol::Constant { public: _, value: _, value_types }) => {
                    let value_types = global_type_scope.transfer_group(*value_types, type_scope!());
                    if let Some(limited_to) = limited_to {
                        assert_types(
                            TypeAssertion::constant(node_source, value_types, type_scope!(), strings),
                            limited_to, type_scope!()
                        )?;
                    }
                    Ok((TypedAstNode::new(AstNodeVariant::ModuleAccess {
                        path
                    }, value_types, node_source), (false, false)))
                }
                Ok(Symbol::Procedure { public: _, parameter_names, parameter_types: _, returns: _, body: _, source: _, type_scope: _ }) => {
                    // 'io::println' --> '|x| io::println(x)'
                    let parameter_names = parameter_names.clone();
                    Ok(type_check_node!(AstNode::new(
                        AstNodeVariant::Function {
                            arguments: parameter_names.iter().map(|n| (*n, node_source))
                                .collect(),
                            body: vec![AstNode::new(
                                AstNodeVariant::Return {
                                    value: AstNode::new(
                                        AstNodeVariant::Call {
                                            called: AstNode::new(
                                                AstNodeVariant::ModuleAccess { path },
                                                node_source
                                            ).into(),
                                            arguments: parameter_names.iter()
                                                .map(|n| AstNode::new(
                                                    AstNodeVariant::VariableAccess { name: *n },
                                                    node_source
                                                ))
                                                .collect()
                                        },
                                        node_source
                                    ).into()
                                },
                                node_source
                            )]
                        },
                        node_source
                    ), None))
                }
                Err(error) => return Err(error)
            }
        }
        AstNodeVariant::Use { paths } => {
            Ok((TypedAstNode::new(AstNodeVariant::Use {
                paths
            }, type_scope!().insert_group(&[Type::Unit]), node_source), (false, false)))
        }
        AstNodeVariant::Variant { name, value } => {
            let value_typed = type_check_node!(*value, None).0;
            let variant_tidx = type_scope!().insert_variants(
                [(name, value_typed.get_types().clone())].into(), false
            );
            let variant_types = type_scope!().insert_group(&[Type::Variants(variant_tidx)]);
            if let Some(limited_to) = limited_to {
                assert_types(
                    TypeAssertion::literal("tag", node_source, variant_types, type_scope!(), strings),
                    limited_to, type_scope!()
                )?;
            }
            Ok((TypedAstNode::new(AstNodeVariant::Variant {
                name,
                value: Box::new(value_typed),
            }, variant_types, node_source), (false, false)))
        }
        AstNodeVariant::Static { value } => {
            let value_typed = type_check_node!(*value, limited_to, assignment, &mut HashMap::new());
            let value_types = value_typed.0.get_types();
            Ok((TypedAstNode::new(AstNodeVariant::Static {
                value: Box::new(value_typed.0),
            }, value_types, node_source), value_typed.1))
        }
        AstNodeVariant::Target { target: _, body: _ } => {
            panic!("Should've been expanded!");
        }
    }
}

pub fn display_types(
    strings: &StringMap,
    type_scope: &TypeScope,
    types: TypeGroup
) -> String {
    fn choose_letter(i: usize) -> String {
        const LETTERS: [char; 26] = [
            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q',
            'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
        ];
        let mut i = i;
        let mut r = String::new();
        loop {
            let c = i % LETTERS.len();
            r.push(LETTERS[c]);
            i = i / LETTERS.len();
            if i == 0 { break; }
        }
        r
    }
    fn collect_letters(
        letters: &mut HashMap<usize, (String, usize)>,
        types: TypeGroup,
        type_scope: &TypeScope
    ) {
        let group_internal_idx = type_scope.group_internal_id(types);
        if let Some((_, usages)) = letters.get_mut(&group_internal_idx) {
            *usages += 1;
            if *usages >= 2 { return; }
        } else {
            let letter = choose_letter(letters.len());
            letters.insert(group_internal_idx, (letter, 1));
        }
        for possible_type in type_scope.group(types) {
            collect_type_letters(letters, &possible_type, type_scope)
        }
    }
    fn collect_type_letters(
        letters: &mut HashMap<usize, (String, usize)>,
        collected_type: &Type,
        type_scope: &TypeScope
    ) {
        match collected_type {
            Type::Any |
            Type::Unit |
            Type::Boolean |
            Type::Integer |
            Type::Float |
            Type::String => {}
            Type::Array(arr) => collect_letters(letters, type_scope.array(*arr), type_scope),
            Type::Object(obj) => {
                for member_types in type_scope.object(*obj).0.values().map(|t| *t).collect::<Vec<TypeGroup>>() {
                    collect_letters(letters, member_types, type_scope);
                }
            }
            Type::ConcreteObject(obj) => {
                for (_, member_types) in type_scope.concrete_object(*obj) {
                    collect_letters(letters, *member_types, type_scope);
                }
            }
            Type::Closure(clo) => {
                let (parameter_types, return_types, _) = type_scope.closure(*clo);
                for parameter_types in parameter_types {
                    collect_letters(letters, *parameter_types, type_scope);
                }
                collect_letters(letters, *return_types, type_scope);
            }
            Type::Variants(var) => {
                for variant_types in type_scope.variants(*var).0.values().map(|t| *t).collect::<Vec<TypeGroup>>() {
                    collect_letters(letters, variant_types, type_scope);
                }
            }
        }
    }
    fn display_group_types(
        group_types: &[Type],
        strings: &StringMap,
        type_scope: &TypeScope,
        letters: &HashMap<usize, (String, usize)>
    ) -> String {
        let mut result = String::new();
        if group_types.len() > 1 { 
            result.push_str("(");
        }
        for i in 0..group_types.len() {
            if i > 0 { result.push_str(" | "); }
            result.push_str(&display_type(strings, type_scope, &group_types[i], letters));
        }
        if group_types.len() > 1 { 
            result.push_str(")");
        }
        result
    }
    fn display_type(
        strings: &StringMap,
        type_scope: &TypeScope,
        displayed_type: &Type,
        letters: &HashMap<usize, (String, usize)>
    ) -> String {
        match displayed_type {
            Type::Any => String::from("any"),
            Type::Unit => String::from("unit"),
            Type::Boolean => String::from("boolean"),
            Type::Integer => String::from("integer"),
            Type::Float => String::from("float"),
            Type::String => String::from("string"),
            Type::Array(arr) => format!(
                "[{}]",
                display_types_internal(strings, type_scope, type_scope.array(*arr), letters)
            ),
            Type::Object(obj) => {
                let (member_types, fixed) = type_scope.object(*obj);
                format!(
                    "{{ {}{} }}",
                    member_types.iter().map(|(member_name, member_type)| { format!(
                        "{} = {}",
                        strings.get(*member_name),
                        display_types_internal(strings, type_scope, *member_type, letters)
                    ) }).collect::<Vec<String>>().join(", "),
                    if *fixed { "" } else { ", ..." }
                )
            }
            Type::ConcreteObject(obj) => format!(
                "{{ {}, ... }}",
                type_scope.concrete_object(*obj).iter().map(|(member_name, member_type)| { format!(
                    "{} = {}",
                    strings.get(*member_name),
                    display_types_internal(strings, type_scope, *member_type, letters)
                ) }).collect::<Vec<String>>().join(", ")
            ),
            Type::Closure(clo) => {
                let (arg_groups, returned_group, _) = type_scope.closure(*clo);
                let mut result: String = String::from("(");
                for a in 0..arg_groups.len() {
                    if a > 0 { result.push_str(", "); }
                    result.push_str(&display_types_internal(strings, type_scope, arg_groups[a], letters));
                }
                result.push_str(") -> ");
                result.push_str(&display_types_internal(strings, type_scope, *returned_group, letters));
                result
            },
            Type::Variants(var) => {
                let (variant_types, fixed) = type_scope.variants(*var);
                format!(
                    "({}{})",
                    variant_types.iter().map(|(variant_name, variant_type)| {
                        format!(
                            "#{} {}",
                            strings.get(*variant_name),
                            display_types_internal(strings, type_scope, *variant_type, letters)
                        )
                    }).collect::<Vec<String>>().join(" | "),
                    if *fixed { "" } else { " | ..." }
                )
            }
        }
    }
    fn display_types_internal(
        strings: &StringMap,
        type_scope: &TypeScope,
        types: TypeGroup,
        letters: &HashMap<usize, (String, usize)>
    ) -> String {
        let group_internal_idx = type_scope.group_internal_id(types);
        if let Some((letter, usage_count)) = letters.get(&group_internal_idx) {
            if *usage_count >= 2 {
                return letter.clone();
            }
        }
        display_group_types(&type_scope.group(types).collect::<Vec<Type>>(), strings, type_scope, letters)
    }
    let mut letters = HashMap::new();
    collect_letters(&mut letters, types, type_scope);
    let mut result = display_types_internal(strings, type_scope, types, &letters);
    let mut letter_types = String::new();
    for (internal_group_idx, (letter, usage_count)) in &letters {
        if *usage_count < 2 { continue; }
        if letter_types.len() > 0 { letter_types.push_str(", "); }
        letter_types.push_str(letter);
        letter_types.push_str(" = ");
        letter_types.push_str(&display_group_types(
            &type_scope.internal_groups()[*internal_group_idx].iter().map(|t| *t).collect::<Vec<Type>>(),
            strings, type_scope, &letters
        ));
    }   
    if letter_types.len() > 0 {
        result.push_str(" where ");
        result.push_str(&letter_types);
    }
    result
}