use std::{rc::Rc, cell::RefCell, collections::HashMap};

use crate::util::{
    strings::{StringIdx, StringMap},
    error::{Error, ErrorSection, ErrorType},
    source::HasSource
};
use crate::frontend::{
    ast::{TypedAstNode, HasAstNodeVariant, AstNodeVariant},
    modules::NamespacePath,
    type_checking::Symbol
};


#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(Rc<str>),
    Array(Rc<RefCell<Box<[Value]>>>),
    Object(Rc<RefCell<HashMap<StringIdx, Value>>>),
    Closure(Vec<StringIdx>, Vec<HashMap<StringIdx, Value>>, Vec<TypedAstNode>),
    Variant(StringIdx, Box<Value>)
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Boolean(a), Value::Boolean(b)) => *a == *b,
            (Value::Integer(a), Value::Integer(b)) => *a == *b,
            (Value::Float(a), Value::Float(b)) => *a == *b,
            (Value::String(a), Value::String(b)) => **a == **b,
            (
                Value::Array(a),
                Value::Array(b)
            ) => Rc::ptr_eq(a, b),
            (
                Value::Object(a),
                Value::Object(b)
            ) => Rc::ptr_eq(a, b),
            (
                Value::Closure(a0, a1, a2),
                Value::Closure(b0, b1, b2)
            ) => *a0 == *b0 && *a1 == *b1 && *a2 == *b2,
            (
                Value::Variant(a0, a1),
                Value::Variant(b0, b1)
            ) => *a0 == *b0 && *a1 == *b1,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Interpreter {
    constants: HashMap<NamespacePath, Value>,
    stack: Vec<HashMap<StringIdx, Value>>,
    returned_value: Option<Value>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            constants: HashMap::new(),
            stack: Vec::new(),
            returned_value: None
        }
    }

    pub fn get_return_value(&mut self) -> Value {
        let value = self.returned_value.clone().unwrap_or(Value::Unit);
        self.returned_value = None;
        value
    }

    pub fn get_constant_value(&self, path: &NamespacePath) -> Option<&Value> {
        self.constants.get(path)
    }

    pub fn evaluate_nodes(
        &mut self,
        nodes: &Vec<TypedAstNode>,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &StringMap
    ) -> Option<Error> {
        self.stack.push(HashMap::new());
        for node in nodes {
            if self.returned_value.is_some() { break; }
            if let Err(error) = self.evaluate_node(node, symbols, strings) {
                return Some(error);
            }
        }
        self.stack.pop();
        None
    }

    pub fn evaluate_node(
        &mut self,
        node: &TypedAstNode,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &StringMap
    ) -> Result<Value, Error> {
        let node_source = node.source();
        match node.node_variant() {
            AstNodeVariant::Procedure { public: _, name: _, arguments: _, body: _ } => {
                panic!("procedure should not be in the tree by now");
            }
            AstNodeVariant::Function { arguments, body } => {
                Ok(Value::Closure(arguments.clone(), self.stack.clone(), body.clone()))
            }
            AstNodeVariant::Variable { public: _, mutable: _, name, value } => {
                if let Some(value) = value {
                    let value = self.evaluate_node(value, symbols, strings)?;
                    let stack_size = self.stack.len();
                    self.stack[stack_size - 1].insert(*name, value);
                }
                Ok(Value::Unit)
            }
            AstNodeVariant::CaseBranches { value, branches, else_body } => {
                let value = self.evaluate_node(&*value, symbols, strings)?;
                let mut ran_branch = false;
                for branch in branches {
                    let branch_value = self.evaluate_node(&branch.0, symbols, strings)?;
                    if value != branch_value { continue; }
                    if let Some(error) = self.evaluate_nodes(&branch.1, symbols, strings) {
                        return Err(error);
                    };
                    ran_branch = true;
                    break;
                }
                if !ran_branch {
                    if let Some(error) = self.evaluate_nodes(else_body, symbols, strings) {
                        return Err(error);
                    };
                }
                Ok(Value::Unit)
            }
            AstNodeVariant::CaseConditon { condition, body, else_body } => {
                let condition = self.evaluate_node(&*condition, symbols, strings)?;
                if let Some(error) = self.evaluate_nodes(
                    if condition == Value::Boolean(true) { body } else { else_body },
                    symbols,
                    strings
                ) {
                    return Err(error);
                };
                Ok(Value::Unit)
            }
            AstNodeVariant::CaseVariant { value, branches, else_body } => {
                let value = self.evaluate_node(&*value, symbols, strings)?;
                let (variant_name, variant_value) = if let Value::Variant(variant_name, variant_value) = value {
                    (variant_name, variant_value)
                } else {
                    panic!("value should be a variant");
                };
                let mut ran_branch = false;
                for branch in branches {
                    if variant_name != branch.0 { continue; }
                    self.stack.push([(branch.1, *variant_value)].into());
                    if let Some(error) = self.evaluate_nodes(&branch.2, symbols, strings) {
                        return Err(error);
                    };
                    self.stack.pop();
                    ran_branch = true;
                    break;
                }
                if let Some(else_body) = else_body {
                    if !ran_branch {
                        if let Some(error) = self.evaluate_nodes(else_body, symbols, strings) {
                            return Err(error);
                        };
                    }
                }
                Ok(Value::Unit)
            }
            AstNodeVariant::Assignment { variable, value } => {
                let value = self.evaluate_node(&*value, symbols, strings)?;
                if let Some(error) = self.evaluate_node_assignment(&*variable, symbols, strings, value) {
                    return Err(error);
                }
                Ok(Value::Unit)
            }
            AstNodeVariant::Return { value } => {
                let value = self.evaluate_node(&*value, symbols, strings)?;
                self.returned_value = Some(value);
                Ok(Value::Unit)
            }
            AstNodeVariant::Call { called, arguments } => {
                if let AstNodeVariant::ModuleAccess { path } = called.node_variant() {
                    if let Symbol::Procedure { parameter_names, scope: _, parameter_types: _, returns: _, body }
                        = symbols.get(path).expect("symbol should exist") {
                        let mut parameter_values = HashMap::new();
                        for param_idx in 0..parameter_names.len() {
                            parameter_values.insert(
                                parameter_names[param_idx],
                                self.evaluate_node(&arguments[param_idx], symbols, strings)?
                            );
                        }
                        self.stack.push(parameter_values);
                        if let Some(error) = self.evaluate_nodes(body, symbols, strings) {
                            return Err(error);
                        }
                        self.stack.pop();
                        let return_value = self.get_return_value();
                        return Ok(return_value);
                    }
                }
                let called = self.evaluate_node(&*called, symbols, strings)?;
                let (parameter_names, mut captured_stack, body) = if let Value::Closure(
                    a, b, c
                ) = called { (a, b, c) } else { panic!("value should be a closure") };
                let mut parameter_values = HashMap::new();
                for param_idx in 0..parameter_names.len() {
                    parameter_values.insert(
                        parameter_names[param_idx],
                        self.evaluate_node(&arguments[param_idx], symbols, strings)?
                    );
                }
                let captured_stack_size = captured_stack.len();
                self.stack.append(&mut captured_stack);
                self.stack.push(parameter_values);
                if let Some(error) = self.evaluate_nodes(&body, symbols, strings) {
                    return Err(error);
                }
                self.stack.pop();
                for _ in 0..captured_stack_size { self.stack.pop(); }
                let return_value = self.get_return_value();
                return Ok(return_value);
            }
            AstNodeVariant::Object { values } => {
                let mut member_values = HashMap::new();
                for (member_name, member_value) in values {
                    let member_value = self.evaluate_node(member_value, symbols, strings)?;
                    member_values.insert(*member_name, member_value);
                }
                Ok(Value::Object(RefCell::new(member_values).into()))
            }
            AstNodeVariant::Array { values } => {
                let mut elements = Vec::new();
                for value in values {
                    elements.push(self.evaluate_node(value, symbols, strings)?);
                }
                Ok(Value::Array(RefCell::new(elements.into()).into()))
            }
            AstNodeVariant::ObjectAccess { object, member } => {
                let accessed = self.evaluate_node(&*object, symbols, strings)?;
                let member_values = if let Value::Object(member_values) = accessed {
                    member_values
                } else { panic!("accessed value should be an object"); };
                let member_value = member_values.borrow().get(member).expect("object should have member").clone();
                Ok(member_value)
            }
            AstNodeVariant::ArrayAccess { array, index } => {
                let accessed = self.evaluate_node(&*array, symbols, strings)?;
                let element_values = if let Value::Array(element_values) = accessed {
                    element_values
                } else { panic!("accessed value should be an array"); };
                let index = self.evaluate_node(&*index, symbols, strings)?;
                let element_index = if let Value::Integer(element_index) = index {
                    element_index
                } else { panic!("accessed index should be an integer"); };
                let element_count = element_values.borrow().len();
                if element_index < 0 || element_index as usize >= element_count {
                    return Err(Error::new([
                        ErrorSection::Error(ErrorType::ArrayIndexOutOfBounds(element_count, element_index)),
                        ErrorSection::Code(node_source)
                    ].into()))
                }
                let element_value = element_values.borrow()[element_index as usize].clone();
                Ok(element_value)
            }
            AstNodeVariant::VariableAccess { name } => {
                let mut value_opt = None;
                for frame in (0..self.stack.len()).rev() {
                    if let Some(value) = self.stack[frame].get(name) {
                        value_opt = Some(value);
                        break;
                    }
                }
                Ok(value_opt.expect("variable should exist").clone())
            }
            AstNodeVariant::BooleanLiteral { value } => {
                Ok(Value::Boolean(*value))
            }
            AstNodeVariant::IntegerLiteral { value } => {
                Ok(Value::Integer(*value))
            }
            AstNodeVariant::FloatLiteral { value } => {
                Ok(Value::Float(*value))
            }
            AstNodeVariant::StringLiteral { value } => {
                Ok(Value::String(strings.get(*value).into()))
            }
            AstNodeVariant::UnitLiteral => {
                Ok(Value::Unit)
            }
            AstNodeVariant::Add { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, strings)?,
                    self.evaluate_node(&*b, symbols, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
                    (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::Subtract { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, strings)?,
                    self.evaluate_node(&*b, symbols, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a - b),
                    (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::Multiply { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, strings)?,
                    self.evaluate_node(&*b, symbols, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a * b),
                    (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::Divide { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, strings)?,
                    self.evaluate_node(&*b, symbols, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a / b),
                    (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::Modulo { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, strings)?,
                    self.evaluate_node(&*b, symbols, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a % b),
                    (Value::Float(a), Value::Float(b)) => Value::Float(a % b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::Negate { x } => {
                Ok(match self.evaluate_node(&*x, symbols, strings)? {
                    Value::Integer(x) => Value::Integer(-x),
                    Value::Float(x) => Value::Float(-x),
                    _ => panic!("value should be a number")
                })
            }
            AstNodeVariant::LessThan { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, strings)?,
                    self.evaluate_node(&*b, symbols, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a < b),
                    (Value::Float(a), Value::Float(b)) => Value::Boolean(a < b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::GreaterThan { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, strings)?,
                    self.evaluate_node(&*b, symbols, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a > b),
                    (Value::Float(a), Value::Float(b)) => Value::Boolean(a > b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::LessThanEqual { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, strings)?,
                    self.evaluate_node(&*b, symbols, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a <= b),
                    (Value::Float(a), Value::Float(b)) => Value::Boolean(a <= b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::GreaterThanEqual { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, strings)?,
                    self.evaluate_node(&*b, symbols, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a >= b),
                    (Value::Float(a), Value::Float(b)) => Value::Boolean(a >= b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::Equals { a, b } => {
                Ok(Value::Boolean(
                    self.evaluate_node(&*a, symbols, strings)? == self.evaluate_node(&*b, symbols, strings)?
                ))
            }
            AstNodeVariant::NotEquals { a, b } => {
                Ok(Value::Boolean(
                    self.evaluate_node(&*a, symbols, strings)? != self.evaluate_node(&*b, symbols, strings)?
                ))
            }
            AstNodeVariant::Not { x } => {
                Ok(match self.evaluate_node(&*x, symbols, strings)? {
                    Value::Boolean(x) => Value::Boolean(!x),
                    _ => panic!("value should be a boolean")
                })
            }
            AstNodeVariant::Or { a, b } => {
                let a = if let Value::Boolean(a) = self.evaluate_node(&*a, symbols, strings)? { a }
                    else { panic!("values should be booleans"); };
                Ok(if a {
                    Value::Boolean(true)
                } else {
                    let b = if let Value::Boolean(b) = self.evaluate_node(&*b, symbols, strings)? { b }
                        else { panic!("values should be booleans"); };
                    Value::Boolean(b)
                })
            }
            AstNodeVariant::And { a, b } => {
                let a = if let Value::Boolean(a) = self.evaluate_node(&*a, symbols, strings)? { a }
                    else { panic!("values should be booleans"); };
                Ok(if !a {
                    Value::Boolean(false)
                } else {
                    let b = if let Value::Boolean(b) = self.evaluate_node(&*b, symbols, strings)? { b }
                        else { panic!("values should be booleans"); };
                    Value::Boolean(b)
                })
            }
            AstNodeVariant::Module { path: _ } => {
                panic!("module declarations should not be in the tree by now");
            }
            AstNodeVariant::ModuleAccess { path } => {
                Ok(match symbols.get(path).expect("symbol should exist") {
                    Symbol::Constant { value } => {
                        if let Some(value) = self.constants.get(path) {
                            value.clone()
                        } else {
                            let value = self.evaluate_node(value, symbols, strings)?;
                            self.constants.insert(path.clone(), value.clone());
                            value
                        }
                    }
                    Symbol::Procedure { parameter_names: _, scope: _, parameter_types: _, returns: _, body: _ } => {
                        Value::Unit
                    }
                })
            }
            AstNodeVariant::Use { paths: _ } => {
                panic!("usage declarations should not be in the tree by now");
            }
            AstNodeVariant::Variant { name, value } => {
                let value = self.evaluate_node(&*value, symbols, strings)?;
                Ok(Value::Variant(*name, value.into()))
            }
        }
    }

    fn evaluate_node_assignment(
        &mut self,
        node: &TypedAstNode,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        strings: &StringMap,
        value: Value
    ) -> Option<Error> {
        let node_source = node.source();
        match node.node_variant() {
            AstNodeVariant::ObjectAccess { object, member } => {
                let accessed = match self.evaluate_node(&*object, symbols, strings) {
                    Ok(v) => v,
                    Err(error) => return Some(error)
                };
                if let Value::Object(member_values) = accessed {
                    member_values.borrow_mut().insert(*member, value);
                } else {
                    panic!("accessed value should be an object")
                }
            }
            AstNodeVariant::ArrayAccess { array, index } => {
                let accessed = match self.evaluate_node(&*array, symbols, strings) {
                    Ok(v) => v,
                    Err(error) => return Some(error)
                };
                let elements = if let Value::Array(elements) = accessed {
                    elements
                } else {
                    panic!("accessed value should be an object")
                };
                let index = match self.evaluate_node(&*index, symbols, strings) {
                    Ok(v) => v,
                    Err(error) => return Some(error)
                };
                let element_index = if let Value::Integer(element_index) = index {
                    element_index
                } else { panic!("accessed index should be an integer"); };
                let element_count = elements.borrow().len();
                if element_index < 0 || element_index as usize >= element_count {
                    return Some(Error::new([
                        ErrorSection::Error(ErrorType::ArrayIndexOutOfBounds(element_count, element_index)),
                        ErrorSection::Code(node_source)
                    ].into()))
                }
                elements.borrow_mut()[element_index as usize] = value;
            }
            AstNodeVariant::VariableAccess { name } => {
                for frame in (0..self.stack.len()).rev() {
                    if let Some(v) = self.stack[frame].get_mut(name) {
                        *v = value;
                        break;
                    }
                }
            }
            _ => panic!("node should be something that can be assigned to")
        }
        None
    }
}