use std::{
    rc::Rc,
    cell::RefCell,
    collections::{HashMap, hash_map::DefaultHasher},
    hash::{Hasher, Hash}
};

use crate::util::{
    strings::{StringIdx, StringMap},
    error::{Error, ErrorSection, ErrorType},
    source::{HasSource, SourceRange}
};
use crate::frontend::{
    ast::{TypedAstNode, HasAstNodeVariant, AstNodeVariant},
    modules::NamespacePath,
    type_checking::Symbol
};


#[derive(Clone)]
pub enum Value {
    Unit,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(Rc<str>),
    Array(Rc<RefCell<Box<[Value]>>>),
    Object(Rc<RefCell<HashMap<StringIdx, Value>>>),
    Closure(Vec<(StringIdx, SourceRange)>, StackFrame, Vec<TypedAstNode>),
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
            ) => *a.borrow() == *b.borrow(),
            (
                Value::Object(a),
                Value::Object(b)
            ) => *a.borrow() == *b.borrow(),
            (
                Value::Closure(_, a, _),
                Value::Closure(_, b, _)
            ) => Rc::ptr_eq(a, b),
            (
                Value::Variant(a0, a1),
                Value::Variant(b0, b1)
            ) => *a0 == *b0 && *a1 == *b1,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "<unit>"),
            Value::Boolean(arg0) => write!(f, "{}", arg0),
            Value::Integer(arg0) => write!(f, "{}", arg0),
            Value::Float(arg0) => write!(f, "{}", arg0),
            Value::String(arg0) => write!(f, "{}", arg0),
            Value::Array(arg0) => write!(f, "{:?}", arg0),
            Value::Object(arg0) => write!(f, "{:?}", arg0),
            Value::Closure(_, captured, _) => write!(f, "<closure ({:?})>", captured),
            Value::Variant(arg0, arg1) => write!(f, "#@{} {:?}", arg0.0, arg1),
        }
    }
}

type BuiltinProcedures = HashMap<
    NamespacePath,
    fn(&mut Interpreter, SourceRange, &[Value], &HashMap<NamespacePath, Symbol<TypedAstNode>>, &HashMap<NamespacePath, StringIdx>, &mut StringMap) -> Result<Value, Error>
>;

type StackFrame = Rc<RefCell<HashMap<StringIdx, Value>>>;

#[derive(Debug, Clone)]
pub struct Interpreter {
    constants: HashMap<NamespacePath, Value>,
    stack: Vec<StackFrame>,
    returned_value: Option<Value>,
    stack_trace: Vec<(String, StringIdx, usize)>,
    builtins: BuiltinProcedures
}

impl Interpreter {
    pub fn new(strings: &mut StringMap) -> Interpreter {
        fn path_from(segments: &[&'static str], strings: &mut StringMap) -> NamespacePath {
            NamespacePath::new(segments.iter().map(|s| strings.insert(s)).collect())
        }
        let mut builtins: BuiltinProcedures = HashMap::new();
        builtins.insert(path_from(&["core", "addr_eq"], strings), |_, _, params, _, _, _| {
            Ok(match (&params[0], &params[1]) {
                (Value::Object(a), Value::Object(b)) => Value::Boolean(Rc::ptr_eq(a, b)),
                (Value::Array(a), Value::Array(b)) => Value::Boolean(Rc::ptr_eq(a, b)),
                (Value::String(a), Value::String(b)) => Value::Boolean(Rc::ptr_eq(a, b)),
                _ => panic!("should be objects, arrays or strings")
            })
        });
        builtins.insert(path_from(&["core", "tag_eq"], strings), |_, _, params, _, _, _| {
            Ok(match (&params[0], &params[1]) {
                (Value::Variant(tag_a, _), Value::Variant(tag_b, _)) => Value::Boolean(*tag_a == *tag_b),
                _ => panic!("should be variants")
            })
        });
        builtins.insert(path_from(&["core", "length"], strings), |_, _, params, _, _, _| {
            Ok(match &params[0] {
                Value::Array(a) => Value::Integer(a.borrow().len() as i64),
                Value::String(a) => Value::Integer(a.chars().count() as i64),
                _ => panic!("should be array or string")
            })
        });
        builtins.insert(path_from(&["core", "array"], strings), |interpreter, source, params, _, _, strings| {
            let count = match params[1] {
                Value::Integer(c) => c,
                _ => panic!("should be an integer")
            };
            if count < 0 {
                return Err(interpreter.generate_panic(&format!("the array length {} is not valid", count), source, strings))
            }
            let mut values = Vec::new();
            for _ in 0..(count as usize) {
                values.push(params[0].clone());
            }
            Ok(Value::Array(RefCell::new(values.into()).into()))
        });
        builtins.insert(path_from(&["core", "exhaust"], strings), |interpreter, _, params, symbols, external_backings, strings| {
            let (captured_frame, body) = if let Value::Closure(
                _, captured_frame, body
            ) = params[0].clone() { (captured_frame, body) } else { panic!("value should be a closure") };
            interpreter.stack_trace.push(("<closure>".into(), strings.insert("???"), 0));
            let end_tag = strings.insert("end");
            interpreter.stack.push(captured_frame);
            interpreter.stack.push(RefCell::new(HashMap::new()).into());
            loop {
                if let Some(error) = interpreter.evaluate_nodes(&body, symbols, external_backings, strings) {
                    return Err(error);
                }
                let return_value = interpreter.get_return_value();
                if let Value::Variant(tag, _) = return_value {
                    if tag == end_tag { break; }
                } else { panic!("should return variant"); }
            }
            interpreter.stack.pop();
            interpreter.stack.pop();
            Ok(Value::Unit)
        });
        builtins.insert(path_from(&["core", "panic"], strings), |interpreter, source, params, _, _, strings| {
            if let Value::String(reason) = &params[0] {
                Err(interpreter.generate_panic(&*reason, source, strings))
            } else { panic!("should be a string"); }
        });
        builtins.insert(path_from(&["core", "as_str"], strings), |_, _, params, _, _, strings| {
            Ok(Value::String(match &params[0] {
                Value::Unit => "<unit>".into(),
                Value::Boolean(b) => b.to_string().into(),
                Value::Integer(i) => i.to_string().into(),
                Value::Float(f) => f.to_string().into(),
                Value::String(s) => s.clone(),
                Value::Array(_) => "<array>".into(),
                Value::Object(_) => "<object>".into(),
                Value::Closure(_, _, _) => "<closure>".into(),
                Value::Variant(tag, _) => format!("#{} <...>", strings.get(*tag)).into(),
            }))
        });
        builtins.insert(path_from(&["core", "as_int"], strings), |_, _, params, _, _, _| {
            Ok(Value::Integer(match &params[0] {
                Value::Integer(i) => *i,
                Value::Float(f) => *f as i64,
                _ => panic!("should be a number")
            }))
        });
        builtins.insert(path_from(&["core", "as_flt"], strings), |_, _, params, _, _, _| {
            Ok(Value::Float(match &params[0] {
                Value::Integer(i) => *i as f64,
                Value::Float(f) => *f,
                _ => panic!("should be a number")
            }))
        });
        builtins.insert(path_from(&["core", "substring"], strings), |interpreter, source, params, _, _, strings| {
            let src = if let Value::String(source) = &params[0] { source }
                else { panic!("should be a string"); };
            let source_length = src.chars().count();
            let start = if let Value::Integer(start) = &params[1] { *start }
                else { panic!("should be an integer"); };
            let end = if let Value::Integer(end) = &params[2] { *end }
            else { panic!("should be an integer"); };
            let start_index = if start < 0 { source_length as i64 + start } else { start } as usize;
            let end_index = if end < 0 { source_length as i64 + end } else { end } as usize;
            if start_index > source_length { 
                return Err(interpreter.generate_panic(
                    &format!("the start index {} is out of bounds for a string of length {}", start, source_length),
                    source, strings
                ));
            }
            if end_index > source_length {
                return Err(interpreter.generate_panic(
                    &format!("the start index {} is out of bounds for a string of length {}", end, source_length),
                    source, strings
                ));
            }
            if start_index > end_index {
                return Err(interpreter.generate_panic(
                    &format!("the start index {} is larger than the end index {} (length of string is {})", start, end, source_length),
                    source, strings
                ));
            }
            Ok(Value::String(
                src.chars()
                    .enumerate()
                    .filter(|(ci, _)| *ci >= start_index && *ci < end_index)
                    .map(|(_, c)| c)
                    .collect::<String>()
                    .into()
            ))
        });
        builtins.insert(path_from(&["core", "concat"], strings), |_, _, params, _, _, _| {
            let a = if let Value::String(a) = &params[0] { a }
                else { panic!("should be a string"); };
            let b = if let Value::String(b) = &params[1] { b }
                else { panic!("should be a string"); };
            Ok(Value::String(
                a.chars()
                    .chain(b.chars())
                    .collect::<String>()
                    .into()
            ))
        });
        builtins.insert(path_from(&["core", "parse_flt"], strings), |_, _, params, _, _, strings| {
            let src = if let Value::String(src) = &params[0] { src }
                else { panic!("should be a string"); };
            return Ok(if let Ok(v) = src.parse() {
                Value::Variant(strings.insert("some"), Value::Float(v).into())
            } else {
                Value::Variant(strings.insert("none"), Value::Unit.into())
            });
        });
        builtins.insert(path_from(&["core", "parse_int"], strings), |_, _, params, _, _, strings| {
            let src = if let Value::String(src) = &params[0] { src }
                else { panic!("should be a string"); };
            return Ok(if let Ok(v) = src.parse() {
                Value::Variant(strings.insert("some"), Value::Integer(v).into())
            } else {
                Value::Variant(strings.insert("none"), Value::Unit.into())
            });
        });
        builtins.insert(path_from(&["core", "string"], strings), |interpreter, source, params, _, _, strings| {
            let repeated = if let Value::String(repeated) = &params[0] { repeated }
            else { panic!("should be a string"); };
            let count = if let Value::Integer(count) = &params[1] { *count }
                else { panic!("should be an integer"); };
            if count < 0 {
                return Err(interpreter.generate_panic(
                    &format!("the string repetition count {} is not valid", count),
                    source, strings
                ));
            }
            Ok(Value::String(
                repeated.repeat(count as usize).into()
            ))
        });
        builtins.insert(path_from(&["core", "hash"], strings), |_, _, params, _, _, _| {
            use std::mem::transmute;
            fn compute_hash(value: &Value) -> i64 {
                match value {
                    Value::Unit => 0,
                    Value::Boolean(b) => if *b { 1 } else { 0 },
                    Value::Integer(i) => {
                        let mut hasher = DefaultHasher::new();
                        i.hash(&mut hasher);
                        unsafe {
                            transmute::<_, i64>(hasher.finish())
                        }
                    }
                    Value::Float(f) => {
                        let mut hasher = DefaultHasher::new();
                        unsafe {
                            transmute::<_, i64>(*f).hash(&mut hasher);
                            transmute::<_, i64>(hasher.finish())
                        }
                    }
                    Value::String(s) => {
                        let mut hasher = DefaultHasher::new();
                        (**s).hash(&mut hasher);
                        unsafe {
                            transmute::<_, i64>(hasher.finish())
                        }
                    }
                    Value::Array(a) => {
                        let mut hasher = DefaultHasher::new();
                        unsafe {
                            transmute::<_, usize>(a.as_ptr()).hash(&mut hasher);
                            transmute::<_, i64>(hasher.finish())
                        }
                    }
                    Value::Object(o) => {
                        let mut hasher = DefaultHasher::new();
                        unsafe {
                            transmute::<_, usize>(o.as_ptr()).hash(&mut hasher);
                            transmute::<_, i64>(hasher.finish())
                        }
                    }
                    Value::Closure(_, c, _) => {
                        let mut hasher = DefaultHasher::new();
                        unsafe {
                            transmute::<_, usize>(c.as_ptr()).hash(&mut hasher);
                            transmute::<_, i64>(hasher.finish())
                        }
                    }
                    Value::Variant(tag, value) => {
                        let mut hasher = DefaultHasher::new();
                        tag.0.hash(&mut hasher);
                        compute_hash(&*value).hash(&mut hasher);
                        unsafe {
                            transmute::<_, i64>(hasher.finish())
                        }
                    }
                }
            }
            Ok(Value::Integer(compute_hash(&params[0])))
        });
        Interpreter {
            constants: HashMap::new(),
            stack: vec![RefCell::new(HashMap::new()).into()],
            returned_value: None,
            stack_trace: Vec::new(),
            builtins
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
        external_backings: &HashMap<NamespacePath, StringIdx>,
        strings: &mut StringMap
    ) -> Option<Error> {
        for node in nodes {
            if self.returned_value.is_some() { break; }
            if let Err(error) = self.evaluate_node(node, symbols, external_backings, strings) {
                return Some(error);
            }
        }
        None
    }

    pub fn stack_trace_push(&mut self, name: String, from: SourceRange, strings: &StringMap) {
        let source_line = strings.get(from.file_content())[..from.start_position()]
            .lines().collect::<Vec<&str>>().len();
        self.stack_trace.push((name, from.file_name(), source_line));
    }

    pub fn generate_panic(&mut self, reason: &str, source: SourceRange, strings: &StringMap) -> Error {
        static ERROR_NOTE_COLOR: &str = "\x1b[0;90m";
        static ERROR_MESSAGE_COLOR: &str = "\x1b[0;91m";
        static ERROR_INDEX_COLOR: &str = "\x1b[0;90m";
        static ERROR_PROCEDURE_COLOR: &str = "\x1b[0;32;1m";
        static ERROR_FILE_NAME_COLOR: &str = "\x1b[0;37m";
        static ERROR_RESET_COLOR: &str = "\x1b[0m";
        let mut err = String::new();
        err.push_str(ERROR_MESSAGE_COLOR);
        err.push_str(reason);
        err.push_str("\n");
        err.push_str(ERROR_NOTE_COLOR);
        err.push_str("Stack trace (latest call first):");
        let mut i = self.stack_trace.len() - 1;
        loop {
            let si = &self.stack_trace[i];
            err.push_str("\n");
            err.push_str(ERROR_INDEX_COLOR);
            err.push_str(" ");
            err.push_str(&i.to_string());
            err.push_str(" ");
            err.push_str(ERROR_PROCEDURE_COLOR);
            err.push_str(&si.0);
            err.push_str(ERROR_NOTE_COLOR);
            err.push_str(" at ");
            err.push_str(ERROR_FILE_NAME_COLOR);
            err.push_str(strings.get(si.1));
            err.push_str(":");
            err.push_str(&si.2.to_string());
            if i == 0 { break; }
            i -= 1;
        }
        err.push_str(ERROR_RESET_COLOR);
        return Error::new([
            ErrorSection::Error(ErrorType::ConstExpressionPanics),
            ErrorSection::Raw(err),
            ErrorSection::Code(source)
        ].into());
    }

    pub fn evaluate_node(
        &mut self,
        node: &TypedAstNode,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        external_backings: &HashMap<NamespacePath, StringIdx>,
        strings: &mut StringMap
    ) -> Result<Value, Error> {
        let node_source = node.source();
        match node.node_variant() {
            AstNodeVariant::Procedure { public: _, name: _, arguments: _, body: _ } => {
                panic!("procedure should not be in the tree by now");
            }
            AstNodeVariant::Function { arguments, body } => {
                let captured = self.stack[self.stack.len() - 1].borrow().clone();
                Ok(Value::Closure(
                    arguments.clone(), RefCell::new(captured).into(), body.clone()
                ))
            }
            AstNodeVariant::Variable { public: _, mutable: _, name, value_types: _, value } => {
                if let Some(value) = value {
                    let value = self.evaluate_node(value, symbols, external_backings, strings)?;
                    let stack_size = self.stack.len();
                    self.stack[stack_size - 1].borrow_mut().insert(*name, value);
                }
                Ok(Value::Unit)
            }
            AstNodeVariant::CaseBranches { value, branches, else_body } => {
                let value = self.evaluate_node(&*value, symbols, external_backings, strings)?;
                let mut ran_branch = false;
                for branch in branches {
                    let branch_value = self.evaluate_node(&branch.0, symbols, external_backings, strings)?;
                    if value != branch_value { continue; }
                    if let Some(error) = self.evaluate_nodes(&branch.1, symbols, external_backings, strings) {
                        return Err(error);
                    };
                    ran_branch = true;
                    break;
                }
                if !ran_branch {
                    if let Some(error) = self.evaluate_nodes(else_body, symbols, external_backings, strings) {
                        return Err(error);
                    };
                }
                Ok(Value::Unit)
            }
            AstNodeVariant::CaseConditon { condition, body, else_body } => {
                let condition = self.evaluate_node(&*condition, symbols, external_backings, strings)?;
                if let Some(error) = self.evaluate_nodes(
                    if condition == Value::Boolean(true) { body } else { else_body },
                    symbols,
                    external_backings, strings
                ) {
                    return Err(error);
                };
                Ok(Value::Unit)
            }
            AstNodeVariant::CaseVariant { value, branches, else_body } => {
                let value = self.evaluate_node(&*value, symbols, external_backings, strings)?;
                let (variant_name, variant_value) = if let Value::Variant(variant_name, variant_value) = value {
                    (variant_name, variant_value)
                } else {
                    panic!("value should be a variant");
                };
                let mut ran_branch = false;
                for branch in branches {
                    if variant_name != branch.0 { continue; }
                    if let Some(variant_var) = &branch.1 {
                        self.stack.push(RefCell::new([(variant_var.0, *variant_value)].into()).into());
                    } else {
                        self.stack.push(RefCell::new(HashMap::new()).into());
                    }
                    if let Some(error) = self.evaluate_nodes(&branch.2, symbols, external_backings, strings) {
                        return Err(error);
                    };
                    self.stack.pop();
                    ran_branch = true;
                    break;
                }
                if let Some(else_body) = else_body {
                    if !ran_branch {
                        if let Some(error) = self.evaluate_nodes(else_body, symbols, external_backings, strings) {
                            return Err(error);
                        };
                    }
                }
                Ok(Value::Unit)
            }
            AstNodeVariant::Assignment { variable, value } => {
                let value = self.evaluate_node(&*value, symbols, external_backings, strings)?;
                if let Some(error) = self.evaluate_node_assignment(&*variable, symbols, external_backings, strings, value) {
                    return Err(error);
                }
                Ok(Value::Unit)
            }
            AstNodeVariant::Return { value } => {
                let value = self.evaluate_node(&*value, symbols, external_backings, strings)?;
                self.returned_value = Some(value);
                Ok(Value::Unit)
            }
            AstNodeVariant::Call { called, arguments } => {
                if let AstNodeVariant::ModuleAccess { path } = called.node_variant() {
                    if let Symbol::Procedure { public: _, parameter_names, parameter_types: _, returns: _, body, source: _ }
                        = symbols.get(path).expect("symbol should exist") {
                        self.stack_trace_push(path.display(strings), node.source(), strings);
                        let returned = if let Some(body) = body {
                            let mut parameter_values = HashMap::new();
                            for param_idx in 0..parameter_names.len() {
                                parameter_values.insert(
                                    parameter_names[param_idx],
                                    self.evaluate_node(&arguments[param_idx], symbols, external_backings, strings)?
                                );
                            }
                            self.stack.push(RefCell::new(parameter_values).into());
                            if let Some(error) = self.evaluate_nodes(body, symbols, external_backings, strings) {
                                return Err(error);
                            }
                            self.stack.pop();
                            let return_value = self.get_return_value();
                            Ok(return_value)
                        } else if let Some(_) = external_backings.get(path) {
                            return Err(Error::new([
                                ErrorSection::Error(ErrorType::ConstDependsOnExternal(path.display(strings))),
                                ErrorSection::Code(node_source)
                            ].into()));
                        } else {
                            let mut parameter_values = Vec::new();
                            for param_idx in 0..parameter_names.len() {
                                parameter_values.push(
                                    self.evaluate_node(&arguments[param_idx], symbols, external_backings, strings)?
                                );
                            }
                            if let Some(implementation) = self.builtins.get(path) {
                                (implementation)(self, node.source(), &parameter_values, symbols, external_backings, strings)
                            } else {
                                panic!("The builtin {} has no implementation!", path.display(strings));
                            }
                        };
                        self.stack_trace.pop();
                        return returned;
                    }
                }
                let called = self.evaluate_node(&*called, symbols, external_backings, strings)?;
                let (parameter_names, captures, body) = if let Value::Closure(
                    a, b, c
                ) = called { (a, b, c) } else { panic!("value should be a closure") };
                let mut parameter_values = HashMap::new();
                for param_idx in 0..parameter_names.len() {
                    parameter_values.insert(
                        parameter_names[param_idx].0,
                        self.evaluate_node(&arguments[param_idx], symbols, external_backings, strings)?
                    );
                }
                self.stack.push(captures);
                self.stack.push(RefCell::new(parameter_values).into());
                self.stack_trace_push("<closure>".into(), node.source(), strings);
                if let Some(error) = self.evaluate_nodes(&body, symbols, external_backings, strings) {
                    return Err(error);
                }
                self.stack.pop();
                self.stack.pop();
                self.stack_trace.pop();
                let return_value = self.get_return_value();
                return Ok(return_value);
            }
            AstNodeVariant::Object { values } => {
                let mut member_values = HashMap::new();
                for (member_name, member_value) in values {
                    let member_value = self.evaluate_node(member_value, symbols, external_backings, strings)?;
                    member_values.insert(*member_name, member_value);
                }
                Ok(Value::Object(RefCell::new(member_values).into()))
            }
            AstNodeVariant::Array { values } => {
                let mut elements = Vec::new();
                for value in values {
                    elements.push(self.evaluate_node(value, symbols, external_backings, strings)?);
                }
                Ok(Value::Array(RefCell::new(elements.into()).into()))
            }
            AstNodeVariant::ObjectAccess { object, member } => {
                let accessed = self.evaluate_node(&*object, symbols, external_backings, strings)?;
                let member_values = if let Value::Object(member_values) = accessed {
                    member_values
                } else { panic!("accessed value should be an object"); };
                let member_value = member_values.borrow().get(member).expect("object should have member").clone();
                Ok(member_value)
            }
            AstNodeVariant::ArrayAccess { array, index } => {
                let accessed = self.evaluate_node(&*array, symbols, external_backings, strings)?;
                let element_values = if let Value::Array(element_values) = accessed {
                    element_values
                } else { panic!("accessed value should be an array"); };
                let index = self.evaluate_node(&*index, symbols, external_backings, strings)?;
                let element_index = if let Value::Integer(element_index) = index {
                    element_index
                } else { panic!("accessed index should be an integer"); };
                let element_count = element_values.borrow().len();
                let final_element_index = if element_index < 0 { (element_count as i64 + element_index) as usize }
                    else { element_index as usize };
                if final_element_index >= element_count {
                    self.stack_trace_push("<index>".into(), node.source(), strings);
                    return Err(self.generate_panic(
                        &format!("array index {} is out of bounds for an array of length {}", element_index, element_count),
                        node.source(), strings
                    ))
                }
                let element_value = element_values.borrow()[final_element_index].clone();
                Ok(element_value)
            }
            AstNodeVariant::VariableAccess { name } => {
                let mut value_opt = None;
                for frame in (0..self.stack.len()).rev() {
                    if let Some(value) = self.stack[frame].borrow().get(name) {
                        value_opt = Some(value.clone());
                        break;
                    }
                }
                Ok(value_opt.expect("variable should exist"))
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
                    self.evaluate_node(&*a, symbols, external_backings, strings)?,
                    self.evaluate_node(&*b, symbols, external_backings, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
                    (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::Subtract { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, external_backings, strings)?,
                    self.evaluate_node(&*b, symbols, external_backings, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a - b),
                    (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::Multiply { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, external_backings, strings)?,
                    self.evaluate_node(&*b, symbols, external_backings, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a * b),
                    (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::Divide { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, external_backings, strings)?,
                    self.evaluate_node(&*b, symbols, external_backings, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => {
                        if b == 0 {
                            self.stack_trace_push("<division>".into(), node.source(), strings);
                            return Err(self.generate_panic("integer division by zero", node.source(), strings))
                        }
                        Value::Integer(a / b)
                    }
                    (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::Modulo { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, external_backings, strings)?,
                    self.evaluate_node(&*b, symbols, external_backings, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a % b),
                    (Value::Float(a), Value::Float(b)) => Value::Float(a % b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::Negate { x } => {
                Ok(match self.evaluate_node(&*x, symbols, external_backings, strings)? {
                    Value::Integer(x) => Value::Integer(-x),
                    Value::Float(x) => Value::Float(-x),
                    _ => panic!("value should be a number")
                })
            }
            AstNodeVariant::LessThan { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, external_backings, strings)?,
                    self.evaluate_node(&*b, symbols, external_backings, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a < b),
                    (Value::Float(a), Value::Float(b)) => Value::Boolean(a < b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::GreaterThan { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, external_backings, strings)?,
                    self.evaluate_node(&*b, symbols, external_backings, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a > b),
                    (Value::Float(a), Value::Float(b)) => Value::Boolean(a > b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::LessThanEqual { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, external_backings, strings)?,
                    self.evaluate_node(&*b, symbols, external_backings, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a <= b),
                    (Value::Float(a), Value::Float(b)) => Value::Boolean(a <= b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::GreaterThanEqual { a, b } => {
                Ok(match (
                    self.evaluate_node(&*a, symbols, external_backings, strings)?,
                    self.evaluate_node(&*b, symbols, external_backings, strings)?
                ) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a >= b),
                    (Value::Float(a), Value::Float(b)) => Value::Boolean(a >= b),
                    _ => panic!("values should be numbers of the same type")
                })
            }
            AstNodeVariant::Equals { a, b } => {
                Ok(Value::Boolean(
                    self.evaluate_node(&*a, symbols, external_backings, strings)?
                        == self.evaluate_node(&*b, symbols, external_backings, strings)?
                ))
            }
            AstNodeVariant::NotEquals { a, b } => {
                Ok(Value::Boolean(
                    self.evaluate_node(&*a, symbols, external_backings, strings)?
                        != self.evaluate_node(&*b, symbols, external_backings, strings)?
                ))
            }
            AstNodeVariant::Not { x } => {
                Ok(match self.evaluate_node(&*x, symbols, external_backings, strings)? {
                    Value::Boolean(x) => Value::Boolean(!x),
                    _ => panic!("value should be a boolean")
                })
            }
            AstNodeVariant::Or { a, b } => {
                let a = if let Value::Boolean(a) = self.evaluate_node(&*a, symbols, external_backings, strings)? { a }
                    else { panic!("values should be booleans"); };
                Ok(if a {
                    Value::Boolean(true)
                } else {
                    let b = if let Value::Boolean(b) = self.evaluate_node(&*b, symbols, external_backings, strings)? { b }
                        else { panic!("values should be booleans"); };
                    Value::Boolean(b)
                })
            }
            AstNodeVariant::And { a, b } => {
                let a = if let Value::Boolean(a) = self.evaluate_node(&*a, symbols, external_backings, strings)? { a }
                    else { panic!("values should be booleans"); };
                Ok(if !a {
                    Value::Boolean(false)
                } else {
                    let b = if let Value::Boolean(b) = self.evaluate_node(&*b, symbols, external_backings, strings)? { b }
                        else { panic!("values should be booleans"); };
                    Value::Boolean(b)
                })
            }
            AstNodeVariant::Module { path: _ } => {
                panic!("module declarations should not be in the tree by now");
            }
            AstNodeVariant::ModuleAccess { path } => {
                Ok(match symbols.get(path).expect("symbol should exist") {
                    Symbol::Constant { public: _, value, value_types: _ } => {
                        if let Some(value) = self.constants.get(path) {
                            value.clone()
                        } else {
                            let value = if let Some(value) = value {
                                value
                            } else {
                                return Err(Error::new([
                                    ErrorSection::Error(ErrorType::ConstDependsOnExternal(path.display(strings))),
                                    ErrorSection::Code(node_source)
                                ].into()));
                            };
                            let value = self.evaluate_node(value, symbols, external_backings, strings)?;
                            self.constants.insert(path.clone(), value.clone());
                            value
                        }
                    }
                    Symbol::Procedure { .. } => {
                        Value::Unit
                    }
                })
            }
            AstNodeVariant::Use { paths: _ } => {
                panic!("usage declarations should not be in the tree by now");
            }
            AstNodeVariant::Variant { name, value } => {
                let value = self.evaluate_node(&*value, symbols, external_backings, strings)?;
                Ok(Value::Variant(*name, value.into()))
            }
            AstNodeVariant::Static { value } => {
                self.evaluate_node(&*value, symbols, external_backings, strings)
            }
            AstNodeVariant::Target { target: _, body: _ } => {
                panic!("Should've been expanded!");
            }
        }
    }

    fn evaluate_node_assignment(
        &mut self,
        node: &TypedAstNode,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>,
        external_backings: &HashMap<NamespacePath, StringIdx>,
        strings: &mut StringMap,
        value: Value
    ) -> Option<Error> {
        match node.node_variant() {
            AstNodeVariant::ObjectAccess { object, member } => {
                let accessed = match self.evaluate_node(&*object, symbols, external_backings, strings) {
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
                let accessed = match self.evaluate_node(&*array, symbols, external_backings, strings) {
                    Ok(v) => v,
                    Err(error) => return Some(error)
                };
                let elements = if let Value::Array(elements) = accessed {
                    elements
                } else {
                    panic!("accessed value should be an object")
                };
                let index = match self.evaluate_node(&*index, symbols, external_backings, strings) {
                    Ok(v) => v,
                    Err(error) => return Some(error)
                };
                let element_index = if let Value::Integer(element_index) = index {
                    element_index
                } else { panic!("accessed index should be an integer"); };
                let element_count = elements.borrow().len();
                let final_element_index = if element_index < 0 { (element_count as i64 + element_index) as usize }
                    else { element_index as usize };
                if final_element_index >= element_count {
                    self.stack_trace_push("<index>".into(), node.source(), strings);
                    return Some(self.generate_panic(
                        &format!("array index {} is out of bounds for an array of length {}", element_index, element_count),
                        node.source(), strings
                    ))
                }
                elements.borrow_mut()[final_element_index] = value;
            }
            AstNodeVariant::VariableAccess { name } => {
                for frame in (0..self.stack.len()).rev() {
                    if let Some(v) = self.stack[frame].borrow_mut().get_mut(name) {
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