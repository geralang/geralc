
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::frontend::{
    ast::TypedAstNode,
    modules::NamespacePath,
    type_checking::Symbol,
    types::{TypeMap, Type, TypeGroup}
};
use crate::util::{
    error::Error,
    strings::{StringIdx, StringMap}
};
use crate::backend::{
    interpreter::Value,
    ir::IrInstruction,
    lowering::{IrGenerator, TypeMapping},
    interpreter::Interpreter,
    ir::IrSymbol
};

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ConstantStringIdx(usize);

impl Into<usize> for ConstantStringIdx { fn into(self) -> usize { self.0 } }
impl Into<usize> for &ConstantStringIdx { fn into(self) -> usize { self.0 } }

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ConstantArrayIdx(usize);

impl Into<usize> for ConstantArrayIdx { fn into(self) -> usize { self.0 } }
impl Into<usize> for &ConstantArrayIdx { fn into(self) -> usize { self.0 } }

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ConstantObjectIdx(usize);

impl Into<usize> for ConstantObjectIdx { fn into(self) -> usize { self.0 } }
impl Into<usize> for &ConstantObjectIdx { fn into(self) -> usize { self.0 } }

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ConstantClosureIdx(usize);

impl Into<usize> for ConstantClosureIdx { fn into(self) -> usize { self.0 } }
impl Into<usize> for &ConstantClosureIdx { fn into(self) -> usize { self.0 } }

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ConstantVariantIdx(usize);

impl Into<usize> for ConstantVariantIdx { fn into(self) -> usize { self.0 } }
impl Into<usize> for &ConstantVariantIdx { fn into(self) -> usize { self.0 } }

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ConstantValue {
    Unit,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(ConstantStringIdx),
    Array(ConstantArrayIdx),
    Object(ConstantObjectIdx),
    Closure(ConstantClosureIdx),
    Variant(ConstantVariantIdx)
}

#[derive(Clone, Debug)]
pub struct ConstantClosure {
    pub parameter_types: Vec<TypeGroup>,
    pub return_type: TypeGroup,
    pub captured: HashMap<StringIdx, ConstantValue>,
    pub variables: Vec<TypeGroup>, 
    pub body: Vec<IrInstruction>
}

#[derive(Clone, Debug)]
enum InternalConstantPoolValue {
    String(Rc<str>),
    Array(Rc<RefCell<Box<[Value]>>>, Box<[ConstantValue]>, TypeGroup),
    Object(Rc<RefCell<HashMap<StringIdx, Value>>>, HashMap<StringIdx, (ConstantValue, TypeGroup)>),
    Closure(super::interpreter::StackFrame, ConstantClosure),
    Variant(StringIdx, ConstantValue, TypeGroup)
}

#[derive(Clone, Debug)]
pub enum ConstantPoolValue<'p> {
    String(&'p str),
    Array(&'p [ConstantValue], TypeGroup),
    Object(&'p HashMap<StringIdx, (ConstantValue, TypeGroup)>),
    Closure(&'p ConstantClosure),
    Variant(StringIdx, ConstantValue, TypeGroup)
}

#[derive(Clone, Debug)]
pub struct ConstantPool {
    values: Vec<InternalConstantPoolValue>
}

impl ConstantPool {
    pub fn new() -> ConstantPool {
        ConstantPool {
            values: Vec::new()
        }
    }

    pub fn insert(
        &mut self, v: &Value, t: TypeGroup, types: &mut TypeMap, type_mapping: &mut TypeMapping,
        symbols: &HashMap<NamespacePath, Symbol<TypedAstNode>>, strings: &mut StringMap,
        external_backings: &HashMap<NamespacePath, StringIdx>, interpreter: &mut Interpreter,
        ir_symbols: &mut Vec<IrSymbol>
    ) -> Result<ConstantValue, Error> {
        Ok(match v {
            Value::Unit => ConstantValue::Unit,
            Value::Boolean(b) => ConstantValue::Boolean(*b),
            Value::Integer(i) => ConstantValue::Integer(*i),
            Value::Float(f) => ConstantValue::Float(*f),
            Value::String(s) => {
                for vi in 0..self.values.len() {
                    if let InternalConstantPoolValue::String(content) = &self.values[vi] {
                        if **content == **s {
                            return Ok(ConstantValue::String(ConstantStringIdx(vi)));
                        }
                    }
                }
                self.values.push(InternalConstantPoolValue::String(s.clone()));
                ConstantValue::String(ConstantStringIdx(self.values.len() - 1))
            }
            Value::Array(a) => {
                for vi in 0..self.values.len() {
                    if let InternalConstantPoolValue::Array(og_ref, _, _) = &self.values[vi] {
                        if Rc::ptr_eq(og_ref, a) { 
                            return Ok(ConstantValue::Array(ConstantArrayIdx(vi)));
                        }
                    }
                }
                let et = if let Type::Array(i) = types.group_concrete(t) { types.array(i) }
                    else { panic!("type does not match value!"); };
                let mut ca = Vec::new();
                for e in a.borrow().iter() {
                    ca.push(self.insert(
                        e, et, types, type_mapping, symbols, strings, external_backings,
                        interpreter, ir_symbols
                    )?);
                }
                self.values.push(InternalConstantPoolValue::Array(a.clone(), ca.into(), et));
                ConstantValue::Array(ConstantArrayIdx(self.values.len() - 1))
            }
            Value::Object(o) => {
                for vi in 0..self.values.len() {
                    if let InternalConstantPoolValue::Object(og_ref, _) = &self.values[vi] {
                        if Rc::ptr_eq(og_ref, o) {
                            return Ok(ConstantValue::Object(ConstantObjectIdx(vi)));
                        }
                    }
                }
                let mt = if let Type::Object(i) = types.group_concrete(t) { types.object(i).0.clone() }
                    else { panic!("type does not match value!"); };
                let mut co = HashMap::new();
                for (mn, mv) in o.borrow().iter() {
                    let mt = *mt.get(mn).expect("type should match value!");
                    co.insert(*mn, (self.insert(
                        mv, mt, types, type_mapping, symbols, strings, external_backings,
                        interpreter, ir_symbols
                    )?, mt));
                }
                self.values.push(InternalConstantPoolValue::Object(o.clone(), co));
                ConstantValue::Object(ConstantObjectIdx(self.values.len() - 1))
            }
            Value::Closure(parameter_names, captures, body) => {
                for vi in 0..self.values.len() {
                    if let InternalConstantPoolValue::Closure(og_ref, _) = &self.values[vi] {
                        if Rc::ptr_eq(og_ref, captures) {
                            return Ok(ConstantValue::Closure(ConstantClosureIdx(vi)));
                        }
                    }
                }
                let (parameter_types, return_type) =
                    if let Type::Closure(clo) = types.group_concrete(t) {
                    let t = types.closure(clo);
                    (t.0.clone(), t.1)
                } else { panic!("should be a closure!"); };
                let mut generator = IrGenerator::new();
                let mut parameters = (HashMap::new(), Vec::new());
                for param_idx in 0..parameter_types.len() {
                    parameters.0.insert(parameter_names[param_idx].0, param_idx);
                    parameters.1.push(parameter_types[param_idx]);
                }
                let body = generator.lower_nodes(
                    body, &captures.borrow().iter().map(|(cn, (_, ct))| (*cn, *ct)).collect(),
                    types, type_mapping, HashMap::new(), symbols, strings,
                    external_backings, &parameters, interpreter, ir_symbols,
                    self
                )?;
                let mut captured = HashMap::new();
                for (capture_name, capture_value) in captures.borrow().iter() {
                    captured.insert(
                        *capture_name,
                        self.insert(
                            &capture_value.0, capture_value.1,
                            types, type_mapping, symbols, strings, external_backings,
                            interpreter, ir_symbols
                        )?
                    );
                }
                self.values.push(InternalConstantPoolValue::Closure(captures.clone(), ConstantClosure {
                    parameter_types,
                    return_type,
                    captured,
                    variables: generator.variable_types(),
                    body
                })); 
                ConstantValue::Closure(ConstantClosureIdx(self.values.len() - 1))
            }
            Value::Variant(tag, value) => {
                let vt = if let Type::Variants(i) = types.group_concrete(t) {
                    *types.variants(i).0.get(tag).expect("type should match value!")
                } else { panic!("type does not match value!"); };
                let converted_value = self.insert(
                    value, vt, types, type_mapping, symbols, strings, external_backings,
                    interpreter, ir_symbols
                )?;
                for vi in 0..self.values.len() {
                    if let InternalConstantPoolValue::Variant(cmp_tag, cmp_value, _)
                            = self.values[vi] {
                        if cmp_tag == *tag && cmp_value == converted_value {
                            return Ok(ConstantValue::Variant(ConstantVariantIdx(vi)));
                        }
                    }
                }
                self.values.push(InternalConstantPoolValue::Variant(*tag, converted_value, vt));
                ConstantValue::Variant(ConstantVariantIdx(self.values.len() - 1))
            }
        })
    }

    pub fn get_string(&self, idx: ConstantStringIdx) -> &str {
        if let InternalConstantPoolValue::String(content) = &self.values[idx.0] { &*content }
        else { panic!("passed illegal index"); }
    }

    pub fn get_array(&self, idx: ConstantArrayIdx) -> (&[ConstantValue], TypeGroup) {
        if let InternalConstantPoolValue::Array(_, values, element_type) = &self.values[idx.0] {
            (&*values, *element_type)
        }
        else { panic!("passed illegal index"); }
    }

    pub fn get_object(&self, idx: ConstantObjectIdx) -> &HashMap<StringIdx, (ConstantValue, TypeGroup)> {
        if let InternalConstantPoolValue::Object(_, members) = &self.values[idx.0] { members }
        else { panic!("passed illegal index"); }
    }

    pub fn get_closure(&self, idx: ConstantClosureIdx) -> &ConstantClosure {
        if let InternalConstantPoolValue::Closure(_, closure) = &self.values[idx.0] { closure }
        else { panic!("passed illegal index"); }
    }

    pub fn get_variant(&self, idx: ConstantVariantIdx) -> (StringIdx, ConstantValue, TypeGroup) {
        if let InternalConstantPoolValue::Variant(tag, value, value_type) = &self.values[idx.0] {
            (*tag, *value, *value_type)
        }
        else { panic!("passed illegal index"); }
    }

    pub fn get_value<'p>(&'p self, idx: usize) -> ConstantPoolValue<'p> {
        match &self.values[idx] {
            InternalConstantPoolValue::String(content) => {
                ConstantPoolValue::String(&*content)
            }
            InternalConstantPoolValue::Array(_, values, element_type) => {
                ConstantPoolValue::Array(&*values, *element_type)
            }
            InternalConstantPoolValue::Object(_, members) => {
                ConstantPoolValue::Object(members)
            }
            InternalConstantPoolValue::Closure(_, closure) => {
                ConstantPoolValue::Closure(closure)
            }
            InternalConstantPoolValue::Variant(tag, value, value_type) => {
                ConstantPoolValue::Variant(*tag, *value, *value_type)
            }
        }
    }

    pub fn get_value_count(&self) -> usize { self.values.len() }
}