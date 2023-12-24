
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::util::strings::StringIdx;
use crate::backend::{
    ir::{IrType, IrTypeBank},
    interpreter::Value
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
    Variant(ConstantVariantIdx)
}

#[derive(Clone, Debug)]
enum InternalConstantPoolValue {
    String(Rc<str>),
    Array(Rc<RefCell<Box<[Value]>>>, Box<[ConstantValue]>, IrType),
    Object(Rc<RefCell<HashMap<StringIdx, Value>>>, HashMap<StringIdx, (ConstantValue, IrType)>),
    Variant(StringIdx, ConstantValue, IrType)
}

#[derive(Clone, Debug)]
pub enum ConstantPoolValue<'p> {
    String(&'p str),
    Array(&'p [ConstantValue], IrType),
    Object(&'p HashMap<StringIdx, (ConstantValue, IrType)>),
    Variant(StringIdx, ConstantValue, IrType)
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

    pub fn insert(&mut self, v: &Value, t: IrType, types: &IrTypeBank) -> ConstantValue {
        match v {
            Value::Unit => ConstantValue::Unit,
            Value::Boolean(b) => ConstantValue::Boolean(*b),
            Value::Integer(i) => ConstantValue::Integer(*i),
            Value::Float(f) => ConstantValue::Float(*f),
            Value::String(s) => {
                for vi in 0..self.values.len() {
                    if let InternalConstantPoolValue::String(content) = &self.values[vi] {
                        if **content == **s {
                            return ConstantValue::String(ConstantStringIdx(vi));
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
                            return ConstantValue::Array(ConstantArrayIdx(vi));
                        }
                    }
                }
                let et = if let IrType::Array(i) = t.direct(types) { *types.get_array(i) }
                    else { panic!("type does not match value!"); };
                let ca = a.borrow()
                    .iter()
                    .map(|e| self.insert(e, et, types))
                    .collect();
                self.values.push(InternalConstantPoolValue::Array(a.clone(), ca, et));
                ConstantValue::Array(ConstantArrayIdx(self.values.len() - 1))
            }
            Value::Object(o) => {
                for vi in 0..self.values.len() {
                    if let InternalConstantPoolValue::Object(og_ref, _) = &self.values[vi] {
                        if Rc::ptr_eq(og_ref, o) {
                            return ConstantValue::Object(ConstantObjectIdx(vi));
                        }
                    }
                }
                let mt = if let IrType::Object(i) = t.direct(types) { types.get_object(i) }
                    else { panic!("type does not match value!"); };
                let co = o.borrow()
                    .iter()
                    .map(|(mn, mv)| {
                        let mt = *mt.get(mn).expect("type should match value!");
                        (*mn, (self.insert(mv, mt, types), mt))
                    })
                    .collect();
                self.values.push(InternalConstantPoolValue::Object(o.clone(), co));
                ConstantValue::Object(ConstantObjectIdx(self.values.len() - 1))
            }
            Value::Closure(_, _, _) => panic!("constant closures are not allowed!"),
            Value::Variant(tag, value) => {
                let vt = if let IrType::Variants(i) = t.direct(types) {
                    *types.get_variants(i).get(tag).expect("type should match value!")
                } else { panic!("type does not match value!"); };
                let converted_value = self.insert(value, vt, types);
                for vi in 0..self.values.len() {
                    if let InternalConstantPoolValue::Variant(cmp_tag, cmp_value, _)
                            = self.values[vi] {
                        if cmp_tag == *tag && cmp_value == converted_value {
                            return ConstantValue::Variant(ConstantVariantIdx(vi));
                        }
                    }
                }
                self.values.push(InternalConstantPoolValue::Variant(*tag, converted_value, vt));
                ConstantValue::Variant(ConstantVariantIdx(self.values.len() - 1))
            }
        }
    }

    pub fn get_string(&self, idx: ConstantStringIdx) -> &str {
        if let InternalConstantPoolValue::String(content) = &self.values[idx.0] { &*content }
        else { panic!("passed illegal index"); }
    }

    pub fn get_array(&self, idx: ConstantArrayIdx) -> (&[ConstantValue], IrType) {
        if let InternalConstantPoolValue::Array(_, values, element_type) = &self.values[idx.0] {
            (&*values, *element_type)
        }
        else { panic!("passed illegal index"); }
    }

    pub fn get_object(&self, idx: ConstantObjectIdx) -> &HashMap<StringIdx, (ConstantValue, IrType)> {
        if let InternalConstantPoolValue::Object(_, members) = &self.values[idx.0] { members }
        else { panic!("passed illegal index"); }
    }

    pub fn get_variant(&self, idx: ConstantVariantIdx) -> (StringIdx, ConstantValue, IrType) {
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
            InternalConstantPoolValue::Variant(tag, value, value_type) => {
                ConstantPoolValue::Variant(*tag, *value, *value_type)
            }
        }
    }

    pub fn get_value_count(&self) -> usize { self.values.len() }
}