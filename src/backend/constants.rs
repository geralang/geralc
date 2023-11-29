
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::util::strings::StringIdx;
use crate::backend::{
    ir::{IrType, IrTypeBank},
    interpreter::Value
};

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ConstantValue {
    Unit,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(usize),
    Array(usize),
    Object(usize),
    Variant(usize)
}

#[derive(Clone, Debug)]
pub struct ConstantPool {
    strings: Vec<Rc<str>>,
    arrays: Vec<(Rc<RefCell<Box<[Value]>>>, Box<[ConstantValue]>, IrType)>,
    objects: Vec<(Rc<RefCell<HashMap<StringIdx, Value>>>, HashMap<StringIdx, (ConstantValue, IrType)>)>,
    variants: Vec<(StringIdx, ConstantValue, IrType)>
}

impl ConstantPool {
    pub fn new() -> ConstantPool {
        ConstantPool {
            strings: Vec::new(),
            arrays: Vec::new(),
            objects: Vec::new(),
            variants: Vec::new()
        }
    }

    pub fn insert(&mut self, v: &Value, t: IrType, types: &IrTypeBank) -> ConstantValue {
        match v {
            Value::Unit => ConstantValue::Unit,
            Value::Boolean(b) => ConstantValue::Boolean(*b),
            Value::Integer(i) => ConstantValue::Integer(*i),
            Value::Float(f) => ConstantValue::Float(*f),
            Value::String(s) => {
                for si in 0..self.strings.len() {
                    if *self.strings[si] == **s {
                        return ConstantValue::String(si);
                    }
                }
                self.strings.push((**s).into());
                ConstantValue::String(self.strings.len() - 1)
            }
            Value::Array(a) => {
                for ai in 0..self.arrays.len() {
                    if Rc::ptr_eq(&self.arrays[ai].0, a) {
                        return ConstantValue::Array(ai);
                    }
                }
                let et = if let IrType::Array(i) = t.direct(types) { *types.get_array(i) }
                    else { panic!("type does not match value!"); };
                let ca = a.borrow()
                    .iter()
                    .map(|e| self.insert(e, et, types))
                    .collect();
                self.arrays.push((a.clone(), ca, et));
                ConstantValue::Array(self.arrays.len() - 1)
            }
            Value::Object(o) => {
                for oi in 0..self.objects.len() {
                    if Rc::ptr_eq(&self.objects[oi].0, o) {
                        return ConstantValue::Object(oi);
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
                self.objects.push((o.clone(), co));
                ConstantValue::Object(self.objects.len() - 1)
            }
            Value::Closure(_, _, _) => panic!("constant closures are not allowed!"),
            Value::Variant(tag, value) => {
                let vt = if let IrType::Variants(i) = t.direct(types) {
                    *types.get_variants(i).get(tag).expect("type should match value!")
                } else { panic!("type does not match value!"); };
                let cv = (*tag, self.insert(value, vt, types), vt);
                for vi in 0..self.variants.len() {
                    if self.variants[vi] == cv {
                        return ConstantValue::Variant(vi);
                    }
                }
                self.variants.push(cv);
                ConstantValue::Variant(self.variants.len() - 1)
            }
        }
    }

    pub fn get_string(&self, idx: usize) -> &str {
        &*self.strings[idx]
    }
    pub fn get_string_count(&self) -> usize { self.strings.len() }

    pub fn get_array(&self, idx: usize) -> (&[ConstantValue], IrType) {
        (&*self.arrays[idx].1, self.arrays[idx].2)
    }
    pub fn get_array_count(&self) -> usize { self.arrays.len() }

    pub fn get_object(&self, idx: usize) -> &HashMap<StringIdx, (ConstantValue, IrType)> {
        &self.objects[idx].1
    }
    pub fn get_object_count(&self) -> usize { self.objects.len() }

    pub fn get_variant(&self, idx: usize) -> (StringIdx, ConstantValue, IrType) {
        self.variants[idx]
    }
    pub fn get_variant_count(&self) -> usize { self.variants.len() }
}