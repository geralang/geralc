
use std::collections::{HashMap, HashSet};

use crate::util::{
    source::SourceRange,
    strings::StringIdx
};
use crate::frontend::modules::NamespacePath;
use crate::backend::interpreter::Value;


#[derive(Debug, Clone)]
pub enum IrSymbol {
    Procedure {
        path: NamespacePath,
        variant: usize,
        parameter_types: Vec<IrType>, return_type: IrType,
        variables: Vec<IrType>,
        body: Vec<IrInstruction>,
        source: SourceRange
    },
    ExternalProcedure {
        path: NamespacePath,
        backing: StringIdx,
        parameter_types: Vec<IrType>, return_type: IrType
    },
    BuiltInProcedure {
        path: NamespacePath,
        variant: usize,
        parameter_types: Vec<IrType>, return_type: IrType
    },
    Variable {
        path: NamespacePath,
        value_type: IrType,
        value: Value
    },
    ExternalVariable {
        path: NamespacePath,
        backing: StringIdx,
        value_type: IrType
    }
}


#[derive(Debug, Clone)]
pub struct IrTypeBank {
    arrays: Vec<IrType>,
    objects: Vec<HashMap<StringIdx, IrType>>,
    concrete_objects: Vec<Vec<(StringIdx, IrType)>>,
    variants: Vec<HashMap<StringIdx, IrType>>,
    closures: Vec<(Vec<IrType>, IrType)>,
    indirect: Vec<IrType>
}

impl IrTypeBank {
    pub fn new() -> IrTypeBank {
        IrTypeBank {
            arrays: Vec::new(),
            objects: Vec::new(),
            concrete_objects: Vec::new(),
            variants: Vec::new(),
            closures: Vec::new(),
            indirect: Vec::new()
        }
    }

    pub fn insert_array(&mut self, element_type: IrType) -> IrArrayTypeIdx {
        for i in 0..self.arrays.len() {
            if self.arrays[i].eq(&element_type, self, &mut HashMap::new()) { return IrArrayTypeIdx(i); }
        }
        self.arrays.push(element_type);
        return IrArrayTypeIdx(self.arrays.len() - 1);
    }
    pub fn get_array(&self, idx: IrArrayTypeIdx) -> &IrType { &self.arrays[idx.0] }
    pub fn get_all_arrays(&self) -> &Vec<IrType> { &self.arrays }

    pub fn insert_object(&mut self, member_types: HashMap<StringIdx, IrType>) -> IrObjectTypeIdx {
        for i in 0..self.objects.len() {
            if IrType::objects_eq(&self.objects[i], &member_types, self, &mut HashMap::new()) { return IrObjectTypeIdx(i); }
        }
        self.objects.push(member_types);
        return IrObjectTypeIdx(self.objects.len() - 1);
    }
    pub fn get_object(&self, idx: IrObjectTypeIdx) -> &HashMap<StringIdx, IrType> { &self.objects[idx.0] }
    pub fn get_all_objects(&self) -> &Vec<HashMap<StringIdx, IrType>> { &self.objects }

    pub fn insert_concrete_object(&mut self, member_types: Vec<(StringIdx, IrType)>) -> IrConcreteObjectTypeIdx {
        for i in 0..self.concrete_objects.len() {
            if IrType::concrete_objects_eq(&self.concrete_objects[i], &member_types, self, &mut HashMap::new()) { return IrConcreteObjectTypeIdx(i); }
        }
        self.concrete_objects.push(member_types);
        return IrConcreteObjectTypeIdx(self.concrete_objects.len() - 1);
    }
    pub fn get_concrete_object(&self, idx: IrConcreteObjectTypeIdx) -> &Vec<(StringIdx, IrType)> { &self.concrete_objects[idx.0] }
    pub fn get_all_concrete_objects(&self) -> &Vec<Vec<(StringIdx, IrType)>> { &self.concrete_objects }

    pub fn insert_variants(&mut self, variant_types: HashMap<StringIdx, IrType>) -> IrVariantTypeIdx {
        for i in 0..self.variants.len() {
            if IrType::variants_eq(&self.variants[i], &variant_types, self, &mut HashMap::new()) { return IrVariantTypeIdx(i); }
        }
        self.variants.push(variant_types);
        return IrVariantTypeIdx(self.variants.len() - 1);
    }
    pub fn get_variants(&self, idx: IrVariantTypeIdx) -> &HashMap<StringIdx, IrType> { &self.variants[idx.0] }
    pub fn get_all_variants(&self) -> &Vec<HashMap<StringIdx, IrType>> { &self.variants }

    pub fn insert_closure(&mut self, closure: (Vec<IrType>, IrType)) -> IrClosureTypeIdx {
        for i in 0..self.closures.len() {
            if IrType::closures_eq(&self.closures[i], &closure, self, &mut HashMap::new()) { return IrClosureTypeIdx(i); }
        }
        self.closures.push(closure);
        return IrClosureTypeIdx(self.closures.len() - 1);
    }
    pub fn get_closure(&self, idx: IrClosureTypeIdx) -> &(Vec<IrType>, IrType) { &self.closures[idx.0] }
    pub fn get_all_closures(&self) -> &Vec<(Vec<IrType>, IrType)> { &self.closures }

    pub fn insert_indirect(&mut self, inserted: IrType) -> IrIndirectTypeIdx {
        for i in 0..self.indirect.len() {
            if self.indirect[i].eq(&inserted, self, &mut HashMap::new()) { return IrIndirectTypeIdx(i); }
        }
        self.indirect.push(inserted);
        return IrIndirectTypeIdx(self.indirect.len() - 1);
    }
    pub fn get_indirect(&self, idx: IrIndirectTypeIdx) -> &IrType { &self.indirect[idx.0] }
    pub fn overwrite_indirect(&mut self, idx: IrIndirectTypeIdx, new_type: IrType) {
        self.indirect[idx.0] = new_type;
    }
    pub fn get_all_indirects(&self) -> &Vec<IrType> { &self.indirect }

    pub fn collapse(&mut self) {
        // ugly buggy solution
        return; // find an alternative later
        let mut merged_object_indices = HashSet::new();
        for object_idx_a in 0..self.objects.len() {
            if merged_object_indices.contains(&object_idx_a) { continue; }
            for object_idx_b in 0..self.objects.len() {
                if merged_object_indices.contains(&object_idx_b) { continue; }
                if IrType::objects_eq(
                    &self.objects[object_idx_a], 
                    &self.objects[object_idx_b],
                    self, &mut HashMap::new()
                ) {
                    self.for_every_type(|t| if let IrType::Object(object_idx) = t {
                        *object_idx = IrObjectTypeIdx(object_idx_a);
                    });
                    merged_object_indices.insert(object_idx_b);
                }
            }
            merged_object_indices.insert(object_idx_a);
        }
        let mut merged_variant_indices = HashSet::new();
        for variant_idx_a in 0..self.variants.len() {
            if merged_variant_indices.contains(&variant_idx_a) { continue; }
            for variant_idx_b in 0..self.variants.len() {
                if merged_variant_indices.contains(&variant_idx_b) { continue; }
                if IrType::variants_eq(
                    &self.variants[variant_idx_a], 
                    &self.variants[variant_idx_b],
                    self, &mut HashMap::new()
                ) {
                    self.for_every_type(|t| if let IrType::Variants(variant_idx) = t {
                        *variant_idx = IrVariantTypeIdx(variant_idx_a);
                    });
                    merged_variant_indices.insert(variant_idx_b);
                }
            }
            merged_variant_indices.insert(variant_idx_a);
        }
    }

    fn for_every_type<F: Fn(&mut IrType)>(&mut self, action: F) {
        for member_type in &mut self.arrays { (action)(member_type); }
        for object in &mut self.objects {
            for (_, member_type) in object { (action)(member_type); }
        }
        for concrete_object in &mut self.concrete_objects {
            for (_, member_type) in concrete_object { (action)(member_type); }
        }
        for variants in &mut self.variants {
            for (_, variant_type) in variants { (action)(variant_type); }
        }
        for closure in &mut self.closures {
            for param_type in &mut closure.0 { (action)(param_type); }
            (action)(&mut closure.1);
        }
        for indirect_type in &mut self.indirect { (action)(indirect_type); }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct IrArrayTypeIdx(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct IrObjectTypeIdx(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct IrConcreteObjectTypeIdx(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct IrVariantTypeIdx(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct IrClosureTypeIdx(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct IrIndirectTypeIdx(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrType {
    Unit,
    Boolean,
    Integer,
    Float,
    String,
    Array(IrArrayTypeIdx),
    Object(IrObjectTypeIdx),
    ConcreteObject(IrConcreteObjectTypeIdx),
    Variants(IrVariantTypeIdx),
    Closure(IrClosureTypeIdx),
    Indirect(IrIndirectTypeIdx)
}

impl IrType {
    pub fn direct(&self, type_bank: &IrTypeBank) -> IrType {
        let mut v = *self;
        while let IrType::Indirect(idx) = v {
            v = *type_bank.get_indirect(idx); 
        }
        v
    }

    pub fn objects_eq(a: &HashMap<StringIdx, IrType>, b: &HashMap<StringIdx, IrType>, type_bank: &IrTypeBank, encountered: &mut HashMap<usize, IrType>) -> bool {
        for member in a.keys() {
            if !b.contains_key(member) { return false; }
            if !a.get(member).unwrap().eq(b.get(member).unwrap(), type_bank, encountered) { return false; }
        }
        for member in b.keys() {
            if !a.contains_key(member) { return false; }
            if !a.get(member).unwrap().eq(b.get(member).unwrap(), type_bank, encountered) { return false; }
        }
        return true;
    }

    pub fn concrete_objects_eq(a: &Vec<(StringIdx, IrType)>, b: &Vec<(StringIdx, IrType)>, type_bank: &IrTypeBank, encountered: &mut HashMap<usize, IrType>) -> bool {
        if a.len() != b.len() { return false; }
        for member in 0..a.len() {
            if a[member].0 != b[member].0 { return false; }
            if !a[member].1.eq(&b[member].1, type_bank, encountered) { return false; }
        }
        return true;
    }

    pub fn variants_eq(a: &HashMap<StringIdx, IrType>, b: &HashMap<StringIdx, IrType>, type_bank: &IrTypeBank, encountered: &mut HashMap<usize, IrType>) -> bool {
        for variant in a.keys() {
            if !b.contains_key(variant) { return false; }
            if !a.get(variant).unwrap().eq(b.get(variant).unwrap(), type_bank, encountered) { return false; }
        }
        for variant in b.keys() {
            if !a.contains_key(variant) { return false; }
            if !a.get(variant).unwrap().eq(b.get(variant).unwrap(), type_bank, encountered) { return false; }
        }
        return true;
    }

    pub fn closures_eq(a: &(Vec<IrType>, IrType), b: &(Vec<IrType>, IrType), type_bank: &IrTypeBank, encountered: &mut HashMap<usize, IrType>) -> bool {
        if a.0.len() != b.0.len() { return false; }
        for arg in 0..a.0.len() {
            if !a.0[arg].eq(&b.0[arg], type_bank, encountered) { return false; }
        }
        if !a.1.eq(&b.1, type_bank, encountered) { return false; }
        return true;
    }

    pub fn eq(&self, other: &IrType, type_bank: &IrTypeBank, encountered: &mut HashMap<usize, IrType>) -> bool {
        match (self, other) {
            // (IrType::Indirect(_), IrType::Indirect(_)) => {
            //     true
            // }
            (IrType::Indirect(i), o) => {
                if let Some(last_type) = encountered.get(&i.0) {
                    *o == *last_type
                } else {
                    encountered.insert(i.0, *o);
                    let result = type_bank.get_indirect(*i).eq(o, type_bank, encountered);
                    encountered.remove(&i.0);
                    result
                }
            }
            (o, IrType::Indirect(i)) => {
                if let Some(last_type) = encountered.get(&i.0) {
                    *o == *last_type
                } else {
                    encountered.insert(i.0, *o);
                    let result = o.eq(type_bank.get_indirect(*i), type_bank, encountered);
                    encountered.remove(&i.0);
                    result
                }
            }
            (IrType::Array(a), IrType::Array(b)) => {
                if a.0 == b.0 { return true; }
                type_bank.get_array(*a).eq(type_bank.get_array(*b), type_bank, encountered)
            }
            (IrType::Object(a), IrType::Object(b)) => {
                if a.0 == b.0 { return true; }
                IrType::objects_eq(
                    type_bank.get_object(*a), type_bank.get_object(*b), type_bank, encountered
                )
            }
            (IrType::ConcreteObject(a), IrType::ConcreteObject(b)) => {
                if a.0 == b.0 { return true; }
                IrType::concrete_objects_eq(
                    type_bank.get_concrete_object(*a), type_bank.get_concrete_object(*b), type_bank, encountered
                )
            }
            (IrType::Variants(a), IrType::Variants(b)) => {
                if a.0 == b.0 { return true; }
                IrType::variants_eq(
                    type_bank.get_variants(*a), type_bank.get_variants(*b), type_bank, encountered
                )
            }
            (IrType::Closure(a), IrType::Closure(b)) => {
                if a.0 == b.0 { return true; }
                IrType::closures_eq(
                    type_bank.get_closure(*a), type_bank.get_closure(*b), type_bank, encountered
                )
            }
            (a, b) => std::mem::discriminant(a) == std::mem::discriminant(b)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IrVariable {
    pub index: usize,
    pub version: usize
} 

#[derive(Debug, Clone)]
pub enum IrInstruction {

    LoadUnit { into: IrVariable },
    LoadBoolean { value: bool, into: IrVariable },
    LoadInteger { value: i64, into: IrVariable },
    LoadFloat { value: f64, into: IrVariable },
    LoadString { value: StringIdx, into: IrVariable },
    LoadObject { member_values: HashMap<StringIdx, IrVariable>, into: IrVariable },
    LoadArray { element_values: Vec<IrVariable>, into: IrVariable },
    LoadVariant { name: StringIdx, v: IrVariable, into: IrVariable },
    LoadGlobalVariable { path: NamespacePath, into: IrVariable },
    LoadParameter { index: usize, into: IrVariable },
    LoadClosure {
        parameter_types: Vec<IrType>, return_type: IrType, captured: HashMap<StringIdx, IrVariable>,
        variables: Vec<IrType>, body: Vec<IrInstruction>, into: IrVariable, source: SourceRange
    },

    GetObjectMember { accessed: IrVariable, member: StringIdx, into: IrVariable },
    SetObjectMember { value: IrVariable, accessed: IrVariable, member: StringIdx },

    GetArrayElement { accessed: IrVariable, index: IrVariable, into: IrVariable },
    SetArrayElement { value: IrVariable, accessed: IrVariable, index: IrVariable },

    GetClosureCapture { name: StringIdx, into: IrVariable },
    SetClosureCapture { value: IrVariable, name: StringIdx },

    Move { from: IrVariable, into: IrVariable },

    Add { a: IrVariable, b: IrVariable, into: IrVariable },
    Subtract { a: IrVariable, b: IrVariable, into: IrVariable },
    Multiply { a: IrVariable, b: IrVariable, into: IrVariable },
    Divide { a: IrVariable, b: IrVariable, into: IrVariable },
    Modulo { a: IrVariable, b: IrVariable, into: IrVariable },
    Negate { x: IrVariable, into: IrVariable },

    LessThan { a: IrVariable, b: IrVariable, into: IrVariable },
    LessThanEquals { a: IrVariable, b: IrVariable, into: IrVariable },
    GreaterThan { a: IrVariable, b: IrVariable, into: IrVariable },
    GreaterThanEquals { a: IrVariable, b: IrVariable, into: IrVariable },
    Equals { a: IrVariable, b: IrVariable, into: IrVariable },
    NotEquals { a: IrVariable, b: IrVariable, into: IrVariable },
    Not { x: IrVariable, into: IrVariable },

    BranchOnValue { value: IrVariable, branches: Vec<(Value, Vec<IrInstruction>)>, else_branch: Vec<IrInstruction> },
    BranchOnVariant { value: IrVariable, branches: Vec<(StringIdx, Option<IrVariable>, Vec<IrInstruction>)>, else_branch: Vec<IrInstruction> },

    Call { path: NamespacePath, variant: usize, arguments: Vec<IrVariable>, into: IrVariable },
    CallClosure { called: IrVariable, arguments: Vec<IrVariable>, into: IrVariable },
    Return { value: IrVariable },

    Phi { options: Vec<IrVariable>, into: IrVariable }

}