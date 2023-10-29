
use std::collections::HashMap;

use crate::util::strings::StringIdx;
use crate::frontend::modules::NamespacePath;
use crate::backend::interpreter::Value;


#[derive(Debug, Clone)]
pub enum IrSymbol {
    Procedure {
        path: NamespacePath,
        variant: usize,
        parameter_types: Vec<IrType>, return_type: IrType,
        variables: Vec<IrType>,
        body: Vec<IrInstruction>
    },
    ExternalProcedure {
        path: NamespacePath,
        backing: StringIdx,
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
    procedure_ptrs: Vec<(Vec<IrType>, IrType)>,
    indirect: Vec<IrType>
}

impl IrTypeBank {
    pub fn new() -> IrTypeBank {
        IrTypeBank {
            arrays: Vec::new(),
            objects: Vec::new(),
            concrete_objects: Vec::new(),
            variants: Vec::new(),
            procedure_ptrs: Vec::new(),
            indirect: Vec::new()
        }
    }

    pub fn insert_array(&mut self, element_type: IrType) -> IrArrayTypeIdx {
        for i in 0..self.arrays.len() {
            if self.arrays[i].eq(&element_type, self) { return IrArrayTypeIdx(i); }
        }
        self.arrays.push(element_type);
        return IrArrayTypeIdx(self.arrays.len() - 1);
    }
    pub fn get_array(&self, idx: IrArrayTypeIdx) -> &IrType { &self.arrays[idx.0] }

    pub fn insert_object(&mut self, member_types: HashMap<StringIdx, IrType>) -> IrObjectTypeIdx {
        for i in 0..self.objects.len() {
            if IrType::objects_eq(&self.objects[i], &member_types, self) { return IrObjectTypeIdx(i); }
        }
        self.objects.push(member_types);
        return IrObjectTypeIdx(self.objects.len() - 1);
    }
    pub fn get_object(&self, idx: IrObjectTypeIdx) -> &HashMap<StringIdx, IrType> { &self.objects[idx.0] }

    pub fn insert_concrete_object(&mut self, member_types: Vec<(StringIdx, IrType)>) -> IrConcreteObjectTypeIdx {
        for i in 0..self.concrete_objects.len() {
            if IrType::concrete_objects_eq(&self.concrete_objects[i], &member_types, self) { return IrConcreteObjectTypeIdx(i); }
        }
        self.concrete_objects.push(member_types);
        return IrConcreteObjectTypeIdx(self.concrete_objects.len() - 1);
    }
    pub fn get_concrete_object(&self, idx: IrConcreteObjectTypeIdx) -> &Vec<(StringIdx, IrType)> { &self.concrete_objects[idx.0] }

    pub fn insert_variants(&mut self, variant_types: HashMap<StringIdx, IrType>) -> IrVariantTypeIdx {
        for i in 0..self.variants.len() {
            if IrType::variants_eq(&self.variants[i], &variant_types, self) { return IrVariantTypeIdx(i); }
        }
        self.variants.push(variant_types);
        return IrVariantTypeIdx(self.variants.len() - 1);
    }
    pub fn get_variants(&self, idx: IrVariantTypeIdx) -> &HashMap<StringIdx, IrType> { &self.variants[idx.0] }

    pub fn insert_procedure_ptr(&mut self, signature: (Vec<IrType>, IrType)) -> IrProcedurePtrTypeIdx {
        for i in 0..self.procedure_ptrs.len() {
            if IrType::procedure_ptr_eq(&self.procedure_ptrs[i], &signature, self) { return IrProcedurePtrTypeIdx(i); }
        }
        self.procedure_ptrs.push(signature);
        return IrProcedurePtrTypeIdx(self.procedure_ptrs.len() - 1);
    }
    pub fn get_procedure_ptr(&self, idx: IrProcedurePtrTypeIdx) -> &(Vec<IrType>, IrType) { &self.procedure_ptrs[idx.0] }

    pub fn insert_indirect(&mut self, inserted: IrType) -> IrIndirectTypeIdx {
        for i in 0..self.indirect.len() {
            if self.indirect[i].eq(&inserted, self) { return IrIndirectTypeIdx(i); }
        }
        self.indirect.push(inserted);
        return IrIndirectTypeIdx(self.indirect.len() - 1);
    }
    pub fn get_indirect(&self, idx: IrIndirectTypeIdx) -> &IrType { &self.indirect[idx.0] }
    pub fn overwrite_indirect(&mut self, idx: IrIndirectTypeIdx, new_type: IrType) {
        self.indirect[idx.0] = new_type;
    }
}

#[derive(Debug, Copy, Clone)]
pub struct IrArrayTypeIdx(usize);

#[derive(Debug, Copy, Clone)]
pub struct IrObjectTypeIdx(usize);

#[derive(Debug, Copy, Clone)]
pub struct IrConcreteObjectTypeIdx(usize);

#[derive(Debug, Copy, Clone)]
pub struct IrVariantTypeIdx(usize);

#[derive(Debug, Copy, Clone)]
pub struct IrProcedurePtrTypeIdx(usize);

#[derive(Debug, Copy, Clone)]
pub struct IrIndirectTypeIdx(usize);

#[derive(Debug, Clone, Copy)]
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
    ProcedurePtr(IrProcedurePtrTypeIdx),
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

    pub fn objects_eq(a: &HashMap<StringIdx, IrType>, b: &HashMap<StringIdx, IrType>, type_bank: &IrTypeBank) -> bool {
        for member in a.keys() {
            if !b.contains_key(member) { return false; }
            if !a.get(member).unwrap().eq(b.get(member).unwrap(), type_bank) { return false; }
        }
        for member in b.keys() {
            if !a.contains_key(member) { return false; }
            if !a.get(member).unwrap().eq(b.get(member).unwrap(), type_bank) { return false; }
        }
        return true;
    }

    pub fn concrete_objects_eq(a: &Vec<(StringIdx, IrType)>, b: &Vec<(StringIdx, IrType)>, type_bank: &IrTypeBank) -> bool {
        if a.len() != b.len() { return false; }
        for member in 0..a.len() {
            if a[member].0 != b[member].0 { return false; }
            if !a[member].1.eq(&b[member].1, type_bank) { return false; }
        }
        return true;
    }

    pub fn variants_eq(a: &HashMap<StringIdx, IrType>, b: &HashMap<StringIdx, IrType>, type_bank: &IrTypeBank) -> bool {
        for variant in a.keys() {
            if !b.contains_key(variant) { return false; }
            if !a.get(variant).unwrap().eq(b.get(variant).unwrap(), type_bank) { return false; }
        }
        for variant in b.keys() {
            if !a.contains_key(variant) { return false; }
            if !a.get(variant).unwrap().eq(b.get(variant).unwrap(), type_bank) { return false; }
        }
        return true;
    }

    pub fn procedure_ptr_eq(a: &(Vec<IrType>, IrType), b: &(Vec<IrType>, IrType), type_bank: &IrTypeBank) -> bool {
        if a.0.len() != b.0.len() { return false; }
        for arg in 0..a.0.len() {
            if !a.0[arg].eq(&b.0[arg], type_bank) { return false; }
        }
        if !a.1.eq(&b.1, type_bank) { return false; }
        return true;
    }

    pub fn eq(&self, other: &IrType, type_bank: &IrTypeBank) -> bool {
        match (self, other) {
            (IrType::Indirect(_), IrType::Indirect(_)) => true,
            (IrType::Indirect(i), o) => o.eq(type_bank.get_indirect(*i), type_bank),
            (IrType::Array(a), IrType::Array(b)) => {
                type_bank.get_array(*a).eq(type_bank.get_array(*b), type_bank)
            }
            (IrType::Object(a), IrType::Object(b)) => {
                IrType::objects_eq(
                    type_bank.get_object(*a), type_bank.get_object(*b), type_bank
                )
            }
            (IrType::ConcreteObject(a), IrType::ConcreteObject(b)) => {
                IrType::concrete_objects_eq(
                    type_bank.get_concrete_object(*a), type_bank.get_concrete_object(*b), type_bank
                )
            }
            (IrType::Variants(a), IrType::Variants(b)) => {
                IrType::variants_eq(
                    type_bank.get_variants(*a), type_bank.get_variants(*b), type_bank
                )
            }
            (IrType::ProcedurePtr(a), IrType::ProcedurePtr(b)) => {
                IrType::procedure_ptr_eq(
                    type_bank.get_procedure_ptr(*a), type_bank.get_procedure_ptr(*b), type_bank
                )
            }
            (a, b) => std::mem::discriminant(a) == std::mem::discriminant(b)
        }
    }
}

#[derive(Debug, Clone, Copy)]
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
    LoadProcedurePtr { path: NamespacePath, into: IrVariable },
    LoadParameter { name: StringIdx, into: IrVariable },

    GetObjectMember { accessed: IrVariable, member: StringIdx, into: IrVariable },
    SetObjectMember { value: IrVariable, accessed: IrVariable, member: StringIdx },

    GetArrayElement { accessed: IrVariable, index: IrVariable, into: IrVariable },
    SetArrayElement { value: IrVariable, accessed: IrVariable, index: IrVariable },

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
    BranchOnVariant { value: IrVariable, branches: Vec<(StringIdx, IrVariable, Vec<IrInstruction>)>, else_branch: Vec<IrInstruction> },

    Call { path: NamespacePath, arguments: Vec<IrVariable>, into: IrVariable },
    CallPtr { called: IrVariable, arguments: Vec<IrVariable>, into: IrVariable },
    Return { value: IrVariable },

    Phi { options: Vec<IrVariable>, into: IrVariable }

}