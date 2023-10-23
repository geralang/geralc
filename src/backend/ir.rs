
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
    variants: Vec<HashMap<StringIdx, IrType>>
}

impl IrTypeBank {
    pub fn new() -> IrTypeBank {
        IrTypeBank {
            arrays: Vec::new(),
            objects: Vec::new(),
            concrete_objects: Vec::new(),
            variants: Vec::new()
        }
    }

    pub fn insert_array(&mut self, element_type: IrType) -> IrArrayTypeIdx {
        for i in 0..self.arrays.len() {
            if self.arrays[i] == element_type { return IrArrayTypeIdx(i); }
        }
        self.arrays.push(element_type);
        return IrArrayTypeIdx(self.arrays.len() - 1);
    }
    pub fn get_array(&self, idx: IrArrayTypeIdx) -> &IrType { &self.arrays[idx.0] }

    pub fn insert_object(&mut self, member_types: HashMap<StringIdx, IrType>) -> IrObjectTypeIdx {
        for i in 0..self.objects.len() {
            if self.objects[i] == member_types { return IrObjectTypeIdx(i); }
        }
        self.objects.push(member_types);
        return IrObjectTypeIdx(self.objects.len() - 1);
    }
    pub fn get_object(&self, idx: IrObjectTypeIdx) -> &HashMap<StringIdx, IrType> { &self.objects[idx.0] }

    pub fn insert_concrete_object(&mut self, member_types: Vec<(StringIdx, IrType)>) -> IrConcreteObjectTypeIdx {
        for i in 0..self.concrete_objects.len() {
            if self.concrete_objects[i] == member_types { return IrConcreteObjectTypeIdx(i); }
        }
        self.concrete_objects.push(member_types);
        return IrConcreteObjectTypeIdx(self.concrete_objects.len() - 1);
    }
    pub fn get_concrete_object(&self, idx: IrConcreteObjectTypeIdx) -> &Vec<(StringIdx, IrType)> { &self.concrete_objects[idx.0] }

    pub fn insert_variants(&mut self, variant_types: HashMap<StringIdx, IrType>) -> IrVariantTypeIdx {
        for i in 0..self.variants.len() {
            if self.variants[i] == variant_types { return IrVariantTypeIdx(i); }
        }
        self.variants.push(variant_types);
        return IrVariantTypeIdx(self.variants.len() - 1);
    }
    pub fn get_variants(&self, idx: IrVariantTypeIdx) -> &HashMap<StringIdx, IrType> { &self.variants[idx.0] }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct IrArrayTypeIdx(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct IrObjectTypeIdx(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct IrConcreteObjectTypeIdx(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct IrVariantTypeIdx(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrType {
    Unit,
    Boolean,
    Integer,
    Float,
    String,
    Array(IrArrayTypeIdx),
    Object(IrObjectTypeIdx),
    ConcreteObject(IrConcreteObjectTypeIdx),
    Variants(IrVariantTypeIdx)
}


#[derive(Debug, Clone, Copy)]
pub struct IrVariable {
    pub index: usize,
    pub version: usize,
    pub variable_type: IrType,
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