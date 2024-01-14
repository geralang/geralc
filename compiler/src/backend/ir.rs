
use std::collections::HashMap;

use crate::util::{
    source::SourceRange,
    strings::StringIdx
};
use crate::frontend::{
    modules::NamespacePath,
    types::TypeGroup
};
use crate::backend::interpreter::Value;


#[derive(Debug, Clone)]
pub enum IrSymbol {
    Procedure {
        path: NamespacePath,
        variant: usize,
        parameter_types: Vec<TypeGroup>, return_type: TypeGroup,
        variables: Vec<TypeGroup>,
        body: Vec<IrInstruction>
    },
    ExternalProcedure {
        path: NamespacePath,
        backing: StringIdx,
        parameter_types: Vec<TypeGroup>, return_type: TypeGroup
    },
    BuiltInProcedure {
        path: NamespacePath,
        variant: usize,
        parameter_types: Vec<TypeGroup>, return_type: TypeGroup
    },
    Variable {
        path: NamespacePath,
        value_type: TypeGroup,
        value: Value
    },
    ExternalVariable {
        path: NamespacePath,
        backing: StringIdx,
        value_type: TypeGroup
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
        parameter_types: Vec<TypeGroup>, return_type: TypeGroup, captured: HashMap<StringIdx, IrVariable>,
        variables: Vec<TypeGroup>, body: Vec<IrInstruction>, into: IrVariable
    },
    LoadValue { value: Value, into: IrVariable },

    GetObjectMember { accessed: IrVariable, member: StringIdx, into: IrVariable },
    SetObjectMember { value: IrVariable, accessed: IrVariable, member: StringIdx },

    GetArrayElement { accessed: IrVariable, index: IrVariable, into: IrVariable, source: SourceRange },
    SetArrayElement { value: IrVariable, accessed: IrVariable, index: IrVariable, source: SourceRange },

    GetClosureCapture { name: StringIdx, into: IrVariable },
    SetClosureCapture { value: IrVariable, name: StringIdx },

    Move { from: IrVariable, into: IrVariable },

    Add { a: IrVariable, b: IrVariable, into: IrVariable },
    Subtract { a: IrVariable, b: IrVariable, into: IrVariable },
    Multiply { a: IrVariable, b: IrVariable, into: IrVariable },
    Divide { a: IrVariable, b: IrVariable, into: IrVariable, source: SourceRange },
    Modulo { a: IrVariable, b: IrVariable, into: IrVariable, source: SourceRange },
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

    Call { path: NamespacePath, variant: usize, arguments: Vec<IrVariable>, into: IrVariable, source: SourceRange },
    CallClosure { called: IrVariable, arguments: Vec<IrVariable>, into: IrVariable, source: SourceRange },
    Return { value: IrVariable },

    Phi { options: Vec<IrVariable>, into: IrVariable }

}