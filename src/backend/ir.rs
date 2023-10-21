
use std::collections::HashMap;

use crate::{frontend::{
    modules::NamespacePath,
    types::{TypeScope, VarTypeIdx, PossibleTypes}
}, util::strings::StringIdx};

use crate::backend::interpreter::Value;


#[derive(Debug, Clone)]
pub enum IrSymbol {
    Procedure {
        path: NamespacePath,
        type_scope: TypeScope, parameter_types: Vec<VarTypeIdx>, return_types: VarTypeIdx,
        variables: Vec<PossibleTypes>,
        body: Vec<IrInstruction>
    },
    Variable {
        path: NamespacePath,
        value: Value
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IrVariable {
    index: usize,
    version: usize
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
    LoadGlobalVariable { path: NamespacePath, into: IrVariable },

    GetObjectMember { accessed: IrVariable, member: StringIdx, into: IrVariable },
    SetObjectMember { value: IrVariable, accessed: IrVariable, member: StringIdx },

    GetArrayElement { accessed: IrVariable, index: IrVariable, into: IrVariable },
    SetArrayElement { value: IrVariable, accessed: IrVariable, index: IrVariable },

    Move { x: IrVariable, into: IrVariable },

    Add { a: IrVariable, b: IrVariable, into: IrVariable },
    Subtract { a: IrVariable, b: IrVariable, into: IrVariable },
    Multiply { a: IrVariable, b: IrVariable, into: IrVariable },
    Divide { a: IrVariable, b: IrVariable, into: IrVariable },
    Modulo { a: IrVariable, b: IrVariable, into: IrVariable },
    Negate { a: IrVariable, b: IrVariable, into: IrVariable },

    LessThan { a: IrVariable, b: IrVariable, into: IrVariable },
    LessThanEquals { a: IrVariable, b: IrVariable, into: IrVariable },
    GreaterThan { a: IrVariable, b: IrVariable, into: IrVariable },
    GreaterThanEqual { a: IrVariable, b: IrVariable, into: IrVariable },
    Equals { a: IrVariable, b: IrVariable, into: IrVariable },
    NotEquals { a: IrVariable, b: IrVariable, into: IrVariable },

    BranchOnValue { value: IrVariable, branches: Vec<(Value, Vec<IrInstruction>)> },
    BranchOnVariant { value: IrVariable, branches: Vec<(StringIdx, IrVariable, Vec<IrInstruction>)> },

    CallProcedure { path: NamespacePath, arguments: Vec<IrVariable>, into: Option<IrVariable> },
    CallClosure { called: IrVariable, arguments: Vec<IrVariable>, into: Option<IrVariable> },

    Return { value: IrVariable }

}