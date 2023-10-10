
use crate::util::{
    strings::StringIdx,
    error::{Error, ErrorSection, ErrorType}
};

use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub struct VarTypeIdx(usize); // this represents the type group of a variable.

#[derive(Debug)]
pub struct TypeScope {
    // TypeGroupIdx is an index into this.
    // The index stored in the array is the index of the actual type group.
    var_type_groups: Vec<usize>,
    // This vector stores the actual type groups.
    type_groups: Vec<TypeGroup>
}

#[derive(Debug)]
pub struct TypeGroup {
    possible_types: PossibleTypes,
    members: Vec<VarTypeIdx>
}

#[derive(Debug, Clone)]
pub enum PossibleTypes {
    Any,
    OneOf(Vec<Type>),
    OfGroup(VarTypeIdx)
}

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Boolean,
    Integer,
    Float,
    String,
    Array(PossibleTypes),
    Object(HashMap<StringIdx, PossibleTypes>)
}

impl TypeScope {
    pub fn new() -> TypeScope {
        TypeScope {
            var_type_groups: Vec::new(),
            type_groups: Vec::new()
        }
    }

    // creates a new type group with any type and returns the index.
    pub fn register_variable(&mut self) -> VarTypeIdx {
        let var_index = self.var_type_groups.len();
        let type_group_index = self.type_groups.len();
        self.var_type_groups.push(type_group_index);
        self.type_groups.push(TypeGroup {
            possible_types: PossibleTypes::Any,
            members: vec![VarTypeIdx(var_index)]
        });
        return VarTypeIdx(var_index);
    }

    // get the possible types of the type group of a given var type index
    pub fn get_group_types(&self, var: &VarTypeIdx) -> &PossibleTypes {
        &self.type_groups[self.var_type_groups[var.0]].possible_types
    }
    pub fn get_group_types_mut(&mut self, var: &VarTypeIdx) -> &mut PossibleTypes {
        &mut self.type_groups[self.var_type_groups[var.0]].possible_types
    }

    pub fn limit_possible_types(&mut self, a: &PossibleTypes, b: &PossibleTypes) -> Result<PossibleTypes, Error> {
        if let Some(new_type) = self.possible_types_limited(a, b) {
            Ok(new_type)
        } else {
            Err(Error::new([
                ErrorSection::Error(ErrorType::NoPossibleTypes(format!("{:?}", a), format!("{:?}", b)))
            ].into()))
        }
    }

    fn possible_types_limited(&mut self, a: &PossibleTypes, b: &PossibleTypes) -> Option<PossibleTypes> {
        match (a, b) {
            (PossibleTypes::Any, _) => {
                Some(b.clone())
            }
            (_, PossibleTypes::Any) => {
                Some(a.clone())
            }
            (PossibleTypes::OfGroup(a_group), PossibleTypes::OfGroup(b_group)) => {
                if let Some(new_type) = self.possible_types_limited(&self.get_group_types(a_group).clone(), &self.get_group_types(b_group).clone()) {
                    self.type_groups[self.var_type_groups[a_group.0]].possible_types = new_type;
                } else {
                    return None;
                }
                let a_group_idx = self.var_type_groups[a_group.0];
                let b_group_idx = self.var_type_groups[b_group.0];
                let b_group = &mut self.type_groups[b_group_idx];
                for var_idx in &b_group.members {
                    self.var_type_groups[var_idx.0] = a_group_idx;
                }
                self.type_groups.remove(b_group_idx);
                for var_idx in &mut self.var_type_groups {
                    if *var_idx <= b_group_idx { continue; }
                    *var_idx -= 1;
                }
                Some(PossibleTypes::OfGroup(a_group.clone()))
            },
            (PossibleTypes::OfGroup(a_group), _) => {
                if let Some(new_type) = self.possible_types_limited(&self.get_group_types(a_group).clone(), b) {
                    self.type_groups[self.var_type_groups[a_group.0]].possible_types = new_type.clone();
                    Some(new_type)
                } else {
                    None
                }
            }
            (_, PossibleTypes::OfGroup(b_group)) => {
                if let Some(new_type) = self.possible_types_limited(a, &self.get_group_types(b_group).clone()) {
                    self.type_groups[self.var_type_groups[b_group.0]].possible_types = new_type.clone();
                    Some(new_type)
                } else {
                    None
                }
            }
            (PossibleTypes::OneOf(one_of_types_a), PossibleTypes::OneOf(one_of_types_b)) => {
                let mut limited: Vec<Type> = Vec::new();
                for one_of_a in one_of_types_a {
                    for one_of_b in one_of_types_b {
                        if let Some(new_possible_type) = self.types_limited(one_of_a, one_of_b) {
                            limited.push(new_possible_type);
                            break;
                        }
                    }
                }
                if limited.len() == 0 { None } else { Some(PossibleTypes::OneOf(limited)) }
            }
        }
    }

    fn types_limited(&mut self, a: &Type, b: &Type) -> Option<Type> {
        match (a, b) {
            (Type::Array(a_element_type), Type::Array(b_element_type)) => {
                if let Some(new_element_type) = self.possible_types_limited(a_element_type, b_element_type) {
                    Some(Type::Array(new_element_type))
                } else { None }
            }
            (Type::Object(a_member_types), Type::Object(b_member_types)) => {
                let mut new_member_types: HashMap<StringIdx, PossibleTypes> = a_member_types.clone();
                for (member_name, b_member_type) in b_member_types {
                    if let Some(a_member_type) = a_member_types.get(member_name) {
                        if let Some(new_member_type) = self.possible_types_limited(a_member_type, b_member_type) {
                            new_member_types.insert(*member_name, new_member_type);
                        } else {
                            return None;
                        }
                    } else {
                        new_member_types.insert(*member_name, b_member_type.clone());
                    }
                }
                Some(Type::Object(new_member_types))
            }
            _ => if std::mem::discriminant(a) == std::mem::discriminant(b) { Some(a.clone()) } else { None }
        }
    }
}