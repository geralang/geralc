
use crate::util::{
    strings::StringIdx,
    error::{Error, ErrorSection, ErrorType}
};

use std::collections::HashMap;

#[derive(Debug)]
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

    // restricts the type group of a given var type index to only be one of a given amount of possible types
    pub fn limit_group_types(&mut self, var: &VarTypeIdx, only: &PossibleTypes) -> Option<Error> {
        let group_types = &mut self.type_groups[self.var_type_groups[var.0]].possible_types;
        if let Some(new_group_types) = group_types.limited_to(only) {
            *group_types = new_group_types;
            None
        } else {
            Some(Error::new([
                ErrorSection::Error(ErrorType::NoPossibleTypes(format!("{:?}", group_types), format!("{:?}", only)))
            ].into()))
        }
    }

    // merges the type groups of a given var type index with the one of another
    pub fn merge_groups(&mut self, first: &VarTypeIdx, second: &VarTypeIdx) -> Option<Error> {
        if let Some(error) = self.limit_group_types(first, &self.get_group_types(second).clone()) {
            return Some(error);
        }
        let first_group_idx = self.var_type_groups[first.0];
        let second_group_idx = self.var_type_groups[second.0];
        let second_group = &mut self.type_groups[self.var_type_groups[second.0]];
        for var_idx in &second_group.members {
            self.var_type_groups[var_idx.0] = first_group_idx;
        }
        self.type_groups.remove(second_group_idx);
        for var_idx in &mut self.var_type_groups {
            if *var_idx <= second_group_idx { continue; }
            *var_idx -= 1;
        }
        None
    }
}

#[derive(Debug, Clone)]
pub enum PossibleTypes {
    Any,
    OneOf(Vec<Type>)
}

impl PossibleTypes {
    pub fn limited_to(&self, only: &PossibleTypes) -> Option<PossibleTypes> {
        match self {
            PossibleTypes::Any => Some(only.clone()),
            PossibleTypes::OneOf(self_possible_types) => match only {
                PossibleTypes::Any => Some(PossibleTypes::OneOf(self_possible_types.clone())),
                PossibleTypes::OneOf(only_possible_types) => {
                    let mut limited: Vec<Type> = Vec::new();
                    for self_possible_type in self_possible_types {
                        for only_possible_type in only_possible_types {
                            if let Some(new_possible_type) = self_possible_type.limited_to(only_possible_type) {
                                limited.push(new_possible_type);
                                break;
                            }
                        }
                    }
                    if limited.len() == 0 { None } else { Some(PossibleTypes::OneOf(limited)) }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Boolean,
    Integer,
    Float,
    String,
    Array(PossibleTypes),
    Object(HashMap<StringIdx, PossibleTypes>)
}

impl Type {
    pub fn limited_to(&self, only: &Type) -> Option<Type> {
        match (self, only) {
            (Type::Array(self_element_type), Type::Array(only_element_type)) => {
                if let Some(new_element_type) = self_element_type.limited_to(only_element_type) {
                    Some(Type::Array(new_element_type))
                } else { None }
            }
            (Type::Object(self_member_types), Type::Object(to_member_types)) => {
                let mut new_member_types = self_member_types.clone();
                for (to_member_name, to_member_type) in to_member_types {
                    if let Some(self_member_type) = self_member_types.get(to_member_name) {
                        if let Some(new_member_type) = self_member_type.limited_to(to_member_type) {
                            new_member_types.insert(*to_member_name, new_member_type);
                        } else {
                            return None;
                        }
                    } else {
                        new_member_types.insert(*to_member_name, to_member_type.clone());
                    }
                }
                Some(Type::Object(new_member_types))
            }
            _ => if std::mem::discriminant(self) == std::mem::discriminant(only) { Some(self.clone()) } else { None }
        }
    }
}