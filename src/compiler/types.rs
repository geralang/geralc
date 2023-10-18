
use crate::util::strings::StringIdx;

use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarTypeIdx(usize); // this represents the type group of a variable.

#[derive(Debug, Clone)]
pub struct TypeScope {
    // TypeGroupIdx is an index into this.
    // The index stored in the array is the index of the actual type group.
    var_type_groups: Vec<usize>,
    // This vector stores the actual type groups.
    type_groups: Vec<PossibleTypes>
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
    Object(HashMap<StringIdx, PossibleTypes>, bool),
    Closure(Vec<VarTypeIdx>, VarTypeIdx),
    Variants(HashMap<StringIdx, PossibleTypes>, bool)
}

pub struct TypeScopeTranslation {
    var_type_groups: Vec<usize>,
    mappings: HashMap<usize, VarTypeIdx>
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
        let type_group_index = self.type_groups.len();
        self.type_groups.push(PossibleTypes::Any);
        let var_index = self.var_type_groups.len();
        self.var_type_groups.push(type_group_index);
        return VarTypeIdx(var_index);
    }

    // get the possible types of the type group of a given var type index
    pub fn get_group_types(&self, var: &VarTypeIdx) -> &PossibleTypes {
        let i = self.get_group_internal_index(var);
        &self.type_groups[i]
    }
    pub fn get_group_types_mut(&mut self, var: &VarTypeIdx) -> &mut PossibleTypes {
        let i = self.get_group_internal_index(var);
        &mut self.type_groups[i]
    }

    pub fn get_group_internal_index(&self, var: &VarTypeIdx) -> usize {
        self.var_type_groups[var.0]
    }
    pub fn get_group_types_from_internal_index(&self, idx: usize) -> &PossibleTypes {
        &self.type_groups[idx]
    }

    pub fn insert_type_scope(&mut self, inserted: &TypeScope) -> TypeScopeTranslation {
        let mut mappings = HashMap::new();
        for internal_group_idx in 0..inserted.type_groups.len() {
            mappings.insert(internal_group_idx, self.register_variable());
        }
        let translation = TypeScopeTranslation {
            var_type_groups: inserted.var_type_groups.clone(),
            mappings: mappings.clone()
        };
        for (old_internal_idx, new_idx) in &mappings {
            *self.get_group_types_mut(new_idx) = translation.translate(&inserted.type_groups[*old_internal_idx]);
        }
        translation
    }

    pub fn limit_possible_types(&mut self, a: &PossibleTypes, b: &PossibleTypes) -> Option<PossibleTypes> {
        match (a, b) {
            (PossibleTypes::Any, _) => {
                Some(b.clone())
            }
            (_, PossibleTypes::Any) => {
                Some(a.clone())
            }
            (PossibleTypes::OfGroup(a_group), PossibleTypes::OfGroup(b_group)) => {
                if let Some(new_type) = self.limit_possible_types(&self.get_group_types(a_group).clone(), &self.get_group_types(b_group).clone()) {
                    *self.get_group_types_mut(a_group) = new_type.clone();
                    *self.get_group_types_mut(b_group) = new_type;
                } else {
                    return None;
                }
                let a_group_idx = self.get_group_internal_index(a_group);
                let b_group_idx = self.get_group_internal_index(b_group);
                if a_group_idx != b_group_idx {
                    self.type_groups.remove(b_group_idx);
                    for var_idx in &mut self.var_type_groups {
                        if *var_idx == b_group_idx {
                            *var_idx = a_group_idx;
                        }
                        if *var_idx > b_group_idx {
                            *var_idx -= 1;
                        }
                    }
                }
                Some(PossibleTypes::OfGroup(a_group.clone()))
            },
            (PossibleTypes::OfGroup(a_group), _) => {
                if let Some(new_type) = self.limit_possible_types(&self.get_group_types(a_group).clone(), b) {
                    self.type_groups[self.var_type_groups[a_group.0]] = new_type.clone();
                    Some(PossibleTypes::OfGroup(a_group.clone()))
                } else {
                    None
                }
            }
            (_, PossibleTypes::OfGroup(b_group)) => {
                if let Some(new_type) = self.limit_possible_types(a, &self.get_group_types(b_group).clone()) {
                    self.type_groups[self.var_type_groups[b_group.0]] = new_type.clone();
                    Some(PossibleTypes::OfGroup(b_group.clone()))
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
            (
                Type::Array(a_element_type),
                Type::Array(b_element_type)
            ) => {
                if let Some(new_element_type) = self.limit_possible_types(a_element_type, b_element_type) {
                    Some(Type::Array(new_element_type))
                } else { None }
            }
            (
                Type::Object(a_member_types, a_fixed),
                Type::Object(b_member_types, b_fixed)
            ) => {
                let mut new_member_types: HashMap<StringIdx, PossibleTypes> = a_member_types.clone();
                if *a_fixed {
                    for (member_name, _) in b_member_types {
                        if !a_member_types.contains_key(member_name) { return None; }
                    }
                }
                if *b_fixed {
                    for (member_name, _) in a_member_types {
                        if !b_member_types.contains_key(member_name) { return None; }
                    }
                }
                for (member_name, b_member_type) in b_member_types {
                    if let Some(a_member_type) = a_member_types.get(member_name) {
                        if let Some(new_member_type) = self.limit_possible_types(a_member_type, b_member_type) {
                            new_member_types.insert(*member_name, new_member_type);
                        } else {
                            return None;
                        }
                    } else {
                        new_member_types.insert(*member_name, b_member_type.clone());
                    }
                }
                Some(Type::Object(new_member_types, *a_fixed && *b_fixed))
            }
            (
                Type::Closure(a_param_groups, a_returned_group),
                Type::Closure(b_param_groups, b_returned_group)
            ) => {
                if a_param_groups.len() != b_param_groups.len() { return None; }
                let mut n_param_groups = Vec::new();
                let n_returned_group = self.register_variable();
                for p in 0..a_param_groups.len() {
                    let n_param_group = self.register_variable();
                    if let None = self.limit_possible_types(
                        &PossibleTypes::OfGroup(n_param_group),
                        &PossibleTypes::OfGroup(a_param_groups[p])
                    ) { return None; }
                    if let None = self.limit_possible_types(
                        &PossibleTypes::OfGroup(n_param_group),
                        &PossibleTypes::OfGroup(b_param_groups[p])
                    ) { return None; }
                    n_param_groups.push(n_param_group);
                }
                if let None = self.limit_possible_types(
                    &PossibleTypes::OfGroup(n_returned_group),
                    &PossibleTypes::OfGroup(*a_returned_group)
                ) { return None; }
                if let None = self.limit_possible_types(
                    &PossibleTypes::OfGroup(n_returned_group),
                    &PossibleTypes::OfGroup(*b_returned_group)
                ) { return None; }
                Some(Type::Closure(n_param_groups, n_returned_group))
            }
            (
                Type::Variants(a_variant_types, a_fixed),
                Type::Variants(b_variant_types, b_fixed)
            ) => {
                let mut new_variant_types: HashMap<StringIdx, PossibleTypes> = a_variant_types.clone();
                if *a_fixed {
                    for (variant_name, _) in b_variant_types {
                        if !a_variant_types.contains_key(variant_name) { return None; }
                    }
                }
                if *b_fixed {
                    for (variant_name, _) in a_variant_types {
                        if !b_variant_types.contains_key(variant_name) { return None; }
                    }
                }
                for (variant_name, b_variant_type) in b_variant_types {
                    if let Some(a_member_type) = a_variant_types.get(variant_name) {
                        if let Some(new_variant_type) = self.limit_possible_types(a_member_type, b_variant_type) {
                            new_variant_types.insert(*variant_name, new_variant_type);
                        } else {
                            return None;
                        }
                    } else {
                        new_variant_types.insert(*variant_name, b_variant_type.clone());
                    }
                }
                Some(Type::Variants(new_variant_types, *a_fixed || *b_fixed))
            }
            _ => if std::mem::discriminant(a) == std::mem::discriminant(b) { Some(a.clone()) } else { None }
        }
    }
}

impl TypeScopeTranslation {
    pub fn translate(&self, translated: &PossibleTypes) -> PossibleTypes {
        match translated {
            PossibleTypes::Any => PossibleTypes::Any,
            PossibleTypes::OneOf(possible_types) => PossibleTypes::OneOf(
                possible_types.iter().map(|t| self.translate_type(t)).collect()
            ),
            PossibleTypes::OfGroup(old_group) => PossibleTypes::OfGroup(
                *self.mappings.get(&self.var_type_groups[old_group.0]).expect("mapping should exist")
            )
        }
    }

    fn translate_type(&self, translated: &Type) -> Type {
        match translated {
            Type::Unit => Type::Unit,
            Type::Boolean => Type::Boolean,
            Type::Integer => Type::Integer,
            Type::Float => Type::Float,
            Type::String => Type::String,
            Type::Array(element_types) => Type::Array(
                self.translate(element_types)
            ),
            Type::Object(member_types, fixed) => Type::Object(
                member_types.iter().map(|(member_name, member_types)| {
                    (*member_name, self.translate(member_types))
                }).collect(),
                *fixed
            ),
            Type::Closure(old_param_groups, old_return_group) => Type::Closure(
                old_param_groups.iter().map(|old_group| {
                    *self.mappings.get(&self.var_type_groups[old_group.0]).expect("mapping should exist")
                }).collect(),
                *self.mappings.get(&self.var_type_groups[old_return_group.0]).expect("mapping should exist")
            ),
            Type::Variants(variant_types, fixed) => Type::Variants(
                variant_types.iter().map(|(variant_name, variant_types)| {
                    (*variant_name, self.translate(variant_types))
                }).collect(),
                *fixed
            )
        }
    }
}