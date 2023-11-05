
use crate::util::strings::StringIdx;

use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarTypeIdx(usize); // this represents the type group of a variable.

#[derive(Debug, Clone)]
pub struct TypeScope {
    // VarTypeIdx is an index into this.
    // The index stored in the array is the index of the actual type group.
    var_type_groups: Vec<usize>,
    // This vector stores the actual type groups.
    type_groups: Vec<Option<Vec<Type>>>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Boolean,
    Integer,
    Float,
    String,
    Array(VarTypeIdx),
    Object(HashMap<StringIdx, VarTypeIdx>, bool),
    ConcreteObject(Vec<(StringIdx, Type)>),
    Closure(Vec<VarTypeIdx>, VarTypeIdx, Option<HashMap<StringIdx, VarTypeIdx>>),
    Variants(HashMap<StringIdx, VarTypeIdx>, bool)
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
        self.type_groups.push(None);
        let var_index = self.var_type_groups.len();
        self.var_type_groups.push(type_group_index);
        return VarTypeIdx(var_index);
    }

    pub fn register_with_types(&mut self, types: Option<Vec<Type>>) -> VarTypeIdx {
        let var = self.register_variable();
        *self.get_group_types_mut(var) = types;
        return var;
    }

    // get the possible types of the type group of a given var type index
    pub fn get_group_types(&self, var: VarTypeIdx) -> &Option<Vec<Type>> {
        let i = self.get_group_internal_index(var);
        &self.type_groups[i]
    }
    pub fn get_group_types_mut(&mut self, var: VarTypeIdx) -> &mut Option<Vec<Type>> {
        let i = self.get_group_internal_index(var);
        &mut self.type_groups[i]
    }

    pub fn get_group_internal_index(&self, var: VarTypeIdx) -> usize {
        self.var_type_groups[var.0]
    }
    pub fn get_group_types_from_internal_index(&self, idx: usize) -> &Option<Vec<Type>> {
        &self.type_groups[idx]
    }

    pub fn limit_possible_types(&mut self, a: VarTypeIdx, b: VarTypeIdx) -> Option<VarTypeIdx> {
        self.limit_possible_types_internal(a, b, Vec::new())
    }

    pub fn merge(&mut self, a: VarTypeIdx, b: VarTypeIdx, encountered: Vec<usize>) -> Option<VarTypeIdx> {
        let limited_types = if let Some(a_types) = self.get_group_types(a) {
            let a_types = a_types.clone();
            if let Some(b_types) = self.get_group_types(b) {
                let b_types = b_types.clone();
                let mut limited: Vec<Type> = Vec::new();
                for one_of_a in &a_types {
                    for one_of_b in &b_types {
                        if let Some(new_possible_type) = self.types_limited(
                            &one_of_a,
                            &one_of_b,
                            encountered.clone()
                        ) {
                            limited.push(new_possible_type);
                            break;
                        }
                    }
                }
                if limited.len() == 0 { return None; }
                Some(limited)
            } else { Some(a_types.clone()) }
        } else {
            if let Some(b_types) = self.get_group_types(b) { Some(b_types.clone()) }
            else { None }
        };
        *self.get_group_types_mut(a) = limited_types;
        let a_group_idx = self.get_group_internal_index(a);
        let b_group_idx = self.get_group_internal_index(b);
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
        Some(a)
    }

    pub fn limit_possible_types_internal(
        &mut self,
        a: VarTypeIdx,
        b: VarTypeIdx,
        mut encountered: Vec<usize>,
    ) -> Option<VarTypeIdx> {
        if encountered.contains(&self.get_group_internal_index(a))
            && encountered.contains(&self.get_group_internal_index(b)) {
            return Some(a)
        }
        encountered.push(self.get_group_internal_index(a));
        encountered.push(self.get_group_internal_index(b));
        self.merge(a, b, encountered)
    }

    fn types_limited(
        &mut self,
        a: &Type,
        b: &Type,
        encountered: Vec<usize>
    ) -> Option<Type> {
        match (a, b) {
            (Type::ConcreteObject(member_types), b) => {
                let object_type = Type::Object(member_types.iter().map(|(member_name, member_type)|
                    (*member_name, self.register_with_types(Some(vec![member_type.clone()])))
                ).collect(), false);
                self.types_limited(
                    &object_type,
                    b,
                    encountered
                )
            }
            (a, Type::ConcreteObject(member_types)) => {
                let object_type = Type::Object(member_types.iter().map(|(member_name, member_type)|
                    (*member_name, self.register_with_types(Some(vec![member_type.clone()])))
                ).collect(), false);
                self.types_limited(
                    a,
                    &object_type,
                    encountered
                )
            }
            (
                Type::Array(a_element_type),
                Type::Array(b_element_type)
            ) => {
                if let Some(new_element_type) = self.limit_possible_types_internal(
                    *a_element_type,
                    *b_element_type,
                    encountered
                ) {
                    Some(Type::Array(new_element_type))
                } else { None }
            }
            (
                Type::Object(a_member_types, a_fixed),
                Type::Object(b_member_types, b_fixed)
            ) => {
                let mut new_member_types: HashMap<StringIdx, VarTypeIdx> = a_member_types.clone();
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
                        if let Some(new_member_type) = self.limit_possible_types_internal(
                            *a_member_type,
                            *b_member_type,
                            encountered.clone()
                        ) {
                            new_member_types.insert(*member_name, new_member_type);
                        } else {
                            return None;
                        }
                    } else {
                        new_member_types.insert(*member_name, b_member_type.clone());
                    }
                }
                Some(Type::Object(new_member_types, *a_fixed || *b_fixed))
            }
            (
                Type::Closure(a_param_groups, a_returned_group, a_captures),
                Type::Closure(b_param_groups, b_returned_group, b_captures)
            ) => {
                if a_param_groups.len() != b_param_groups.len() { return None; }
                let mut n_param_groups = Vec::new();
                let n_returned_group = self.register_variable();
                for p in 0..a_param_groups.len() {
                    let n_param_group = self.register_variable();
                    if let None = self.limit_possible_types_internal(
                        n_param_group,
                        a_param_groups[p],
                        encountered.clone()
                    ) { return None; }
                    if let None = self.limit_possible_types_internal(
                        n_param_group,
                        b_param_groups[p],
                        encountered.clone()
                    ) { return None; }
                    n_param_groups.push(n_param_group);
                }
                if let None = self.limit_possible_types_internal(
                    n_returned_group,
                    *a_returned_group,
                    encountered.clone()
                ) { return None; }
                if let None = self.limit_possible_types_internal(
                    n_returned_group,
                    *b_returned_group,
                    encountered
                ) { return None; }
                Some(Type::Closure(
                    n_param_groups,
                    n_returned_group,
                    if let Some(_) = a_captures { a_captures.clone() }
                    else if let Some(_) = b_captures {b_captures.clone() }
                    else { None }
                ))
            }
            (
                Type::Variants(a_variant_types, a_fixed),
                Type::Variants(b_variant_types, b_fixed)
            ) => {
                let mut new_variant_types: HashMap<StringIdx, VarTypeIdx> = a_variant_types.clone();
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
                        if let Some(new_variant_type) = self.limit_possible_types_internal(
                            *a_member_type,
                            *b_variant_type,
                            encountered.clone()
                        ) {
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

pub struct TypeGroupDuplications {
    mapped: HashSet<usize>,
    mappings: HashMap<VarTypeIdx, VarTypeIdx>
}

impl TypeGroupDuplications {
    pub fn new() -> TypeGroupDuplications {
        TypeGroupDuplications {
            mapped: HashSet::new(),
            mappings: HashMap::new()
        }
    }

    pub fn duplicate(&mut self, var: VarTypeIdx, type_scope: &mut TypeScope) {
        let group_idx = type_scope.get_group_internal_index(var);
        if self.mapped.contains(&group_idx) { return; }
        let new_var_idx = type_scope.register_variable();
        *type_scope.get_group_types_mut(new_var_idx) = type_scope.get_group_types(var).clone();
        for var_idx in 0..type_scope.var_type_groups.len() {
            let var_idx = VarTypeIdx(var_idx);
            if type_scope.get_group_internal_index(var_idx) == group_idx {
                self.mappings.insert(var_idx, new_var_idx);
            }
        }
        self.mapped.insert(group_idx);
    }

    pub fn apply(&self, group: VarTypeIdx) -> VarTypeIdx {
        self.mappings.get(&group).map(|g| *g).unwrap_or(group)
    }
}
