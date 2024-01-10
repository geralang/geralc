use std::collections::{HashMap, HashSet};

use crate::util::strings::StringIdx;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeGroup(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeScope(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ArrayType(usize);
impl ArrayType { pub fn get_internal_id(&self) -> usize { self.0 } }

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ObjectType(usize);
impl ObjectType { pub fn get_internal_id(&self) -> usize { self.0 } }

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ConcreteObjectType(usize);
impl ConcreteObjectType { pub fn get_internal_id(&self) -> usize { self.0 } }

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ClosureType(usize);
impl ClosureType { pub fn get_internal_id(&self) -> usize { self.0 } }

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct VariantsType(usize);
impl VariantsType { pub fn get_internal_id(&self) -> usize { self.0 } }

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Any,
    Unit,
    Boolean,
    Integer,
    Float,
    String,
    Array(ArrayType),
    Object(ObjectType),
    ConcreteObject(ConcreteObjectType),
    Closure(ClosureType),
    Variants(VariantsType)
}

#[derive(Debug, Clone)]
pub struct TypeMap {
    next_scope_id: usize,
    groups: Vec<usize>,
    internal_groups: Vec<(HashSet<Type>, HashSet<usize>)>,
    arrays: Vec<TypeGroup>,
    objects: Vec<(HashMap<StringIdx, TypeGroup>, bool)>,
    concrete_objects: Vec<Vec<(StringIdx, TypeGroup)>>,
    closures: Vec<(
        Vec<TypeGroup>, TypeGroup, Option<HashMap<StringIdx, TypeGroup>>
    )>,
    variants: Vec<(HashMap<StringIdx, TypeGroup>, bool)>
}

impl TypeMap {
    pub fn new() -> TypeMap {
        TypeMap {
            next_scope_id: 0,
            groups: Vec::new(),
            internal_groups: Vec::new(),
            arrays: Vec::new(),
            objects: Vec::new(),
            concrete_objects: Vec::new(),
            closures: Vec::new(),
            variants: Vec::new()
        }
    }

    pub fn create_scope(&mut self) -> TypeScope {
        let id = self.next_scope_id;
        self.next_scope_id += 1;
        return TypeScope(id);
    }

    pub fn internal_arrays(&self) -> &Vec<TypeGroup> { &self.arrays }
    pub fn insert_array(
        &mut self, element_type: TypeGroup
    ) -> ArrayType {
        let idx = self.arrays.len();
        self.arrays.push(element_type);
        return ArrayType(idx);
    }
    pub fn insert_dedup_array(&mut self, v: TypeGroup) -> ArrayType {
        for idx in 0..self.arrays.len() {
            if !TypeMap::internal_arrays_eq(
                self.arrays[idx], self, v, self, &mut HashSet::new()
            ) { continue; }
            return ArrayType(idx);
        }
        return self.insert_array(v);
    }
    pub fn array(&self, array: ArrayType) -> TypeGroup {
        self.arrays[array.0]
    }

    pub fn internal_objects(&self)
        -> &Vec<(HashMap<StringIdx, TypeGroup>, bool)> { &self.objects }
    pub fn insert_object(
        &mut self, member_types: HashMap<StringIdx, TypeGroup>, fixed: bool
    ) -> ObjectType {
        let object_value = (member_types, fixed);
        let idx = self.objects.len();
        self.objects.push(object_value);
        return ObjectType(idx);
    }
    pub fn insert_dedup_object(&mut self, v: (HashMap<StringIdx, TypeGroup>, bool)) -> ObjectType {
        for idx in 0..self.objects.len() {
            if !TypeMap::internal_objects_eq(
                &self.objects[idx], self, &v, self, &mut HashSet::new()
            ) { continue; }
            return ObjectType(idx);
        }
        return self.insert_object(v.0, v.1);
    }
    pub fn object(
        &self, object: ObjectType
    ) -> &(HashMap<StringIdx, TypeGroup>, bool) {
        &self.objects[object.0]
    }

    pub fn internal_concrete_objects(&self)
        -> &Vec<Vec<(StringIdx, TypeGroup)>> {
        &self.concrete_objects
    }
    pub fn insert_concrete_object(
        &mut self, member_types: Vec<(StringIdx, TypeGroup)>
    ) -> ConcreteObjectType {
        let idx = self.concrete_objects.len();
        self.concrete_objects.push(member_types);
        return ConcreteObjectType(idx);
    }
    pub fn insert_dedup_concrete_object(
        &mut self, v: Vec<(StringIdx, TypeGroup)>
    ) -> ConcreteObjectType {
        for idx in 0..self.concrete_objects.len() {
            if !TypeMap::internal_concrete_objects_eq(
                &self.concrete_objects[idx], self, &v, self, &mut HashSet::new()
            ) { continue; }
            return ConcreteObjectType(idx);
        }
        return self.insert_concrete_object(v);
    }
    pub fn concrete_object(
        &self, concrete_object: ConcreteObjectType
    ) -> &Vec<(StringIdx, TypeGroup)> {
        &self.concrete_objects[concrete_object.0]
    }

    pub fn internal_closures(&self)
        -> &Vec<(
            Vec<TypeGroup>, TypeGroup, Option<HashMap<StringIdx, TypeGroup>>
        )> {
        &self.closures
    }
    pub fn insert_closure(
        &mut self, param_types: Vec<TypeGroup>, return_type: TypeGroup,
        captures: Option<HashMap<StringIdx, TypeGroup>>
    ) -> ClosureType {
        let idx = self.closures.len();
        self.closures.push((param_types, return_type, captures));
        return ClosureType(idx);
    }
    pub fn insert_dedup_closure(
        &mut self, v: (Vec<TypeGroup>, TypeGroup, Option<HashMap<StringIdx, TypeGroup>>)
    ) -> ClosureType {
        for idx in 0..self.closures.len() {
            if !TypeMap::internal_closures_eq(
                &self.closures[idx], self, &v, self, &mut HashSet::new()
            ) { continue; }
            return ClosureType(idx);
        }
        return self.insert_closure(v.0, v.1, v.2);
    }
    pub fn closure(
        &self, closure: ClosureType
    ) -> &(Vec<TypeGroup>, TypeGroup, Option<HashMap<StringIdx, TypeGroup>>) {
        &self.closures[closure.0]
    }

    pub fn internal_variants(&self)
        -> &Vec<(HashMap<StringIdx, TypeGroup>, bool)> { &self.variants }
    pub fn insert_variants(
        &mut self, variants: HashMap<StringIdx, TypeGroup>, fixed: bool
    ) -> VariantsType {
        let idx = self.variants.len();
        self.variants.push((variants, fixed));
        return VariantsType(idx);
    }
    pub fn insert_dedup_variants(&mut self, v: (HashMap<StringIdx, TypeGroup>, bool)) -> VariantsType {
        for idx in 0..self.variants.len() {
            if !TypeMap::internal_variants_eq(
                &self.variants[idx], self, &v, self, &mut HashSet::new()
            ) { continue; }
            return VariantsType(idx);
        }
        return self.insert_variants(v.0, v.1);
    }
    pub fn variants(
        &self, variants: VariantsType
    ) -> &(HashMap<StringIdx, TypeGroup>, bool) {
        &self.variants[variants.0]
    }

    pub fn internal_groups(&self)
        -> &Vec<(HashSet<Type>, HashSet<usize>)> { &self.internal_groups }
    pub fn insert_group(&mut self, types: &[Type], scope: TypeScope) -> TypeGroup {
        let internal_idx = self.internal_groups.len();
        self.internal_groups.push((types.iter().map(|t| *t).collect(), [scope.0].into()));
        let group_idx = self.groups.len();
        self.groups.push(internal_idx);
        return TypeGroup(group_idx);
    }
    pub fn insert_group_with_scopes(&mut self, types: &[Type], scopes: &[TypeScope]) -> TypeGroup {
        let group = self.insert_group(types, TypeScope(0));
        self.internal_groups[self.groups[group.0]].1 = scopes.iter().map(|s| s.0).collect();
        return group;
    }
    pub fn add_scope(&mut self, group: TypeGroup, scope: TypeScope) {
        self.internal_groups[self.groups[group.0]].1.insert(scope.0);
    }
    pub fn add_scope_rec(&mut self, group: TypeGroup, scope: TypeScope) {
        self.internal_groups[self.groups[group.0]].1.insert(scope.0);
        for t in self.group(group).collect::<Vec<Type>>() {
            self.add_scope_type_rec(t, scope);
        }
    }
    fn add_scope_type_rec(&mut self, t: Type, scope: TypeScope) {
        match t {
            Type::Any | Type::Unit | Type::Boolean | Type::Integer | Type::Float |
            Type::String => {}
            Type::Array(arr) => {
                self.add_scope_rec(self.array(arr), scope);
            }
            Type::Object(obj) => {
                for (_, member_t) in self.object(obj).0.clone() {
                    self.add_scope_rec(member_t, scope);
                }
            }
            Type::ConcreteObject(obj) => {
                for (_, member_t) in self.concrete_object(obj).clone() {
                    self.add_scope_rec(member_t, scope);
                }
            }
            Type::Closure(clo) => {
                let (param_types, return_t, capture_types) = self.closure(clo).clone();
                for param_t in param_types {
                    self.add_scope_rec(param_t, scope);
                }
                self.add_scope_rec(return_t, scope);
                if let Some(capture_types) = capture_types {
                    for (_, capture_t) in capture_types {
                        self.add_scope_rec(capture_t, scope);
                    }
                }
            }
            Type::Variants(var) => {
                for (_, var_t) in self.variants(var).0.clone() {
                    self.add_scope_rec(var_t, scope);
                }
            }
        }
    }
    pub fn group(
        &self, group: TypeGroup
    ) -> impl Iterator<Item = Type> + '_ {
        let internal_idx = self.groups[group.0];
        return self.internal_groups[internal_idx].0.iter().map(|t| *t);
    }
    pub fn group_scopes(&self, group: TypeGroup) -> impl Iterator<Item = TypeScope> + '_ {
        let internal_idx = self.groups[group.0];
        return self.internal_groups[internal_idx].1.iter().map(|s| TypeScope(*s));
    }
    pub fn group_concrete(
        &self, group: TypeGroup
    ) -> Type {
        let t = self.group(group).next().expect("was assumed to be concrete!");
        if let Type::Any = t { Type::Unit } else { t }
    }
    pub fn group_internal_id(&self, group: TypeGroup) -> usize {
        return self.groups[group.0];
    }
    pub fn set_group_types(
        &mut self, group: TypeGroup, new_types: &[Type]
    ) {
        let internal_id = self.group_internal_id(group);
        self.internal_groups[internal_id].0 = new_types.iter()
            .map(|t| *t).collect()
    }

    pub fn try_merge_groups(
        &mut self,
        a: TypeGroup, b: TypeGroup
    ) -> bool {
        let mut merged_groups = HashSet::new();
        if !self.try_merge_groups_internal(a, true, b, true, &mut Vec::new(), &mut merged_groups) {
            return false;
        }
        for (group_a, group_b) in merged_groups {
            let a_internal = self.group_internal_id(group_a);
            let b_internal = self.group_internal_id(group_b);
            if a_internal != b_internal {
                for internal_group_idx in &mut self.groups {
                    if *internal_group_idx == b_internal {
                        *internal_group_idx = a_internal;
                    }
                }
            }
        }
        true
    }

    fn try_merge_groups_internal(
        &mut self,
        a: TypeGroup,
        a_mut: bool,
        b: TypeGroup,
        b_mut: bool,
        encountered: &mut Vec<usize>,
        merged: &mut HashSet<(TypeGroup, TypeGroup)>
    ) -> bool {
        let a_internal = self.group_internal_id(a);
        let b_internal = self.group_internal_id(b);
        if encountered.contains(&a_internal)
        && encountered.contains(&b_internal) {
            return true
        }
        let mergable = self.internal_groups[a_internal].1.iter()
            .find(|s| self.internal_groups[b_internal].1.contains(*s))
            .is_some()
            || self.internal_groups[b_internal].1.iter()
            .find(|s| self.internal_groups[a_internal].1.contains(*s))
            .is_some();
        let a_mut = a_mut && mergable;
        let b_mut = b_mut && mergable;
        encountered.push(a_internal);
        encountered.push(b_internal);
        let mut merged_types = HashSet::new();
        let a_types = self.group(a).collect::<Vec<Type>>();
        let b_types = self.group(b).collect::<Vec<Type>>();
        for a_type in &a_types {
            for b_type in &b_types {
                if let Some(r_type) = self.try_merge_types_internal(
                    *a_type, a_mut, *b_type, b_mut, encountered, merged
                ) {
                    merged_types.insert(r_type);
                }
            }
        }
        if merged_types.is_empty() { return false; }
        if a_mut {
            self.internal_groups[a_internal].0 = merged_types.clone();
        }
        if b_mut {
            self.internal_groups[b_internal].0 = merged_types;
        }
        if a_mut && b_mut {
            merged.insert((a, b));
        }
        encountered.pop();
        encountered.pop();
        true
    }

    fn try_merge_types_internal(
        &mut self,
        a: Type, a_mut: bool, b: Type, b_mut: bool,
        encountered: &mut Vec<usize>,
        merged: &mut HashSet<(TypeGroup, TypeGroup)>
    ) -> Option<Type> {
        match (a, b) {
            (Type::Any, b) => Some(b),
            (a, Type::Any) => Some(a),
            (Type::ConcreteObject(obj_a), b) => {
                let obj_type = Type::Object(self.insert_object(
                    self.concrete_object(obj_a).iter().map(|e| *e).collect(),
                    false
                ));
                self.try_merge_types_internal(obj_type, a_mut, b, b_mut, encountered, merged)
            }
            (a, Type::ConcreteObject(obj_b)) => {
                let obj_type = Type::Object(self.insert_object(
                    self.concrete_object(obj_b).iter().map(|e| *e).collect(),
                    false
                ));
                self.try_merge_types_internal(a, a_mut, obj_type, b_mut, encountered, merged)
            }
            (Type::Array(arr_a), Type::Array(arr_b)) => {
                if self.try_merge_groups_internal(
                    self.array(arr_a), a_mut,
                    self.array(arr_b), b_mut,
                    encountered, merged
                ) { Some(a) } else { None }
            }
            (Type::Object(obj_a), Type::Object(obj_b)) => {
                let (members_a, fixed_a) = self.object(obj_a).clone();
                let (members_b, fixed_b) = self.object(obj_b).clone();
                let member_names = members_a.keys().chain(members_b.keys())
                    .map(|n| *n).collect::<HashSet<StringIdx>>();
                let mut new_members = HashMap::new();
                for member_name in member_names {
                    match (
                        members_a.get(&member_name),
                        members_b.get(&member_name)
                    ) {
                        (Some(member_type_a), Some(member_type_b)) => {
                            if self.try_merge_groups_internal(
                                *member_type_a, a_mut,
                                *member_type_b, b_mut,
                                encountered, merged
                            ) {
                                new_members.insert(member_name, *member_type_a);
                            } else { return None }
                        }
                        (Some(member_type_a), None) => {
                            if !fixed_b {
                                new_members.insert(member_name, *member_type_a);
                            } else { return None }
                        }
                        (None, Some(member_type_b)) => {
                            if !fixed_a {
                                new_members.insert(member_name, *member_type_b);
                            } else { return None }
                        }
                        (None, None) => panic!("Impossible!")
                    }
                }
                Some(Type::Object(self.insert_object(
                    new_members, fixed_a || fixed_b
                )))
            }
            (Type::Closure(clo_a), Type::Closure(clo_b)) => {
                let (params_a, return_a, captures_a) = self.closure(clo_a).clone();
                let (params_b, return_b, captures_b) = self.closure(clo_b).clone();
                if params_a.len() != params_b.len() { return None }
                for p in 0..params_a.len() {
                    if !self.try_merge_groups_internal(
                        params_a[p], a_mut,
                        params_b[p], b_mut,
                        encountered, merged
                    ) { return None; }
                }
                if !self.try_merge_groups_internal(
                    return_a, a_mut, return_b, b_mut, encountered, merged
                ) { return None; }
                Some(Type::Closure(self.insert_closure(
                    params_a.clone(),
                    return_a,
                    if captures_a.is_some() { captures_a.clone() }
                        else { captures_b.clone() }
                )))
            }
            (Type::Variants(var_a), Type::Variants(var_b)) => {
                let (variants_a, fixed_a) = self.variants(var_a).clone();
                let (variants_b, fixed_b) = self.variants(var_b).clone();
                let variant_names = variants_a.keys().chain(variants_b.keys())
                    .map(|n| *n).collect::<HashSet<StringIdx>>();
                let mut new_variants = HashMap::new();
                for variant_name in variant_names {
                    match (
                        variants_a.get(&variant_name),
                        variants_b.get(&variant_name)
                    ) {
                        (Some(variant_type_a), Some(variant_type_b)) => {
                            if self.try_merge_groups_internal(
                                *variant_type_a, a_mut, *variant_type_b, b_mut,
                                encountered, merged
                            ) {
                                new_variants.insert(variant_name, *variant_type_a);
                            } else { return None }
                        }
                        (Some(variant_type_a), None) => {
                            if !fixed_b {
                                new_variants.insert(
                                    variant_name, *variant_type_a
                                );
                            } else { return None }
                        }
                        (None, Some(variant_type_b)) => {
                            if !fixed_a {
                                new_variants.insert(
                                    variant_name, *variant_type_b
                                );
                            } else { return None }
                        }
                        (None, None) => panic!("Impossible!")
                    }
                }
                Some(Type::Variants(self.insert_variants(
                    new_variants, fixed_a || fixed_b
                )))
            }
            _ => if std::mem::discriminant(&a) == std::mem::discriminant(&b) {
                Some(a)
            } else { None }
        }
    }

    // pub fn transfer_group(
    //     &self, group: TypeGroup, dest: &mut TypeMap
    // ) -> TypeGroup {       
    //     self.transfer_group_internal(group, dest, &mut HashMap::new())
    // }

    // fn transfer_group_internal(
    //     &self, group: TypeGroup, dest: &mut TypeMap,
    //     encountered: &mut HashMap<usize, TypeGroup>
    // ) -> TypeGroup {
    //     let internal_idx = self.group_internal_id(group);
    //     if let Some(transferred_group) = encountered.get(&internal_idx) {
    //         return *transferred_group;
    //     }
    //     let transferred_group = dest.insert_group(&[]);
    //     encountered.insert(internal_idx, transferred_group);
    //     let transferred_types = self.group(group)
    //         .collect::<Vec<Type>>().into_iter()
    //         .map(|t| self.transfer_type_internal(t, dest, encountered))
    //         .collect::<Vec<Type>>(); 
    //     dest.set_group_types(transferred_group, &transferred_types);
    //     transferred_group
    // }

    // fn transfer_type_internal(
    //     &self, t: Type, dest: &mut TypeMap, encountered: &mut HashMap<usize, TypeGroup>
    // ) -> Type {
    //     match t {
    //         Type::Array(arr) => {
    //             let t = self.transfer_group_internal(
    //                 self.array(arr), dest, encountered
    //             );
    //             Type::Array(dest.insert_array(t))
    //         },
    //         Type::Object(obj) => {
    //             let (old_members, fixed) = self.object(obj).clone();
    //             let new_members = old_members.into_iter().map(|(mn, mt)| (
    //                 mn,
    //                 self.transfer_group_internal(
    //                     mt, dest, encountered
    //                 )
    //             )).collect();
    //             Type::Object(dest.insert_object(new_members, fixed))
    //         }
    //         Type::ConcreteObject(obj) => {
    //             let old_members = self.concrete_object(obj).clone();
    //             let new_members = old_members.into_iter().map(|(mn, mt)| (
    //                 mn,
    //                 self.transfer_group_internal(
    //                     mt, dest, encountered
    //                 )
    //             )).collect();
    //             Type::ConcreteObject(dest.insert_concrete_object(new_members))
    //         }
    //         Type::Closure(clo) => {
    //             let (old_param_types, old_return_type, old_captures) = self.closure(clo).clone();
    //             let new_param_types = old_param_types.into_iter().map(|t| self.transfer_group_internal(
    //                 t, dest, encountered
    //             )).collect();
    //             let new_return_type = self.transfer_group_internal(old_return_type, dest, encountered);
    //             let new_captures = old_captures.map(|c| c.into_iter().map(|(cn, ct)| (
    //                 cn,
    //                 self.transfer_group_internal(ct, dest, encountered)
    //             )).collect());
    //             Type::Closure(dest.insert_closure(
    //                 new_param_types, 
    //                 new_return_type, 
    //                 new_captures
    //             ))
    //         }
    //         Type::Variants(var) => {
    //             let (old_variants, fixed) = self.variants(var).clone();
    //             let new_variants = old_variants.into_iter().map(|(vn, vt)| (
    //                 vn,
    //                 self.transfer_group_internal(
    //                     vt, dest, encountered
    //                 )
    //             )).collect();
    //             Type::Variants(dest.insert_variants(
    //                 new_variants,
    //                 fixed
    //             ))
    //         }
    //         _ => t
    //     }
    // }

    pub fn groups_eq(
        &self, a: TypeGroup, b: TypeGroup
    ) -> bool {
        TypeMap::internal_groups_eq(a, self, b, self, &mut HashSet::new())
    }

    // pub fn sep_groups_eq(
    //     &self, a: TypeGroup, other_scope: &TypeMap, b: TypeGroup
    // ) -> bool {
    //     TypeMap::internal_groups_eq(a, self, b, other_scope, &mut HashSet::new())
    // }

    fn internal_groups_eq(
        a: TypeGroup, a_scope: &TypeMap, b: TypeGroup, b_scope: &TypeMap,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        let a_internal = a_scope.group_internal_id(a);
        let b_internal = b_scope.group_internal_id(b);
        if a_internal == b_internal { return true; }
        let internal = (a_internal, b_internal);
        if encountered.contains(&internal) { return true; }
        encountered.insert(internal);
        let mut result = true;
        for group_a_t in a_scope.group(a).into_iter() {
            let mut found = false;
            for group_b_t in b_scope.group(b).into_iter() {
                if !TypeMap::internal_types_eq(
                    group_a_t, a_scope, group_b_t, b_scope, encountered
                ) { continue; }
                found = true;
                break;
            }
            if !found {
                result = false;
                break;
            }
        }
        encountered.remove(&internal);
        return result;
    }

    fn internal_types_eq(
        a: Type, a_scope: &TypeMap, b: Type, b_scope: &TypeMap,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        match (a, b) {
            (Type::Array(arr_a), Type::Array(arr_b)) => {
                if arr_a.get_internal_id() == arr_b.get_internal_id() { return true; }
                TypeMap::internal_arrays_eq(
                    a_scope.array(arr_a), a_scope,
                    b_scope.array(arr_b), b_scope,
                    encountered
                )
            }
            (Type::Object(obj_a), Type::Object(obj_b)) => {
                if obj_a.get_internal_id() == obj_b.get_internal_id() { return true; }
                TypeMap::internal_objects_eq(
                    a_scope.object(obj_a), a_scope,
                    b_scope.object(obj_b), b_scope,
                    encountered
                )
            }
            (Type::ConcreteObject(obj_a), Type::ConcreteObject(obj_b)) => {
                if obj_a.get_internal_id() == obj_b.get_internal_id() { return true; }
                TypeMap::internal_concrete_objects_eq(
                    a_scope.concrete_object(obj_a), a_scope,
                    b_scope.concrete_object(obj_b), b_scope,
                    encountered
                )
            }
            (Type::Closure(clo_a), Type::Closure(clo_b)) => {
                if clo_a.get_internal_id() == clo_b.get_internal_id() { return true; }
                TypeMap::internal_closures_eq(
                    a_scope.closure(clo_a), a_scope,
                    b_scope.closure(clo_b), b_scope,
                    encountered
                )
            }
            (Type::Variants(var_a), Type::Variants(var_b)) => {
                if var_a.get_internal_id() == var_b.get_internal_id() { return true; }
                TypeMap::internal_variants_eq(
                    a_scope.variants(var_a), a_scope,
                    b_scope.variants(var_b), b_scope,
                    encountered
                )
            }
            (a, b) => {
                std::mem::discriminant(&a) == std::mem::discriminant(&b)
            }
        }
    }

    fn internal_arrays_eq(
        a: TypeGroup, a_scope: &TypeMap, b: TypeGroup, b_scope: &TypeMap,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        TypeMap::internal_groups_eq(a, a_scope, b, b_scope, encountered)
    }

    fn internal_objects_eq(
        a: &(HashMap<StringIdx, TypeGroup>, bool), a_scope: &TypeMap,
        b: &(HashMap<StringIdx, TypeGroup>, bool), b_scope: &TypeMap,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        let a = &a.0;
        let b = &b.0;
        for member in a.keys() {
            if !b.contains_key(member) { return false; }
            if !TypeMap::internal_groups_eq(
                *a.get(member).expect("key from above"), a_scope,
                *b.get(member).expect("checked above"), b_scope,
                encountered
            ) { return false; }
        }
        for member in b.keys() {
            if !a.contains_key(member) { return false; }
            if !TypeMap::internal_groups_eq(
                *a.get(member).expect("checked above"), a_scope,
                *b.get(member).expect("key from above"), b_scope,
                encountered
            ) { return false; }
        }
        return true;
    }

    fn internal_concrete_objects_eq(
        a: &Vec<(StringIdx, TypeGroup)>, a_scope: &TypeMap,
        b: &Vec<(StringIdx, TypeGroup)>, b_scope: &TypeMap,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        if a.len() != b.len() { return false; }
        for member_idx in 0..a.len() {
            if !TypeMap::internal_groups_eq(
                a[member_idx].1, a_scope,
                b[member_idx].1, b_scope,
                encountered
            ) { return false; }
        }
        return true;
    }

    fn internal_closures_eq(
        a: &(Vec<TypeGroup>, TypeGroup, Option<HashMap<StringIdx, TypeGroup>>), a_scope: &TypeMap,
        b: &(Vec<TypeGroup>, TypeGroup, Option<HashMap<StringIdx, TypeGroup>>), b_scope: &TypeMap,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        let (a_params, a_return, _) = a;
        let (b_params, b_return, _) = b;
        if a_params.len() != b_params.len() { return false; }
        for p in 0..a_params.len() {
            if !TypeMap::internal_groups_eq(
                a_params[p], a_scope, b_params[p], b_scope, encountered
            ) { return false; }
        }
        return TypeMap::internal_groups_eq(
            *a_return, a_scope, *b_return, b_scope, encountered
        );
    }

    fn internal_variants_eq(
        a: &(HashMap<StringIdx, TypeGroup>, bool), a_scope: &TypeMap,
        b: &(HashMap<StringIdx, TypeGroup>, bool), b_scope: &TypeMap,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        let a = &a.0;
        let b = &b.0;
        for variant in a.keys() {
            if !b.contains_key(variant) { return false; }
            if !TypeMap::internal_groups_eq(
                *a.get(variant).expect("key from above"), a_scope,
                *b.get(variant).expect("checked above"), b_scope,
                encountered
            ) { return false; }
        }
        for variant in b.keys() {
            if !a.contains_key(variant) { return false; }
            if !TypeMap::internal_groups_eq(
                *a.get(variant).expect("checked above"), a_scope,
                *b.get(variant).expect("key from above"), b_scope,
                encountered
            ) { return false; }
        }
        return true;
    }

    pub fn duplicate_group(
        &mut self, group: TypeGroup, scope: TypeScope
    ) -> TypeGroup {       
        self.duplicate_group_internal(group, scope, &mut HashMap::new())
    }

    fn duplicate_group_internal(
        &mut self, group: TypeGroup, scope: TypeScope,
        encountered: &mut HashMap<usize, TypeGroup>
    ) -> TypeGroup {
        let internal_idx = self.group_internal_id(group);
        if let Some(transferred_group) = encountered.get(&internal_idx) {
            return *transferred_group;
        }
        let transferred_group = self.insert_group(&[], scope);
        encountered.insert(internal_idx, transferred_group);
        let transferred_types = self.group(group)
            .collect::<Vec<Type>>().into_iter()
            .map(|t| self.duplicate_type_internal(t, scope, encountered))
            .collect::<Vec<Type>>(); 
        self.set_group_types(transferred_group, &transferred_types);
        transferred_group
    }

    fn duplicate_type_internal(
        &mut self, t: Type, scope: TypeScope, encountered: &mut HashMap<usize, TypeGroup>
    ) -> Type {
        match t {
            Type::Array(arr) => {
                let t = self.duplicate_group_internal(
                    self.array(arr), scope, encountered
                );
                Type::Array(self.insert_array(t))
            },
            Type::Object(obj) => {
                let (old_members, fixed) = self.object(obj).clone();
                let new_members = old_members.into_iter().map(|(mn, mt)| (
                    mn,
                    self.duplicate_group_internal(
                        mt, scope, encountered
                    )
                )).collect();
                Type::Object(self.insert_object(new_members, fixed))
            }
            Type::ConcreteObject(obj) => {
                let old_members = self.concrete_object(obj).clone();
                let new_members = old_members.into_iter().map(|(mn, mt)| (
                    mn,
                    self.duplicate_group_internal(
                        mt, scope, encountered
                    )
                )).collect();
                Type::ConcreteObject(self.insert_concrete_object(new_members))
            }
            Type::Closure(clo) => {
                let (old_param_types, old_return_type, old_captures) = self.closure(clo).clone();
                let new_param_types = old_param_types.into_iter().map(|t| self.duplicate_group_internal(
                    t, scope, encountered
                )).collect();
                let new_return_type = self.duplicate_group_internal(old_return_type, scope, encountered);
                let new_captures = old_captures.map(|c| c.into_iter().map(|(cn, ct)| (
                    cn,
                    self.duplicate_group_internal(ct, scope, encountered)
                )).collect());
                Type::Closure(self.insert_closure(
                    new_param_types, 
                    new_return_type, 
                    new_captures
                ))
            }
            Type::Variants(var) => {
                let (old_variants, fixed) = self.variants(var).clone();
                let new_variants = old_variants.into_iter().map(|(vn, vt)| (
                    vn,
                    self.duplicate_group_internal(
                        vt, scope, encountered
                    )
                )).collect();
                Type::Variants(self.insert_variants(
                    new_variants,
                    fixed
                ))
            }
            _ => t
        }
    }

    fn deduplicated(&self) -> TypeMap {
        let mut new = TypeMap::new();
        // deduplicate arrays
        let mut mapped_arrays = HashMap::new();
        for og_array_idx in 0..self.arrays.len() {
            let mut found = false;
            for new_array_idx in 0..new.arrays.len() {
                if !TypeMap::internal_arrays_eq(
                    self.arrays[og_array_idx], self,
                    new.arrays[new_array_idx], self,
                    &mut HashSet::new()
                ) { continue; }
                mapped_arrays.insert(og_array_idx, new_array_idx);
                found = true;
                break;
            }
            if found { continue; }
            let new_array_idx = new.arrays.len();
            mapped_arrays.insert(og_array_idx, new_array_idx);
            new.arrays.push(self.arrays[og_array_idx]);
        }
        // deduplicate objects
        let mut mapped_objects = HashMap::new();
        for og_object_idx in 0..self.objects.len() {
            let mut found = false;
            for new_object_idx in 0..new.objects.len() {
                if !TypeMap::internal_objects_eq(
                    &self.objects[og_object_idx], self,
                    &new.objects[new_object_idx], self,
                    &mut HashSet::new()
                ) { continue; }
                mapped_objects.insert(og_object_idx, new_object_idx);
                found = true;
                break;
            }
            if found { continue; }
            let new_object_idx = new.objects.len();
            mapped_objects.insert(og_object_idx, new_object_idx);
            new.objects.push(self.objects[og_object_idx].clone());
        }
        // deduplicate concrete objects
        let mut mapped_concrete_objects = HashMap::new();
        for og_concrete_object_idx in 0..self.concrete_objects.len() {
            let mut found = false;
            for new_concrete_object_idx in 0..new.concrete_objects.len() {
                if !TypeMap::internal_concrete_objects_eq(
                    &self.concrete_objects[og_concrete_object_idx], self,
                    &new.concrete_objects[new_concrete_object_idx], self,
                    &mut HashSet::new()
                ) { continue; }
                mapped_concrete_objects.insert(og_concrete_object_idx, new_concrete_object_idx);
                found = true;
                break;
            }
            if found { continue; }
            let new_concrete_object_idx = new.concrete_objects.len();
            mapped_concrete_objects.insert(og_concrete_object_idx, new_concrete_object_idx);
            new.concrete_objects.push(self.concrete_objects[og_concrete_object_idx].clone());
        }
        // deduplicate closures
        let mut mapped_closures = HashMap::new();
        for og_closure_idx in 0..self.closures.len() {
            let mut found = false;
            for new_closure_idx in 0..new.closures.len() {
                if !TypeMap::internal_closures_eq(
                    &self.closures[og_closure_idx], self,
                    &new.closures[new_closure_idx], self,
                    &mut HashSet::new()
                ) { continue; }
                mapped_closures.insert(og_closure_idx, new_closure_idx);
                found = true;
                break;
            }
            if found { continue; }
            let new_closure_idx = new.closures.len();
            mapped_closures.insert(og_closure_idx, new_closure_idx);
            new.closures.push(self.closures[og_closure_idx].clone());
        }
        // deduplicate variants
        let mut mapped_variants = HashMap::new();
        for og_variants_idx in 0..self.variants.len() {
            let mut found = false;
            for new_variants_idx in 0..new.variants.len() {
                if !TypeMap::internal_variants_eq(
                    &self.variants[og_variants_idx], self,
                    &new.variants[new_variants_idx], self,
                    &mut HashSet::new()
                ) { continue; }
                mapped_variants.insert(og_variants_idx, new_variants_idx);
                found = true;
                break;
            }
            if found { continue; }
            let new_variants_idx = new.variants.len();
            mapped_variants.insert(og_variants_idx, new_variants_idx);
            new.variants.push(self.variants[og_variants_idx].clone());
        }
        // map type groups
        fn apply_mappings_type(
            t: Type,
            arrays: &HashMap<usize, usize>, objects: &HashMap<usize, usize>, 
            concrete_objects: &HashMap<usize, usize>, closures: &HashMap<usize, usize>, 
            variants: &HashMap<usize, usize>
        ) -> Type { match t {
            Type::Any | Type::Unit | Type::Boolean | Type::Integer | Type::Float |
            Type::String => t,
            Type::Array(i) => {
                Type::Array(ArrayType(*arrays.get(&i.0).unwrap_or(&i.0)))
            }
            Type::Object(i) => {
                Type::Object(ObjectType(*objects.get(&i.0).unwrap_or(&i.0)))
            }
            Type::ConcreteObject(i) => {
                Type::ConcreteObject(ConcreteObjectType(
                    *concrete_objects.get(&i.0).unwrap_or(&i.0)
                ))
            }
            Type::Closure(i) => {
                Type::Closure(ClosureType(*closures.get(&i.0).unwrap_or(&i.0)))
            }
            Type::Variants(i) => {
                Type::Variants(VariantsType(*variants.get(&i.0).unwrap_or(&i.0)))
            }
        } }
        new.groups = self.groups.clone();
        new.internal_groups = self.internal_groups.iter()
            .map(|(types, scopes)| (
                types.iter().map(|t|
                    apply_mappings_type(
                        *t, &mapped_arrays, &mapped_objects, &mapped_concrete_objects,
                        &mapped_closures, &mapped_variants
                    )
                ).collect(),
                scopes.clone()
            ))
            .collect();
        // done
        return new;
    }

    pub fn deduplicate(&mut self) {
        *self = self.deduplicated();
    }

    pub fn replace_any_with_unit(&mut self) {
        for (group, _) in &mut self.internal_groups {
            *group = group.iter().map(|t|
                if let Type::Any = *t { Type::Unit } else { *t }
            ).collect();
        }
    }
}
