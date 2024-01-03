use std::collections::{HashMap, HashSet};

use crate::util::strings::StringIdx;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeGroup(usize);
pub trait IntoTypeGroupIdx { fn get_group_index(&self) -> usize; }
impl IntoTypeGroupIdx for TypeGroup { fn get_group_index(&self) -> usize { self.0 } }
impl IntoTypeGroupIdx for &TypeGroup { fn get_group_index(&self) -> usize { self.0 } }

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ArrayType(usize);
impl ArrayType { pub fn get_internal_id(&self) -> usize { self.0 } }
pub trait IntoArrayTypeIdx { fn get_array_type_index(&self) -> usize; }
impl IntoArrayTypeIdx for ArrayType { fn get_array_type_index(&self) -> usize { self.0 } }
impl IntoArrayTypeIdx for &ArrayType { fn get_array_type_index(&self) -> usize { self.0 } }

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ObjectType(usize);
impl ObjectType { pub fn get_internal_id(&self) -> usize { self.0 } }
pub trait IntoObjectTypeIdx { fn get_object_type_index(&self) -> usize; }
impl IntoObjectTypeIdx for ObjectType { fn get_object_type_index(&self) -> usize { self.0 } }
impl IntoObjectTypeIdx for &ObjectType { fn get_object_type_index(&self) -> usize { self.0 } }

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ConcreteObjectType(usize);
impl ConcreteObjectType { pub fn get_internal_id(&self) -> usize { self.0 } }
pub trait IntoConcreteObjectTypeIdx { fn get_concrete_object_type_index(&self) -> usize; }
impl IntoConcreteObjectTypeIdx for ConcreteObjectType {
    fn get_concrete_object_type_index(&self) -> usize { self.0 }
}
impl IntoConcreteObjectTypeIdx for &ConcreteObjectType {
    fn get_concrete_object_type_index(&self) -> usize { self.0 }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ClosureType(usize);
impl ClosureType { pub fn get_internal_id(&self) -> usize { self.0 } }
pub trait IntoClosureTypeIdx { fn get_closure_type_index(&self) -> usize; }
impl IntoClosureTypeIdx for ClosureType { fn get_closure_type_index(&self) -> usize { self.0 } }
impl IntoClosureTypeIdx for &ClosureType { fn get_closure_type_index(&self) -> usize { self.0 } }

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct VariantsType(usize);
impl VariantsType { pub fn get_internal_id(&self) -> usize { self.0 } }
pub trait IntoVariantsTypeIdx { fn get_variants_type_index(&self) -> usize; }
impl IntoVariantsTypeIdx for VariantsType { fn get_variants_type_index(&self) -> usize { self.0 } }
impl IntoVariantsTypeIdx for &VariantsType { fn get_variants_type_index(&self) -> usize { self.0 } }

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
pub struct TypeScope {
    groups: Vec<usize>,
    internal_groups: Vec<HashSet<Type>>,
    arrays: Vec<TypeGroup>,
    objects: Vec<(HashMap<StringIdx, TypeGroup>, bool)>,
    concrete_objects: Vec<Vec<(StringIdx, TypeGroup)>>,
    closures: Vec<(
        Vec<TypeGroup>, TypeGroup, Option<HashMap<StringIdx, TypeGroup>>
    )>,
    variants: Vec<(HashMap<StringIdx, TypeGroup>, bool)>
}

impl TypeScope {
    pub fn new() -> TypeScope {
        TypeScope {
            groups: Vec::new(),
            internal_groups: Vec::new(),
            arrays: Vec::new(),
            objects: Vec::new(),
            concrete_objects: Vec::new(),
            closures: Vec::new(),
            variants: Vec::new()
        }
    }

    pub fn internal_arrays(&self) -> &Vec<TypeGroup> { &self.arrays }
    pub fn insert_array(
        &mut self, element_type: TypeGroup
    ) -> ArrayType {
        let idx = self.arrays.len();
        self.arrays.push(element_type);
        return ArrayType(idx);
    }
    pub fn array(&self, array: impl IntoArrayTypeIdx) -> TypeGroup {
        self.arrays[array.get_array_type_index()]
    }

    pub fn internal_objects(&self)
        -> &Vec<(HashMap<StringIdx, TypeGroup>, bool)> { &self.objects }
    pub fn insert_object(
        &mut self, member_types: HashMap<StringIdx, TypeGroup>, fixed: bool
    ) -> ObjectType {
        let idx = self.objects.len();
        self.objects.push((member_types, fixed));
        return ObjectType(idx);
    }
    pub fn object(
        &self, object: impl IntoObjectTypeIdx
    ) -> &(HashMap<StringIdx, TypeGroup>, bool) {
        &self.objects[object.get_object_type_index()]
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
    pub fn concrete_object(
        &self, concrete_object: impl IntoConcreteObjectTypeIdx
    ) -> &Vec<(StringIdx, TypeGroup)> {
        &self.concrete_objects[concrete_object.get_concrete_object_type_index()]
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
    pub fn closure(
        &self, closure: impl IntoClosureTypeIdx
    ) -> &(Vec<TypeGroup>, TypeGroup, Option<HashMap<StringIdx, TypeGroup>>) {
        &self.closures[closure.get_closure_type_index()]
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
    pub fn variants(
        &self, variants: impl IntoVariantsTypeIdx
    ) -> &(HashMap<StringIdx, TypeGroup>, bool) {
        &self.variants[variants.get_variants_type_index()]
    }

    pub fn internal_groups(&self)
        -> &Vec<HashSet<Type>> { &self.internal_groups }
    pub fn insert_group(&mut self, types: &[Type]) -> TypeGroup {
        let internal_idx = self.internal_groups.len();
        self.internal_groups.push(types.iter().map(|t| *t).collect());
        let group_idx = self.groups.len();
        self.groups.push(internal_idx);
        return TypeGroup(group_idx);
    }
    pub fn group(
        &self, group: impl IntoTypeGroupIdx
    ) -> impl Iterator<Item = Type> + '_ {
        let internal_idx = self.groups[group.get_group_index()];
        return self.internal_groups[internal_idx].iter().map(|t| *t);
    }
    pub fn group_concrete(
        &self, group: impl IntoTypeGroupIdx + Copy
    ) -> Type {
        let t = self.group(group).next().expect("was assumed to be concrete!");
        if let Type::Any = t { Type::Unit } else { t }
    }
    pub fn group_internal_id(&self, group: impl IntoTypeGroupIdx) -> usize {
        return self.groups[group.get_group_index()];
    }
    pub fn set_group_types(
        &mut self, group: impl IntoTypeGroupIdx, new_types: &[Type]
    ) {
        let internal_id = self.group_internal_id(group);
        self.internal_groups[internal_id] = new_types.iter()
            .map(|t| *t).collect()
    }

    pub fn try_merge_groups(
        &mut self,
        a: impl IntoTypeGroupIdx + Copy, b: impl IntoTypeGroupIdx + Copy
    ) -> bool {
        self.try_merge_groups_internal(a, b, &mut HashSet::new())
    }

    fn try_merge_groups_internal(
        &mut self,
        a: impl IntoTypeGroupIdx + Copy, b: impl IntoTypeGroupIdx + Copy,
        encountered: &mut HashSet<usize>
    ) -> bool {
        let a_internal = self.group_internal_id(a);
        let b_internal = self.group_internal_id(b);
        if encountered.contains(&a_internal)
            && encountered.contains(&b_internal
        ) {
            return true
        }
        encountered.insert(a_internal);
        encountered.insert(b_internal);
        let mut merged_types = HashSet::new();
        for a_type in self.group(a).collect::<Vec<Type>>() {
            for b_type in self.group(b).collect::<Vec<Type>>() {
                if let Some(r_type) = self.try_merge_types_internal(
                    a_type, b_type, encountered
                ) {
                    merged_types.insert(r_type);
                }
            }
        }
        if merged_types.is_empty() { return false; }
        self.internal_groups[a_internal] = merged_types;
        if a_internal != b_internal {
            for internal_group_idx in &mut self.groups {
                if *internal_group_idx == b_internal {
                    *internal_group_idx = a_internal;
                }
            }
        }
        encountered.remove(&a_internal);
        encountered.remove(&b_internal);
        true
    }

    fn try_merge_types_internal(
        &mut self,
        a: Type, b: Type,
        encountered: &mut HashSet<usize>
    ) -> Option<Type> {
        match (a, b) {
            (Type::Any, b) => Some(b),
            (a, Type::Any) => Some(a),
            (Type::ConcreteObject(obj_a), b) => {
                let obj_type = Type::Object(self.insert_object(
                    self.concrete_object(obj_a).iter().map(|e| *e).collect(),
                    false
                ));
                self.try_merge_types_internal(obj_type, b, encountered)
            }
            (a, Type::ConcreteObject(obj_b)) => {
                let obj_type = Type::Object(self.insert_object(
                    self.concrete_object(obj_b).iter().map(|e| *e).collect(),
                    false
                ));
                self.try_merge_types_internal(a, obj_type, encountered)
            }
            (Type::Array(arr_a), Type::Array(arr_b)) => {
                if self.try_merge_groups_internal(
                    self.array(arr_a),
                    self.array(arr_b),
                    encountered
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
                                member_type_a, member_type_b,
                                encountered
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
                        params_a[p], params_b[p], encountered
                    ) { return None; }
                }
                if !self.try_merge_groups_internal(
                    return_a, return_b, encountered
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
                                variant_type_a, variant_type_b,
                                encountered
                            ) {
                                new_variants.insert(
                                    variant_name, *variant_type_a
                                );
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

    pub fn duplicate_group(
        &mut self, group: impl IntoTypeGroupIdx + Copy
    ) -> TypeGroup {
        self.duplicate_group_internal(group, &mut HashMap::new())
    }

    fn duplicate_group_internal(
        &mut self, group: impl IntoTypeGroupIdx + Copy,
        encountered: &mut HashMap<usize, TypeGroup>
    ) -> TypeGroup {
        let internal_idx = self.group_internal_id(group);
        if let Some(dup_group) = encountered.get(&internal_idx) {
            return *dup_group;
        }
        let dup_group = self.insert_group(&[]);
        encountered.insert(internal_idx, dup_group);
        let mut dup_types = HashSet::new();
        for dup_type in self.group(group).collect::<Vec<Type>>() {
            dup_types.insert(
                self.duplicate_type_internal(dup_type, encountered)
            );
        }
        let dup_internal_idx = self.group_internal_id(dup_group);
        self.internal_groups[dup_internal_idx] = dup_types;      
        dup_group
    }

    fn duplicate_type_internal(
        &mut self, t: Type, encountered: &mut HashMap<usize, TypeGroup>
    ) -> Type {
        match t {
            Type::Array(arr) => {
                let t = self.duplicate_group_internal(
                    self.array(arr), encountered
                );
                Type::Array(self.insert_array(t))
            },
            Type::Object(obj) => {
                let (old_members, fixed) = self.object(obj).clone();
                let new_members = old_members.into_iter().map(|(mn, mt)| (
                    mn,
                    self.duplicate_group_internal(
                        mt, encountered
                    )
                )).collect();
                Type::Object(self.insert_object(new_members, fixed))
            }
            Type::ConcreteObject(obj) => {
                let old_members = self.concrete_object(obj).clone();
                let new_members = old_members.into_iter().map(|(mn, mt)| (
                    mn,
                    self.duplicate_group_internal(
                        mt, encountered
                    )
                )).collect();
                Type::ConcreteObject(self.insert_concrete_object(new_members))
            }
            Type::Closure(clo) => {
                let (old_param_types, old_return_type, old_captures) = self.closure(clo).clone();
                let new_param_types = old_param_types.into_iter().map(|t| self.duplicate_group_internal(
                    t, encountered
                )).collect();
                let new_return_type = self.duplicate_group_internal(old_return_type, encountered);
                let new_captures = old_captures.map(|c| c.into_iter().map(|(cn, ct)| (
                    cn,
                    self.duplicate_group_internal(ct, encountered)
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
                        vt, encountered
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

    pub fn groups_eq(
        &self, a: impl IntoTypeGroupIdx + Copy, b: impl IntoTypeGroupIdx + Copy
    ) -> bool {
        self.internal_groups_eq(a, b, &mut HashSet::new())
    }

    fn internal_groups_eq(
        &self, a: impl IntoTypeGroupIdx + Copy, b: impl IntoTypeGroupIdx + Copy,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        let a_internal = self.group_internal_id(a);
        let b_internal = self.group_internal_id(b);
        if a_internal == b_internal { return true; }
        let internal = (a_internal, b_internal);
        if encountered.contains(&internal) { return true; }
        encountered.insert(internal);
        let result = self.group(a).into_iter().zip(self.group(b).into_iter())
            .map(|(ta, tb)| self.internal_types_eq(ta, tb, encountered))
            .reduce(|a, b| a && b)
            .unwrap_or(true);
        encountered.remove(&internal);
        return result;
    }

    fn internal_types_eq(
        &self, a: Type, b: Type, encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        match (a, b) {
            (Type::Array(arr_a), Type::Array(arr_b)) => {
                if arr_a.get_internal_id() == arr_b.get_internal_id() { return true; }
                self.internal_arrays_eq(arr_a, arr_b, encountered)
            }
            (Type::Object(obj_a), Type::Object(obj_b)) => {
                if obj_a.get_internal_id() == obj_b.get_internal_id() { return true; }
                self.internal_objects_eq(obj_a, obj_b, encountered)
            }
            (Type::ConcreteObject(obj_a), Type::ConcreteObject(obj_b)) => {
                if obj_a.get_internal_id() == obj_b.get_internal_id() { return true; }
                self.internal_concrete_objects_eq(obj_a, obj_b, encountered)
            }
            (Type::Closure(clo_a), Type::Closure(clo_b)) => {
                if clo_a.get_internal_id() == clo_b.get_internal_id() { return true; }
                self.internal_closures_eq(clo_a, clo_b, encountered)
            }
            (Type::Variants(var_a), Type::Variants(var_b)) => {
                if var_a.get_internal_id() == var_b.get_internal_id() { return true; }
                self.internal_variants_eq(var_a, var_b, encountered)
            }
            (a, b) => {
                std::mem::discriminant(&a) == std::mem::discriminant(&b)
            }
        }
    }

    fn internal_arrays_eq(
        &self, a: impl IntoArrayTypeIdx, b: impl IntoArrayTypeIdx,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        self.internal_groups_eq(self.array(a), self.array(b), encountered)
    }

    fn internal_objects_eq(
        &self, a: impl IntoObjectTypeIdx, b: impl IntoObjectTypeIdx,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        let a = &self.object(a).0;
        let b = &self.object(b).0;
        for member in a.keys() {
            if !b.contains_key(member) { return false; }
            if !self.internal_groups_eq(
                a.get(member).expect("key from above"), b.get(member).expect("checked above"),
                encountered
            ) { return false; }
        }
        for member in b.keys() {
            if !a.contains_key(member) { return false; }
            if !self.internal_groups_eq(
                a.get(member).expect("checked above"), b.get(member).expect("key from above"),
                encountered
            ) { return false; }
        }
        return true;
    }

    fn internal_concrete_objects_eq(
        &self, a: impl IntoConcreteObjectTypeIdx, b: impl IntoConcreteObjectTypeIdx,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        let a = &self.concrete_object(a).iter().map(|m| *m)
            .collect::<HashMap<StringIdx, TypeGroup>>();
        let b = &self.concrete_object(b).iter().map(|m| *m)
            .collect::<HashMap<StringIdx, TypeGroup>>();
        for member in a.keys() {
            if !b.contains_key(member) { return false; }
            if !self.internal_groups_eq(
                a.get(member).expect("key from above"), b.get(member).expect("checked above"),
                encountered
            ) { return false; }
        }
        for member in b.keys() {
            if !a.contains_key(member) { return false; }
            if !self.internal_groups_eq(
                a.get(member).expect("checked above"), b.get(member).expect("key from above"),
                encountered
            ) { return false; }
        }
        return true;
    }

    fn internal_closures_eq(
        &self, a: impl IntoClosureTypeIdx, b: impl IntoClosureTypeIdx,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        let (a_params, a_return, _) = self.closure(a);
        let (b_params, b_return, _) = self.closure(b);
        if a_params.len() != b_params.len() { return false; }
        for p in 0..a_params.len() {
            if !self.internal_groups_eq(a_params[p], b_params[p], encountered) { return false; }
        }
        return self.internal_groups_eq(a_return, b_return, encountered);
    }

    fn internal_variants_eq(
        &self, a: impl IntoVariantsTypeIdx, b: impl IntoVariantsTypeIdx,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        let a = &self.variants(a).0;
        let b = &self.variants(b).0;
        for variant in a.keys() {
            if !b.contains_key(variant) { return false; }
            if !self.internal_groups_eq(
                a.get(variant).expect("key from above"), b.get(variant).expect("checked above"),
                encountered
            ) { return false; }
        }
        for variant in b.keys() {
            if !a.contains_key(variant) { return false; }
            if !self.internal_groups_eq(
                a.get(variant).expect("checked above"), b.get(variant).expect("key from above"),
                encountered
            ) { return false; }
        }
        return true;
    }

    pub fn deduplicate(&mut self) {
        for group_idx in 0..self.groups.len() {
            for compared_group_idx in 0..group_idx {
                if self.groups_eq(TypeGroup(group_idx), TypeGroup(compared_group_idx)) {
                    self.try_merge_groups_internal(
                        TypeGroup(group_idx), TypeGroup(compared_group_idx), &mut HashSet::new()
                    );
                    break;
                }
            }
        }
    }
}
