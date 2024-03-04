use std::collections::{HashMap, HashSet};

use crate::util::strings::{StringIdx, StringMap};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeGroup(usize);

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
    groups: Vec<usize>,
    internal_groups: Vec<HashSet<Type>>,
    arrays: Vec<TypeGroup>,
    objects: Vec<(HashMap<StringIdx, TypeGroup>, bool)>,
    concrete_objects: Vec<Vec<(StringIdx, TypeGroup)>>,
    closures: Vec<(Vec<TypeGroup>, TypeGroup)>,
    variants: Vec<(HashMap<StringIdx, TypeGroup>, bool)>,
    chosen_concrete: Vec<(HashSet<Type>, Type)>,
    merge_queue: Vec<(usize, usize)>
}

impl TypeMap {
    pub fn new() -> TypeMap {
        TypeMap {
            groups: Vec::new(),
            internal_groups: Vec::new(),
            arrays: Vec::new(),
            objects: Vec::new(),
            concrete_objects: Vec::new(),
            closures: Vec::new(),
            variants: Vec::new(),
            chosen_concrete: Vec::new(),
            merge_queue: Vec::new()
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
    pub fn insert_dedup_array(&mut self, v: TypeGroup) -> ArrayType {
        for idx in 0..self.arrays.len() {
            if !self.internal_arrays_eq(
                self.arrays[idx], v, &mut HashSet::new()
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
    pub fn insert_dedup_object(
        &mut self, v: (HashMap<StringIdx, TypeGroup>, bool)
    ) -> ObjectType {
        for idx in 0..self.objects.len() {
            if !self.internal_objects_eq(
                &self.objects[idx].clone(), &v, &mut HashSet::new()
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
            if !self.internal_concrete_objects_eq(
                &self.concrete_objects[idx].clone(), &v, &mut HashSet::new()
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

    pub fn internal_closures(&self) -> &Vec<(Vec<TypeGroup>, TypeGroup)> {
        &self.closures
    }
    pub fn insert_closure(
        &mut self, param_types: Vec<TypeGroup>, return_type: TypeGroup
    ) -> ClosureType {
        let idx = self.closures.len();
        self.closures.push((param_types, return_type));
        return ClosureType(idx);
    }
    pub fn insert_dedup_closure(
        &mut self,
        v: (Vec<TypeGroup>, TypeGroup)
    ) -> ClosureType {
        for idx in 0..self.closures.len() {
            if !self.internal_closures_eq(
                &self.closures[idx].clone(), &v, &mut HashSet::new()
            ) { continue; }
            return ClosureType(idx);
        }
        return self.insert_closure(v.0, v.1);
    }
    pub fn closure(
        &self, closure: ClosureType
    ) -> &(Vec<TypeGroup>, TypeGroup) {
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
    pub fn insert_dedup_variants(
        &mut self, v: (HashMap<StringIdx, TypeGroup>, bool)
    ) -> VariantsType {
        for idx in 0..self.variants.len() {
            if !self.internal_variants_eq(
                &self.variants[idx].clone(), &v, &mut HashSet::new()
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
        -> &Vec<HashSet<Type>> { &self.internal_groups }
    pub fn insert_group(&mut self, types: &[Type]) -> TypeGroup {
        let internal_idx = self.internal_groups.len();
        self.internal_groups.push(types.iter().map(|t| *t).collect());
        let group_idx = self.groups.len();
        self.groups.push(internal_idx);
        return TypeGroup(group_idx);
    }
    pub fn group(
        &self, group: TypeGroup
    ) -> impl Iterator<Item = Type> + '_ {
        let internal_idx = self.groups[group.0];
        return self.internal_groups[internal_idx].iter().map(|t| *t);
    }
    pub fn group_is_concrete(&self, group: TypeGroup) -> bool {
        self.group(group).count() == 1
    }
    pub fn group_concrete(
        &mut self, group: TypeGroup
    ) -> Type {
        let mut types = self.group(group).collect::<Vec<Type>>();
        if types.len() > 1 {
            // It can happen that a function that can accept multiple possible types is never used,
            // leading to any of the possible types being valid. If this is the case,
            // one shall be picked and the rest shall be removed.
            // Make sure that we always choose the same type when given the same possible types.
            let possible_types = self.internal_groups[self.groups[group.0]].clone();
            let chosen_type;
            if let Some(ct) = self.chosen_concrete.iter()
            .find(|(pt, _)| *pt == possible_types)
            .map(|(_, ct)| *ct) {
                chosen_type = ct;
            } else {
                chosen_type = types[0];
                self.chosen_concrete.push((possible_types, chosen_type));
            }
            types = vec![chosen_type];
            self.set_group_types(group, &types);
        }
        return types[0];
    }
    pub fn group_internal_id(&self, group: TypeGroup) -> usize {
        return self.groups[group.0];
    }
    pub fn set_group_types(
        &mut self, group: TypeGroup, new_types: &[Type]
    ) {
        let internal_id = self.group_internal_id(group);
        self.internal_groups[internal_id] = new_types.iter().map(|t| *t).collect(); 
    }

    fn compute_merged_types(
        &mut self, a_internal: usize, b_internal: usize
    ) -> HashSet<Type> {
        let mut merged_types = HashSet::new();
        let a_types = self.internal_groups[a_internal].iter().map(|t| *t).collect::<Vec<Type>>();
        let b_types = self.internal_groups[b_internal].iter().map(|t| *t).collect::<Vec<Type>>();
        for a_type in &a_types {
            for b_type in &b_types {
                if let Some(r_type) = self.try_merge_types_internal(*a_type, *b_type) {
                    merged_types.insert(r_type);
                }
            }
        }
        return merged_types;
    }

    pub fn try_merge_groups(
        &mut self,
        a: TypeGroup, b: TypeGroup
    ) -> bool {
        self.merge_queue.clear();
        if !self.try_merge_groups_internal(a, b) {
            return false;
        }
        while self.merge_queue.len() > 0 {
            let (a_internal, b_internal) = self.merge_queue[0];
            let merged_types = self.compute_merged_types(a_internal, b_internal);
            self.merge_queue.remove(0);
            if merged_types.len() == 0 {
                return false;
            }
            self.internal_groups[a_internal] = merged_types.clone();
            self.internal_groups[b_internal] = merged_types;
            self.merge_groups_internally(a_internal, b_internal);
        }
        return true;
    }

    fn merge_groups_internally(&mut self, a_internal: usize, b_internal: usize) {
        if a_internal == b_internal { return; }
        for internal_group_idx in &mut self.groups {
            if *internal_group_idx == b_internal {
                *internal_group_idx = a_internal;
            }
        }
    }

    fn try_merge_groups_internal(
        &mut self,
        a: TypeGroup, b: TypeGroup
    ) -> bool {
        let queue_item = (self.group_internal_id(a), self.group_internal_id(b));
        if queue_item.0 == queue_item.1 { return true; }
        if self.merge_queue.contains(&queue_item) { return true; }
        self.merge_queue.push(queue_item);
        let merged_types = self.compute_merged_types(queue_item.0, queue_item.1);
        if merged_types.is_empty() { return false; }
        self.internal_groups[queue_item.0] = merged_types.clone();
        self.internal_groups[queue_item.1] = merged_types;
        return true;
    }

    fn try_merge_types_internal(
        &mut self,
        a: Type, b: Type
    ) -> Option<Type> {
        match (a, b) {
            (Type::Any, Type::Any) => Some(Type::Any),
            (Type::Any, b) => Some(b),
            (a, Type::Any) => Some(a),
            (Type::ConcreteObject(obj_a), b) => {
                let obj_type = Type::Object(self.insert_object(
                    self.concrete_object(obj_a).iter().map(|e| *e).collect(),
                    false
                ));
                self.try_merge_types_internal(obj_type, b)
            }
            (a, Type::ConcreteObject(obj_b)) => {
                let obj_type = Type::Object(self.insert_object(
                    self.concrete_object(obj_b).iter().map(|e| *e).collect(),
                    false
                ));
                self.try_merge_types_internal(a, obj_type)
            }
            (Type::Array(arr_a), Type::Array(arr_b)) => {
                if self.try_merge_groups_internal(
                    self.array(arr_a), self.array(arr_b)
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
                                *member_type_a, *member_type_b
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
                        (None, None) => unreachable!()
                    }
                }
                Some(Type::Object(self.insert_object(
                    new_members, fixed_a || fixed_b
                )))
            }
            (Type::Closure(clo_a), Type::Closure(clo_b)) => {
                let (params_a, return_a) = self.closure(clo_a).clone();
                let (params_b, return_b) = self.closure(clo_b).clone();
                if params_a.len() != params_b.len() { return None }
                for p in 0..params_a.len() {
                    if !self.try_merge_groups_internal(
                        params_a[p], params_b[p]
                    ) { return None; }
                }
                if !self.try_merge_groups_internal(
                    return_a, return_b
                ) { return None; }
                Some(Type::Closure(self.insert_closure(
                    params_a.clone(),
                    return_a
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
                                *variant_type_a, *variant_type_b
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

    pub fn duplicate_group(
        &mut self, src: TypeGroup
    ) -> TypeGroup {
        self.internal_duplicate_group(
            src, &mut HashMap::new()
        )
    }

    pub fn duplicate_call_signature(
        &mut self, param_types: &[TypeGroup], return_type: TypeGroup
    ) -> (Vec<TypeGroup>, TypeGroup) {
        let mut encountered = HashMap::new();
        let param_types = param_types.iter()
            .map(|t| self.internal_duplicate_group(*t, &mut encountered))
            .collect();
        let return_type = self.internal_duplicate_group(return_type, &mut encountered);
        return (param_types, return_type);
    }

    fn internal_duplicate_group(
        &mut self,
        src: TypeGroup,
        encountered: &mut HashMap<usize, TypeGroup>
    ) -> TypeGroup {
        let id = self.group_internal_id(src);
        if let Some(g) = encountered.get(&id) { return *g; }
        let new_group = self.insert_group(&[]);
        encountered.insert(id, new_group);
        let transferred_types = self.group(src)
            .collect::<Vec<Type>>().into_iter()
            .map(|t| self.internal_duplicate_type(t, encountered))
            .collect::<Vec<Type>>(); 
        self.set_group_types(new_group, &transferred_types);
        new_group
    }

    fn internal_duplicate_type(
        &mut self,
        t: Type,
        encountered: &mut HashMap<usize, TypeGroup>
    ) -> Type {
        match t {
            Type::Array(arr) => {
                let t = self.internal_duplicate_group(
                    self.array(arr), encountered
                );
                Type::Array(self.insert_array(t))
            },
            Type::Object(obj) => {
                let (old_members, fixed) = self.object(obj).clone();
                let new_members = old_members.into_iter().map(|(mn, mt)| (
                    mn,
                    self.internal_duplicate_group(
                        mt, encountered
                    )
                )).collect();
                Type::Object(self.insert_object(new_members, fixed))
            }
            Type::ConcreteObject(obj) => {
                let old_members = self.concrete_object(obj).clone();
                let new_members = old_members.into_iter().map(|(mn, mt)| (
                    mn,
                    self.internal_duplicate_group(
                        mt, encountered
                    )
                )).collect();
                Type::ConcreteObject(self.insert_concrete_object(new_members))
            }
            Type::Closure(clo) => {
                let (old_param_types, old_return_type) = self.closure(clo).clone();
                let new_param_types = old_param_types.into_iter().map(|t| self.internal_duplicate_group(
                    t, encountered
                )).collect();
                let new_return_type = self.internal_duplicate_group(
                    old_return_type, encountered
                );
                Type::Closure(self.insert_closure(
                    new_param_types, 
                    new_return_type
                ))
            }
            Type::Variants(var) => {
                let (old_variants, fixed) = self.variants(var).clone();
                let new_variants = old_variants.into_iter().map(|(vn, vt)| (
                    vn,
                    self.internal_duplicate_group(
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
        &mut self, a: TypeGroup, b: TypeGroup
    ) -> bool {
        self.internal_groups_eq(a, b, &mut HashSet::new())
    }

    fn internal_groups_eq(
        &mut self,
        a: TypeGroup, b: TypeGroup,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        let a_internal = self.group_internal_id(a);
        let b_internal = self.group_internal_id(b);
        if a_internal == b_internal { return true; }
        let internal = (a_internal, b_internal);
        if encountered.contains(&internal) { return true; }
        encountered.insert(internal);
        let mut result = true;
        for group_a_t in self.group(a).collect::<Vec<Type>>() {
            let mut found = false;
            for group_b_t in self.group(b).collect::<Vec<Type>>() {
                if !self.internal_types_eq(
                    group_a_t, group_b_t, encountered
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
        &mut self,
        a: Type, b: Type,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        match (a, b) {
            (Type::Array(arr_a), Type::Array(arr_b)) => {
                if arr_a.get_internal_id() == arr_b.get_internal_id() { return true; }
                self.internal_arrays_eq(
                    self.array(arr_a),
                    self.array(arr_b),
                    encountered
                )
            }
            (Type::Object(obj_a), Type::Object(obj_b)) => {
                if obj_a.get_internal_id() == obj_b.get_internal_id() { return true; }
                self.internal_objects_eq(
                    &self.object(obj_a).clone(),
                    &self.object(obj_b).clone(),
                    encountered
                )
            }
            (Type::ConcreteObject(obj_a), Type::ConcreteObject(obj_b)) => {
                if obj_a.get_internal_id() == obj_b.get_internal_id() { return true; }
                self.internal_concrete_objects_eq(
                    &self.concrete_object(obj_a).clone(),
                    &self.concrete_object(obj_b).clone(),
                    encountered
                )
            }
            (Type::Closure(clo_a), Type::Closure(clo_b)) => {
                if clo_a.get_internal_id() == clo_b.get_internal_id() { return true; }
                self.internal_closures_eq(
                    &self.closure(clo_a).clone(),
                    &self.closure(clo_b).clone(),
                    encountered
                )
            }
            (Type::Variants(var_a), Type::Variants(var_b)) => {
                if var_a.get_internal_id() == var_b.get_internal_id() { return true; }
                self.internal_variants_eq(
                    &self.variants(var_a).clone(),
                    &self.variants(var_b).clone(),
                    encountered
                )
            }
            (a, b) => {
                std::mem::discriminant(&a) == std::mem::discriminant(&b)
            }
        }
    }

    fn internal_arrays_eq(
        &mut self,
        a: TypeGroup, b: TypeGroup,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        self.internal_groups_eq(a, b, encountered)
    }

    fn internal_objects_eq(
        &mut self,
        a: &(HashMap<StringIdx, TypeGroup>, bool),
        b: &(HashMap<StringIdx, TypeGroup>, bool),
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        let a = &a.0;
        let b = &b.0;
        for member in a.keys() {
            if !b.contains_key(member) { return false; }
            let a_member = *a.get(member).expect("key from above");
            let b_member = *b.get(member).expect("checked above");
            if !self.internal_groups_eq(
                a_member, b_member,
                encountered
            ) { return false; }
        }
        for member in b.keys() {
            if !a.contains_key(member) { return false; }
            let a_member = *a.get(member).expect("key from above");
            let b_member = *b.get(member).expect("checked above");
            if !self.internal_groups_eq(
                a_member, b_member,
                encountered
            ) { return false; }
        }
        return true;
    }

    fn internal_concrete_objects_eq(
        &mut self,
        a: &Vec<(StringIdx, TypeGroup)>,
        b: &Vec<(StringIdx, TypeGroup)>,
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        if a.len() != b.len() { return false; }
        for member_idx in 0..a.len() {
            let a_member = a[member_idx].1;
            let b_member = b[member_idx].1;
            if !self.internal_groups_eq(
                a_member, b_member,
                encountered
            ) { return false; }
        }
        return true;
    }

    fn internal_closures_eq(
        &mut self,
        a: &(Vec<TypeGroup>, TypeGroup),
        b: &(Vec<TypeGroup>, TypeGroup),
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        let (a_params, a_return) = a;
        let (b_params, b_return) = b;
        if a_params.len() != b_params.len() { return false; }
        for p in 0..a_params.len() {
            let a_param = a_params[p];
            let b_param = b_params[p];
            if !self.internal_groups_eq(
                a_param, b_param, encountered
            ) { return false; }
        }
        return self.internal_groups_eq(
            *a_return, *b_return, encountered
        );
    }

    fn internal_variants_eq(
        &mut self,
        a: &(HashMap<StringIdx, TypeGroup>, bool),
        b: &(HashMap<StringIdx, TypeGroup>, bool),
        encountered: &mut HashSet<(usize, usize)>
    ) -> bool {
        let a = &a.0;
        let b = &b.0;
        for variant in a.keys() {
            if !b.contains_key(variant) { return false; }
            let a_variant = *a.get(variant).expect("key from above");
            let b_variant = *b.get(variant).expect("checked above");
            if !self.internal_groups_eq(
                a_variant, b_variant,
                encountered
            ) { return false; }
        }
        for variant in b.keys() {
            if !a.contains_key(variant) { return false; }
            let a_variant = *a.get(variant).expect("key from above");
            let b_variant = *b.get(variant).expect("checked above");
            if !self.internal_groups_eq(
                a_variant, b_variant,
                encountered
            ) { return false; }
        }
        return true;
    }

    pub fn deduplicated(&mut self) -> TypeMap {
        let mut new = TypeMap::new();
        // deduplicate arrays
        let mut mapped_arrays = HashMap::new();
        for og_array_idx in 0..self.arrays.len() {
            let mut found = false;
            for new_array_idx in 0..new.arrays.len() {
                if !self.internal_arrays_eq(
                    self.arrays[og_array_idx],
                    new.arrays[new_array_idx],
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
                if !self.internal_objects_eq(
                    &self.objects[og_object_idx].clone(),
                    &new.objects[new_object_idx],
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
                if !self.internal_concrete_objects_eq(
                    &self.concrete_objects[og_concrete_object_idx].clone(),
                    &new.concrete_objects[new_concrete_object_idx].clone(),
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
                if !self.internal_closures_eq(
                    &self.closures[og_closure_idx].clone(),
                    &new.closures[new_closure_idx],
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
                if !self.internal_variants_eq(
                    &self.variants[og_variants_idx].clone(),
                    &new.variants[new_variants_idx],
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
                Type::Array(ArrayType(*arrays.get(&i.0).unwrap_or_else(|| panic!("index {} doesn't have a mapping!", i.0))))
            }
            Type::Object(i) => {
                Type::Object(ObjectType(*objects.get(&i.0).unwrap_or_else(|| panic!("index {} doesn't have a mapping!", i.0))))
            }
            Type::ConcreteObject(i) => {
                Type::ConcreteObject(ConcreteObjectType(
                    *concrete_objects.get(&i.0).unwrap_or_else(|| panic!("index {} doesn't have a mapping!", i.0))
                ))
            }
            Type::Closure(i) => {
                Type::Closure(ClosureType(*closures.get(&i.0).unwrap_or_else(|| panic!("index {} doesn't have a mapping!", i.0))))
            }
            Type::Variants(i) => {
                Type::Variants(VariantsType(*variants.get(&i.0).unwrap_or_else(|| panic!("index {} doesn't have a mapping!", i.0))))
            }
        } }
        new.groups = self.groups.clone();
        new.internal_groups = self.internal_groups.iter().map(|types|
                types.iter().map(|t|
                    apply_mappings_type(
                        *t, &mapped_arrays, &mapped_objects, &mapped_concrete_objects,
                        &mapped_closures, &mapped_variants
                    )
                ).collect()
            ).collect();
        // done
        return new;
    }

    pub fn replace_any_with_unit(&mut self) {
        for group_types in &mut self.internal_groups {
            *group_types = group_types.iter().map(|t|
                if let Type::Any = *t { Type::Unit } else { *t }
            ).collect();
        }
    }

    pub fn display_types(&self, strings: &StringMap, t: TypeGroup) -> String {
        fn choose_letter(i: usize) -> String {
            const LETTERS: [char; 26] = [
                'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q',
                'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
            ];
            let mut i = i;
            let mut r = String::new();
            loop {
                let c = i % LETTERS.len();
                r.push(LETTERS[c]);
                i = i / LETTERS.len();
                if i == 0 { break; }
            }
            r
        }
        fn collect_letters(
            letters: &mut HashMap<usize, (String, usize)>,
            types: &TypeMap,
            t: TypeGroup
        ) {
            let group_internal_idx = types.group_internal_id(t);
            if let Some((_, usages)) = letters.get_mut(&group_internal_idx) {
                *usages += 1;
                if *usages >= 2 { return; }
            } else {
                let letter = choose_letter(letters.len());
                letters.insert(group_internal_idx, (letter, 1));
            }
            for possible_type in types.group(t).collect::<Vec<Type>>() {
                collect_type_letters(letters, types, possible_type)
            }
        }
        fn collect_type_letters(
            letters: &mut HashMap<usize, (String, usize)>,
            types: &TypeMap,
            collected_type: Type
        ) {
            match collected_type {
                Type::Any |
                Type::Unit |
                Type::Boolean |
                Type::Integer |
                Type::Float |
                Type::String => {}
                Type::Array(arr) => collect_letters(letters, types, types.array(arr)),
                Type::Object(obj) => {
                    for member_types in types.object(obj).0.values().map(|t| *t).collect::<Vec<TypeGroup>>() {
                        collect_letters(letters, types, member_types);
                    }
                }
                Type::ConcreteObject(obj) => {
                    for (_, member_types) in types.concrete_object(obj).clone() {
                        collect_letters(letters, types, member_types);
                    }
                }
                Type::Closure(clo) => {
                    let (parameter_types, return_types) = types.closure(clo).clone();
                    for parameter_types in parameter_types {
                        collect_letters(letters, types, parameter_types);
                    }
                    collect_letters(letters, types, return_types);
                }
                Type::Variants(var) => {
                    for variant_types in types.variants(var).0.values().map(|t| *t).collect::<Vec<TypeGroup>>() {
                        collect_letters(letters, types, variant_types);
                    }
                }
            }
        }
        fn display_group_types(
            group_types: &[Type],
            strings: &StringMap,
            types: &TypeMap,
            letters: &HashMap<usize, (String, usize)>
        ) -> String {
            let mut result = String::new();
            if group_types.len() > 1 { 
                result.push_str("(");
            }
            for i in 0..group_types.len() {
                if i > 0 { result.push_str(" | "); }
                result.push_str(&display_type(strings, types, group_types[i], letters));
            }
            if group_types.len() > 1 { 
                result.push_str(")");
            }
            result
        }
        fn display_type(
            strings: &StringMap,
            types: &TypeMap,
            displayed_type: Type,
            letters: &HashMap<usize, (String, usize)>
        ) -> String {
            match displayed_type {
                Type::Any => String::from("any"),
                Type::Unit => String::from("unit"),
                Type::Boolean => String::from("boolean"),
                Type::Integer => String::from("integer"),
                Type::Float => String::from("float"),
                Type::String => String::from("string"),
                Type::Array(arr) => format!(
                    "[{}]",
                    display_types_internal(strings, types, types.array(arr), letters)
                ),
                Type::Object(obj) => {
                    let (member_types, fixed) = types.object(obj).clone();
                    format!(
                        "{{ {}{} }}",
                        member_types.into_iter().map(|(member_name, member_type)| { format!(
                            "{} = {}",
                            strings.get(member_name),
                            display_types_internal(strings, types, member_type, letters)
                        ) }).collect::<Vec<String>>().join(", "),
                        if fixed { "" } else { ", ..." }
                    )
                }
                Type::ConcreteObject(obj) => format!(
                    "{{ {}, ... }}",
                    types.concrete_object(obj).clone().into_iter().map(|(member_name, member_type)| { format!(
                        "{} = {}",
                        strings.get(member_name),
                        display_types_internal(strings, types, member_type, letters)
                    ) }).collect::<Vec<String>>().join(", ")
                ),
                Type::Closure(clo) => {
                    let (arg_groups, returned_group) = types.closure(clo).clone();
                    let mut result: String = String::from("(");
                    for a in 0..arg_groups.len() {
                        if a > 0 { result.push_str(", "); }
                        result.push_str(&display_types_internal(strings, types, arg_groups[a], letters));
                    }
                    result.push_str(") -> ");
                    result.push_str(&display_types_internal(strings, types, returned_group, letters));
                    result
                },
                Type::Variants(var) => {
                    let (variant_types, fixed) = types.variants(var).clone();
                    format!(
                        "({}{})",
                        variant_types.into_iter().map(|(variant_name, variant_type)| {
                            format!(
                                "#{} {}",
                                strings.get(variant_name),
                                display_types_internal(strings, types, variant_type, letters)
                            )
                        }).collect::<Vec<String>>().join(" | "),
                        if fixed { "" } else { " | ..." }
                    )
                }
            }
        }
        fn display_types_internal(
            strings: &StringMap,
            types: &TypeMap,
            t: TypeGroup,
            letters: &HashMap<usize, (String, usize)>
        ) -> String {
            let group_internal_idx = types.group_internal_id(t);
            if let Some((letter, usage_count)) = letters.get(&group_internal_idx) {
                if *usage_count >= 2 {
                    return letter.clone();
                }
            }
            display_group_types(&types.group(t).collect::<Vec<Type>>(), strings, types, letters)
        }
        let mut letters = HashMap::new();
        collect_letters(&mut letters, self, t);
        let mut result = display_types_internal(strings, self, t, &letters);
        let mut letter_types = String::new();
        for (internal_group_idx, (letter, usage_count)) in &letters {
            if *usage_count < 2 { continue; }
            if letter_types.len() > 0 { letter_types.push_str(", "); }
            letter_types.push_str(letter);
            letter_types.push_str(" = ");
            letter_types.push_str(&display_group_types(
                &self.internal_groups()[*internal_group_idx].iter().map(|t| *t).collect::<Vec<Type>>(),
                strings, self, &letters
            ));
        }   
        if letter_types.len() > 0 {
            result.push_str(" where ");
            result.push_str(&letter_types);
        }
        result
    }

    pub fn display_type_difference(
        &self, strings: &StringMap, a: TypeGroup, b: TypeGroup
    ) -> Option<String> {
        fn display_type_simple(t: Type) -> String { match t {
            Type::Any => "any".into(),
            Type::Unit => "unit".into(),
            Type::Boolean => "boolean".into(),
            Type::Integer => "integer".into(),
            Type::Float => "float".into(),
            Type::String => "string".into(),
            Type::Array(_) => "array".into(),
            Type::Object(_) | Type::ConcreteObject(_) => "object".into(),
            Type::Closure(_) => "closure".into(),
            Type::Variants(_) => "variants".into(),
        } }
        fn internal_group_difference(
            types: &TypeMap, strings: &StringMap,
            encountered: &mut HashSet<(usize, usize)>, a: TypeGroup, b: TypeGroup
        ) -> TypeDifference {
            let encounter = (types.group_internal_id(a), types.group_internal_id(b));
            if encountered.contains(&encounter) { return TypeDifference::Recursive; }
            encountered.insert(encounter);
            let mut similar = false;
            let mut r = TypeDifference::Mergable;
            for type_a in types.group(a) {
                for type_b in types.group(b) {
                    let cd = internal_type_difference(types, strings, encountered, type_a, type_b);
                    match cd {
                        TypeDifference::Mergable => return TypeDifference::Mergable,
                        _ => {
                            use std::mem::discriminant;
                            if discriminant(&type_a) == discriminant(&type_b) { similar = true; }
                            r = r & cd;
                        }
                    }
                }
            }
            encountered.remove(&encounter);
            if similar { return r; }
            return TypeDifference::Different(format!(
                "=> {} != {}",
                types.group(a).map(display_type_simple)
                    .collect::<HashSet<String>>().into_iter()
                    .collect::<Vec<String>>().join(" | "),
                types.group(b).map(display_type_simple)
                    .collect::<HashSet<String>>().into_iter()
                    .collect::<Vec<String>>().join(" | ")
            ));
        }
        fn internal_type_difference(
            types: &TypeMap, strings: &StringMap,
            encountered: &mut HashSet<(usize, usize)>, a: Type, b: Type
        ) -> TypeDifference {
            use std::mem::discriminant;
            match (a, b) {
                (Type::Any, _) | (_, Type::Any) => TypeDifference::Mergable,
                (Type::Array(arr_a), Type::Array(arr_b)) => {
                    internal_group_difference(
                        types, strings, encountered, types.array(arr_a), types.array(arr_b)
                    ).map(|d| format!("-> in any array element\n{}", d))
                }
                (Type::Object(obj_a), Type::Object(obj_b)) => {
                    let (members_a, fixed_a) = types.object(obj_a);
                    let (members_b, fixed_b) = types.object(obj_b);
                    let mut r = TypeDifference::Mergable;
                    for member in members_a.keys().chain(members_b.keys()) {
                        match (members_a.get(member), members_b.get(member)) {
                            (Some(member_a), Some(member_b)) => {
                                let d = internal_group_difference(
                                    types, strings, encountered, *member_a, *member_b
                                ).map(|d| format!(
                                    "-> in object member '{}'\n{}", strings.get(*member), d
                                ));
                                r = r & d;
                            }
                            (Some(_), None) => if *fixed_b {
                                return TypeDifference::Different(format!(
                                    "=> object member '{}' is missing", strings.get(*member)
                                ))
                            }
                            (None, Some(_)) => if *fixed_a {
                                return TypeDifference::Different(format!(
                                    "=> object member '{}' is missing", strings.get(*member)
                                ))
                            }
                            (None, None) => unreachable!()
                        }
                    }
                    return r;
                }
                (Type::ConcreteObject(obj_a), Type::ConcreteObject(obj_b)) => {
                    let members_a = types.concrete_object(obj_a);
                    let members_b = types.concrete_object(obj_b);
                    let mut r = TypeDifference::Mergable;
                    for member in members_a.iter().map(|v| &v.0)
                            .chain(members_b.iter().map(|v| &v.0)) {
                        match (
                            members_a.iter().find(|v| v.0 == *member).map(|v| &v.1),
                            members_b.iter().find(|v| v.0 == *member).map(|v| &v.1)
                        ) {
                            (Some(member_a), Some(member_b)) => {
                                let d = internal_group_difference(
                                    types, strings, encountered, *member_a, *member_b
                                ).map(|d| format!(
                                    "-> in object member '{}'\n{}", strings.get(*member), d
                                ));
                                r = r & d;
                            }
                            (Some(_), None) |
                            (None, Some(_)) => {}
                            (None, None) => unreachable!()
                        }
                    }
                    return r;
                }
                (Type::Closure(clo_a), Type::Closure(clo_b)) => {
                    let (params_a, return_a) = types.closure(clo_a);
                    let (params_b, return_b) = types.closure(clo_b);
                    let mut r = TypeDifference::Mergable;
                    if params_a.len() != params_b.len() {
                        return TypeDifference::Different(format!(
                            "=> functions take {} and {} argument{}",
                            params_a.len(), params_b.len(),
                            if params_b.len() == 1 { "s" } else { "" }
                        ))
                    }
                    for p in 0..params_a.len() {
                        let d = internal_group_difference(
                            types, strings, encountered, params_a[p], params_b[p]
                        ).map(|d| format!(
                            "-> in {}{} function parameter\n{}", p + 1, match (p + 1) % 10 {
                                1 => "st", 2 => "nd", 3 => "rd", _ => "th"
                            }, d
                        ));
                        r = r & d;
                    }
                    return r & internal_group_difference(
                        types, strings, encountered, *return_a, *return_b
                    ).map(|d| format!(
                        "-> in function return type\n{}", d
                    ));
                }
                (Type::Variants(var_a), Type::Variants(var_b)) => {
                    let (variants_a, fixed_a) = types.variants(var_a);
                    let (variants_b, fixed_b) = types.variants(var_b);
                    let mut r = TypeDifference::Mergable;
                    for variant in variants_a.keys().chain(variants_b.keys()) {
                        match (variants_a.get(variant), variants_b.get(variant)) {
                            (Some(variant_a), Some(variant_b)) => {
                                let d = internal_group_difference(
                                    types, strings, encountered, *variant_a, *variant_b
                                ).map(|d| format!(
                                    "-> in variant '{}'\n{}", strings.get(*variant), d
                                ));
                                r = r & d;
                            }
                            (Some(_), None) => if *fixed_b {
                                return TypeDifference::Different(format!(
                                    "=> variant '{}' is missing", strings.get(*variant)
                                ))
                            }
                            (None, Some(_)) => if *fixed_a {
                                return TypeDifference::Different(format!(
                                    "=> variant '{}' is missing", strings.get(*variant)
                                ))
                            }
                            (None, None) => unreachable!()
                        }
                    }
                    return r;
                }
                (a, b) => if discriminant(&a) == discriminant(&b) { TypeDifference::Mergable } else {
                    TypeDifference::Different(format!("=> {} != {}", display_type_simple(a), display_type_simple(b)))
                }
            }
        }
        if let TypeDifference::Different(d)
            = internal_group_difference(self, strings, &mut HashSet::new(), a, b) {
            Some(d)
        } else { None }
    }
}

enum TypeDifference {
    Different(String),
    Recursive,
    Mergable
}

impl TypeDifference {
    fn map<F: Fn(String) -> String>(self, f: F) -> Self {
        if let TypeDifference::Different(d) = self {
            TypeDifference::Different((f)(d))
        } else { self }
    }
}

impl std::ops::BitAnd for TypeDifference {
    type Output = TypeDifference;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (TypeDifference::Different(a), TypeDifference::Different(b))
                => TypeDifference::Different(if a.len() < b.len() { a } else { b }),
            (TypeDifference::Different(d), _) | (_, TypeDifference::Different(d))
                => TypeDifference::Different(d),
            (TypeDifference::Recursive, _) | (_, TypeDifference::Recursive)
                => TypeDifference::Recursive, 
            _ => TypeDifference::Mergable
        }
    }
}