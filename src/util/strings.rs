
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct StringIdx(usize);

pub struct StringMap {
    indices: HashMap<Box<str>, StringIdx>,
    strings: Vec<Box<str>>
}

impl StringMap {
    pub fn new() -> StringMap {
        StringMap { indices: HashMap::new(), strings: Vec::new() }
    }

    pub fn insert(&mut self, string: &str) -> StringIdx {
        if let Some(idx) = self.indices.get(string) { return *idx; }
        let idx = StringIdx(self.strings.len());
        self.indices.insert(string.into(), idx);
        self.strings.push(string.into());
        idx
    }

    pub fn get<'s>(&'s self, idx: StringIdx) -> &'s str {
        &self.strings[idx.0]
    }
}

