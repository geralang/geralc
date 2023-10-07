
use std::collections::{HashMap, HashSet};

use crate::util::{
    strings::{StringMap, StringIdx},
    error::{Error, ErrorSection, ErrorType},
    source::HasSource
};
use crate::compiler::ast::{HasAstNodeVariant, AstNodeVariant};


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamespacePath {
    segments: Vec<StringIdx>
}

impl NamespacePath {
    pub fn new(segments: Vec<StringIdx>) -> NamespacePath {
        NamespacePath {
            segments
        }
    }

    pub fn display(&self, strings: &StringMap) -> String {
        let mut output = String::new();
        for segment in &*self.segments {
            if output.len() != 0 { output.push_str("::"); }
            output.push_str(strings.get(*segment));
        }
        output
    }

    pub fn get_segments(&self) -> &Vec<StringIdx> {
        &self.segments
    }
}


#[derive(Debug)]
pub struct Module<T: Clone + HasAstNodeVariant<T> + HasSource> {
    path: NamespacePath,
    symbols: HashMap<StringIdx, T>,
    exported: HashSet<StringIdx>
}

impl<T: Clone + HasAstNodeVariant<T> + HasSource> Module<T> {
    pub fn new(path: NamespacePath) -> Module<T> {
        Module {
            path,
            symbols: HashMap::new(),
            exported: HashSet::new()
        }
    }

    pub fn load(&mut self, mut nodes: Vec<T>, strings: &StringMap) -> Vec<Error> {
        let mut errors = Vec::new();
        while nodes.len() > 0 {
            let node = nodes.remove(0);
            match node.node_variant() {
                AstNodeVariant::Procedure { public, name, arguments: _, body: _ } |
                AstNodeVariant::Variable { public, mutable: _, name, value: _ } => {
                    let mut new_path_segments = self.path.get_segments().clone();
                    new_path_segments.push(*name);
                    let new_path = NamespacePath::new(new_path_segments);
                    if self.symbols.contains_key(name) {
                        errors.push(Error::new([
                            ErrorSection::Error(ErrorType::SymbolAlreadyExists(new_path.display(strings))),
                            ErrorSection::Code(node.source()),
                            ErrorSection::Info(format!("'{}' was previously declared here", new_path.display(strings))),
                            ErrorSection::Code(self.symbols.get(name).expect("checked above").source())
                        ].into()));
                    }
                    if *public { self.exported.insert(*name); }
                    self.symbols.insert(*name, node);
                }
                AstNodeVariant::Module { path: _ } => {
                    errors.push(Error::new([
                        ErrorSection::Error(ErrorType::ModuleDeclarationNotAtTop),
                        ErrorSection::Code(node.source())
                    ].into()));
                }
                _ => {
                    panic!("The grammar checker failed to see an invalid statement in the global scope!");
                }
            }
        }
        errors
    }

    pub fn canonicalize(self_path: &NamespacePath, modules: &HashMap<NamespacePath, Module<T>>) -> Vec<Error> {
        todo!("https://trello.com/c/16vKluys for more information");
        let mut errors = Vec::new();
        errors
    }
}