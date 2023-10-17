
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
    file_name: StringIdx,
    symbols: HashMap<StringIdx, T>,
    exported: HashMap<StringIdx, bool>,
    usages: Vec<NamespacePath>
}

impl<T: Clone + HasAstNodeVariant<T> + HasSource> Module<T> {
    pub fn new(path: NamespacePath, file_name: StringIdx, nodes: Vec<T>, strings: &StringMap) -> Result<Module<T>, Vec<Error>> {
        let mut new = Module {
            path,
            file_name,
            symbols: HashMap::new(),
            exported: HashMap::new(),
            usages: Vec::new()
        };
        let load_errors = new.load(nodes, strings);
        if load_errors.len() > 0 { Err(load_errors) } else { Ok(new) }
    }

    fn load(&mut self, mut nodes: Vec<T>, strings: &StringMap) -> Vec<Error> {
        let mut errors = Vec::new();
        while nodes.len() > 0 {
            let mut node = nodes.remove(0);
            let node_source = node.source();
            match node.node_variant_mut() {
                AstNodeVariant::Procedure { public, name, arguments: _, body: _ } |
                AstNodeVariant::Variable { public, mutable: _, name, value: _ } => {
                    let mut new_path_segments = self.path.get_segments().clone();
                    new_path_segments.push(*name);
                    let new_path = NamespacePath::new(new_path_segments);
                    if self.symbols.contains_key(name) {
                        errors.push(Error::new([
                            ErrorSection::Error(ErrorType::SymbolAlreadyExists(new_path.display(strings))),
                            ErrorSection::Code(node_source),
                            ErrorSection::Info(format!("'{}' was previously declared here", new_path.display(strings))),
                            ErrorSection::Code(self.symbols.get(name).expect("checked above").source())
                        ].into()));
                    }
                    self.exported.insert(*name, *public);
                    self.symbols.insert(*name, node);
                }
                AstNodeVariant::Module { path: _ } => {
                    errors.push(Error::new([
                        ErrorSection::Error(ErrorType::ModuleDeclarationNotAtTop),
                        ErrorSection::Code(node.source())
                    ].into()));
                }
                AstNodeVariant::Use { paths } => {
                    self.usages.append(paths);
                } 
                _ => {
                    panic!("The grammar checker failed to see an invalid statement in the global scope!");
                }
            }
        }
        errors
    }

    pub fn file_name(&self) -> StringIdx { self.file_name }
    pub fn symbols(self) -> HashMap<StringIdx, T> { self.symbols }

    pub fn canonicalize(&mut self, modules: &HashMap<NamespacePath, Module<T>>, strings: &mut StringMap) -> Vec<Error> {
        self.expand_wildcards(modules, strings);
        let mut errors = Vec::new();
        let symbol_names = self.symbols.keys().map(|s| *s).collect::<Vec<StringIdx>>();
        for symbol_name in symbol_names {
            let mut symbol = self.symbols.remove(&symbol_name).expect("key must be valid");
            match symbol.node_variant() {
                // catch 'use' here later
                _ => {
                    errors.append(&mut self.canonicalize_node(&mut symbol, modules, &mut HashSet::new(), strings));
                    self.symbols.insert(symbol_name, symbol);
                }
            }
        }
        errors
    }

    fn expand_wildcards(&mut self, modules: &HashMap<NamespacePath, Module<T>>, strings: &mut StringMap) {
        let asterisk_idx = strings.insert("*");
        let mut i = 0;
        while i < self.usages.len() {
            let usage_path_ref = &self.usages[i];
            if usage_path_ref.get_segments()[usage_path_ref.get_segments().len() - 1] != asterisk_idx {
                i += 1;
                continue;
            }
            let usage = self.usages.remove(i);
            let usage_mod_segments: Vec<StringIdx> = usage.segments[0..usage.segments.len() - 1].into();
            // use all symbols in the module
            if let Some(module) = modules.get(&NamespacePath::new(usage_mod_segments.clone())) {
                for symbol in &module.exported {
                    let mut new_usage_segments = usage_mod_segments.clone();
                    new_usage_segments.push(*symbol.0);
                    self.usages.push(NamespacePath::new(new_usage_segments));
                }
            }
            // use all submodules
            for module_path in modules.keys() {
                if module_path.segments.len() <= usage_mod_segments.len() { continue; }
                if module_path.segments[..usage_mod_segments.len()] != usage_mod_segments { continue; }
                self.usages.push(NamespacePath::new(module_path.segments[..usage_mod_segments.len() + 1].into()));
            }
        }
    }

    fn canonicalize_nodes(&self, nodes: &mut [T], modules: &HashMap<NamespacePath, Module<T>>, variables: &mut HashSet<StringIdx>, strings: &StringMap) -> Vec<Error> {
        let mut errors = Vec::new();
        for node in nodes {
            errors.append(&mut self.canonicalize_node(node, modules, variables, strings));
        }
        errors
    }

    fn canonicalize_node(&self, node: &mut T, modules: &HashMap<NamespacePath, Module<T>>, variables: &mut HashSet<StringIdx>, strings: &StringMap) -> Vec<Error> {
        let mut errors = Vec::new();
        macro_rules! visit_node {
            ($node: expr) => { errors.append(&mut self.canonicalize_node($node, modules, variables, strings)) }
        }
        macro_rules! visit_nodes {
            ($nodes: expr) => { errors.append(&mut self.canonicalize_nodes($nodes, modules, &mut variables.clone(), strings)) }
        }
        let node_source = node.source();
        let node_variant = node.node_variant_mut();
        match node_variant {
            AstNodeVariant::Procedure { public: _, name: _, arguments: _, body } => {
                visit_nodes!(body);
            }
            AstNodeVariant::Function { arguments: _, body } => {
                visit_nodes!(body);
            }
            AstNodeVariant::Variable { public: _, mutable: _, name, value } => {
                if let Some(value) = value {
                    visit_node!(&mut **value);
                }
                variables.insert(*name);
            }
            AstNodeVariant::CaseBranches { value, branches, else_body } => {
                visit_node!(&mut **value);
                for branch in branches {
                    visit_node!(&mut branch.0);
                    visit_nodes!(&mut branch.1);
                }
                visit_nodes!(else_body);
            }
            AstNodeVariant::CaseConditon { condition, body, else_body } => {
                visit_node!(&mut **condition);
                visit_nodes!(body);
                visit_nodes!(else_body);
            }
            AstNodeVariant::Assignment { variable, value } => {
                visit_node!(&mut **variable);
                visit_node!(&mut **value);
            }
            AstNodeVariant::Return { value } => {
                visit_node!(&mut **value);
            }
            AstNodeVariant::Call { called, arguments } => {
                visit_node!(&mut **called);
                visit_nodes!(arguments);
            }
            AstNodeVariant::Object { values } => {
                for member in values {
                    visit_node!(&mut member.1);
                }
            }
            AstNodeVariant::Array { values } => {
                visit_nodes!(values);
            }
            AstNodeVariant::ObjectAccess { object, member: _ } => {
                visit_node!(&mut **object);
            }
            AstNodeVariant::ArrayAccess { array, index } => {
                visit_node!(&mut **array);
                visit_node!(&mut **index);
            }
            AstNodeVariant::VariableAccess { name } => {
                if !variables.contains(name) && self.exported.contains_key(name) {
                    let mut path_segments = self.path.get_segments().clone();
                    path_segments.push(*name);
                    *node_variant = AstNodeVariant::ModuleAccess { path: NamespacePath::new(path_segments) };
                    errors.append(&mut self.canonicalize_node(node, modules, variables, strings));
                } else if !variables.contains(name) {
                    let mut last_usage = None;
                    for usage in &self.usages {
                        if usage.segments[usage.segments.len() - 1] != *name { continue; }
                        last_usage = Some(usage);
                    }
                    if let Some(usage) = last_usage {
                        *node_variant = AstNodeVariant::ModuleAccess { path: usage.clone() };
                        errors.append(&mut self.canonicalize_node(node, modules, variables, strings));
                    }
                }
            }
            AstNodeVariant::BooleanLiteral { value: _ } |
            AstNodeVariant::IntegerLiteral { value: _ } |
            AstNodeVariant::FloatLiteral { value: _ } |
            AstNodeVariant::StringLiteral { value: _ } |
            AstNodeVariant::UnitLiteral => {}
            AstNodeVariant::Add { a, b } |
            AstNodeVariant::Subtract { a, b } |
            AstNodeVariant::Multiply { a, b } |
            AstNodeVariant::Divide { a, b } |
            AstNodeVariant::Modulo { a, b } |
            AstNodeVariant::LessThan { a, b } |
            AstNodeVariant::GreaterThan { a, b } |
            AstNodeVariant::LessThanEqual { a, b } |
            AstNodeVariant::GreaterThanEqual { a, b } |
            AstNodeVariant::Equals { a, b } |
            AstNodeVariant::NotEquals { a, b } |
            AstNodeVariant::Or { a, b } |
            AstNodeVariant::And { a, b } => {
                visit_node!(&mut **a);
                visit_node!(&mut **b);
            }
            AstNodeVariant::Negate { x } |
            AstNodeVariant::Not { x } => {
                visit_node!(&mut **x);
            }
            AstNodeVariant::Module { path: _ } => {}
            AstNodeVariant::ModuleAccess { path } => {
                let first_segment = path.segments[0];
                let mut last_usage = None;
                for usage in &self.usages {
                    if usage.segments[usage.segments.len() - 1] != first_segment { continue; }
                    last_usage = Some(usage);
                }
                if let Some(usage) = last_usage {
                    let mut new_path_segments = usage.segments.clone();
                    new_path_segments.append(&mut path.segments[1..].into());
                    *path = NamespacePath::new(new_path_segments);
                }
                let module_name = NamespacePath::new(path.get_segments()[0..path.get_segments().len() - 1].into());
                let accessed_name = path.get_segments()[path.get_segments().len() - 1];
                let module = if let Some(module) = modules.get(&module_name) { Some(module) }
                    else if module_name == self.path { Some(self) }
                    else { errors.push(Error::new([
                        ErrorSection::Error(ErrorType::ModuleDoesNotExist(module_name.display(strings))),
                        ErrorSection::Code(node_source)
                    ].into())); None };
                if let Some(module) = module {
                    if let Some(is_public) = module.exported.get(&accessed_name) {
                        if !is_public && module.symbols.get(&accessed_name).expect("symbol should exist").source().file_name() != node_source.file_name() { errors.push(Error::new([
                            ErrorSection::Error(ErrorType::SymbolIsNotPublic(path.display(strings))),
                            ErrorSection::Code(node_source)
                        ].into())); }
                    } else { errors.push(Error::new([
                        ErrorSection::Error(ErrorType::SymbolDoesNotExist(path.display(strings))),
                        ErrorSection::Code(node_source)
                    ].into())); }
                }
            }
            AstNodeVariant::Use { paths: _ } => {}
        }
        errors
    }
}