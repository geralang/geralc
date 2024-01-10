
use std::collections::HashMap;

use crate::frontend::{
    ast::{AstNode, TypedAstNode},
    modules::{NamespacePath, Module},
    types::TypeMap,
    type_checking::Symbol
};
use crate::util::strings::{StringMap, StringIdx};
use crate::backend::ir::IrSymbol;

pub enum CompileTarget {
    AstConsumer(fn(TypeMap, HashMap<NamespacePath, Module<AstNode>>, HashMap<NamespacePath, StringIdx>, &mut StringMap) -> String),
    TypedAstConsumer(fn(TypeMap, HashMap<NamespacePath, Symbol<TypedAstNode>>, HashMap<NamespacePath, StringIdx>, &mut StringMap) -> String),
    IrConsumer(fn(Vec<IrSymbol>, TypeMap, NamespacePath, &mut StringMap) -> String)
}