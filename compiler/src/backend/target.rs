
use std::collections::HashMap;

use crate::frontend::{
    ast::{AstNode, TypedAstNode},
    modules::{NamespacePath, Module},
    types::TypeMap,
    type_checking::Symbol
};
use crate::util::strings::{StringMap, StringIdx};
use crate::backend::{
    ir::IrSymbol,
    constants::ConstantPool
};

pub enum CompileTarget {
    AstConsumer(fn(TypeMap, HashMap<NamespacePath, Module<AstNode>>, HashMap<NamespacePath, StringIdx>, &mut StringMap, usize) -> String),
    TypedAstConsumer(fn(TypeMap, HashMap<NamespacePath, Symbol<TypedAstNode>>, HashMap<NamespacePath, StringIdx>, &mut StringMap, usize) -> String),
    IrConsumer(fn(Vec<IrSymbol>, TypeMap, ConstantPool, NamespacePath, &mut StringMap, usize) -> String)
}