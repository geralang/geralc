
use std::collections::HashMap;

use crate::frontend::{
    ast::{AstNode, TypedAstNode},
    modules::{NamespacePath, Module},
    types::TypeScope,
    type_checking::Symbol
};
use crate::util::strings::{StringMap, StringIdx};
use crate::backend::ir::{IrSymbol, IrTypeBank};

pub enum CompileTarget {
    AstConsumer(fn(TypeScope, HashMap<NamespacePath, Module<AstNode>>, HashMap<NamespacePath, StringIdx>, &mut StringMap) -> String),
    TypedAstConsumer(fn(TypeScope, HashMap<NamespacePath, Symbol<TypedAstNode>>, HashMap<NamespacePath, StringIdx>, &mut StringMap) -> String),
    IrConsumer(fn(Vec<IrSymbol>, IrTypeBank, NamespacePath, &mut StringMap) -> String)
}