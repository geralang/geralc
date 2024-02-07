
use std::collections::HashSet;

use crate::util::{strings::{StringIdx, StringMap}, source::{SourceRange, HasSource}};
use crate::frontend::{modules::NamespacePath, types::TypeGroup};

#[derive(Debug, Clone, PartialEq)]
pub struct AstNode {
    variant: AstNodeVariant<AstNode>,
    source: SourceRange
}

impl AstNode {
    pub fn new(variant: AstNodeVariant<AstNode>, source: SourceRange) -> AstNode {
        AstNode {
            variant,
            source
        }
    }

    pub fn replace_source(&mut self, new: SourceRange) { self.source = new; }
}

impl HasAstNodeVariant<AstNode> for AstNode {
    fn node_variant(&self) -> &AstNodeVariant<AstNode> { &self.variant }
    fn node_variant_mut(&mut self) -> &mut AstNodeVariant<AstNode> { &mut self.variant }
    fn move_node(self) -> AstNodeVariant<AstNode> { self.variant }
}

impl HasSource for AstNode {
    fn source(&self) -> SourceRange { self.source }
}


#[derive(Debug, Clone, PartialEq)]
pub struct TypedAstNode {
    variant: AstNodeVariant<TypedAstNode>,
    node_type: TypeGroup,
    source: SourceRange
}

impl TypedAstNode {
    pub fn new(variant: AstNodeVariant<TypedAstNode>, node_type: TypeGroup, source: SourceRange) -> TypedAstNode {
        TypedAstNode {
            variant,
            node_type,
            source
        }
    }

    pub fn replace_source(&mut self, new: SourceRange) { self.source = new; }
    pub fn get_types(&self) -> TypeGroup { self.node_type }
    pub fn get_types_mut(&mut self) -> &mut TypeGroup { &mut self.node_type }
}

impl HasAstNodeVariant<TypedAstNode> for TypedAstNode {
    fn node_variant(&self) -> &AstNodeVariant<TypedAstNode> { &self.variant }
    fn node_variant_mut(&mut self) -> &mut AstNodeVariant<TypedAstNode> { &mut self.variant}
    fn move_node(self) -> AstNodeVariant<TypedAstNode> { self.variant }
}

impl HasSource for TypedAstNode {
    fn source(&self) -> SourceRange { self.source }
}


pub trait HasAstNodeVariant<T: Clone + HasAstNodeVariant<T>> {
    fn node_variant(&self) -> &AstNodeVariant<T>;
    fn node_variant_mut(&mut self) -> &mut AstNodeVariant<T>;
    fn move_node(self) -> AstNodeVariant<T>;
    fn to_string(&self, strings: &StringMap) -> String { self.node_variant().to_string(strings) }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstNodeVariant<T: Clone + HasAstNodeVariant<T>> {
    Procedure { public: bool, name: StringIdx, arguments: Vec<(StringIdx, SourceRange)>, body: Vec<T> },
    Function { arguments: Vec<(StringIdx, SourceRange)>, captures: Option<HashSet<StringIdx>>, body: Vec<T> },
    Variable { public: bool, mutable: bool, name: StringIdx, value_types: Option<TypeGroup>, value: Option<Box<T>> },
    CaseBranches { value: Box<T>, branches: Vec<(T, Vec<T>)>, else_body: Vec<T> },
    CaseConditon { condition: Box<T>, body: Vec<T>, else_body: Vec<T> },
    CaseVariant { value: Box<T>, branches: Vec<(StringIdx, Option<(StringIdx, SourceRange, Option<TypeGroup>)>, Vec<T>)>, else_body: Option<Vec<T>> },
    Assignment { variable: Box<T>, value: Box<T> },
    Return { value: Box<T> },
    Call { called: Box<T>, arguments: Vec<T> },
    Object { values: Vec<(StringIdx, T)> },
    Array { values: Vec<T> },
    ObjectAccess { object: Box<T>, member: StringIdx },
    ArrayAccess { array: Box<T>, index: Box<T> },
    VariableAccess { name: StringIdx },
    BooleanLiteral { value: bool },
    IntegerLiteral { value: i64 },
    FloatLiteral { value: f64 },
    StringLiteral { value: StringIdx },
    UnitLiteral,
    Add { a: Box<T>, b: Box<T> },
    Subtract { a: Box<T>, b: Box<T> },
    Multiply { a: Box<T>, b: Box<T> },
    Divide { a: Box<T>, b: Box<T> },
    Modulo { a: Box<T>, b: Box<T> },
    Negate { x: Box<T> },
    LessThan { a: Box<T>, b: Box<T> },
    GreaterThan { a: Box<T>, b: Box<T> },
    LessThanEqual { a: Box<T>, b: Box<T> },
    GreaterThanEqual { a: Box<T>, b: Box<T> },
    Equals { a: Box<T>, b: Box<T> },
    NotEquals { a: Box<T>, b: Box<T> },
    Not { x: Box<T> },
    Or { a: Box<T>, b: Box<T> },
    And { a: Box<T>, b: Box<T> },
    Module { path: NamespacePath },
    ModuleAccess { path: NamespacePath },
    Use { paths: Vec<NamespacePath> },
    Variant { name: StringIdx, value: Box<T> },
    Static { value: Box<T> },
    Target { target: StringIdx, body: Vec<T> }
}

fn indent(input: String, amount: usize) -> String {
    input.replace("\n", &format!("\n{}", " ".repeat(amount)))
}

impl<T: Clone + HasAstNodeVariant<T>> AstNodeVariant<T> {
    pub fn to_string(&self, strings: &StringMap) -> String {
        match self {
            AstNodeVariant::Procedure { public, name, arguments, body } =>
                format!("Procedure\n  public = {}\n  name = '{}'\n  arguments = [{}]\n  body = \n    {}",
                    public,
                    strings.get(*name),
                    arguments.iter().map(|s| strings.get(s.0).to_string()).collect::<Vec<String>>().join(", "),
                    indent(body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNodeVariant::Function { arguments, captures, body } =>
                format!("Function\n  arguments = [{}]\n  captures = [{}]\nbody = \n    {}",
                    arguments.iter().map(|s| strings.get(s.0).to_string()).collect::<Vec<String>>().join(", "),
                    captures.as_ref().map(|c| c.iter().map(|s| strings.get(*s).to_string()).collect::<Vec<String>>().join(", ")).unwrap_or("<undefined>".into()),
                    indent(body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNodeVariant::Variable { public, mutable, name, value_types: _, value } => 
                format!("Variable\n  public = {}\n  mutable = {}\n  name = '{}'\n  value = \n    {}",
                    public,
                    mutable,
                    strings.get(*name),
                    indent(value.as_ref().map(|n| n.to_string(strings)).unwrap_or(String::from("<none>")), 4)
                ),
            AstNodeVariant::CaseBranches { value, branches, else_body } =>
                format!("CaseBranches\n  value = \n    {}\n  branches = \n    {}\n  else_body = \n    {}",
                    indent(value.to_string(strings), 4),
                    indent(branches.iter().map(|(branch_value, branch_body)| format!("branch\n  value = \n    {}\n  body = \n    {}",
                        indent(branch_value.to_string(strings), 4),
                        indent(branch_body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                    )).collect::<Vec<String>>().join("\n"), 4),
                    indent(else_body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNodeVariant::CaseConditon { condition, body, else_body } =>
                format!("CaseConditon\n  condition = \n    {}\n  body = \n    {}\n  else_body = \n    {}",
                    indent(condition.to_string(strings), 4),
                    indent(body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4),
                    indent(else_body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNodeVariant::CaseVariant { value, branches, else_body } =>
                format!("CaseVariant\n  condition = \n    {}\n  body = \n    {}\n  else_body = \n    {}",
                    indent(value.to_string(strings), 4),
                    indent(branches.iter().map(|(variant_name, variable, branch_body)| format!("branch\n  variant_name = '{}'\n  variant_variable = {}\n  body = \n    {}",
                        strings.get(*variant_name),
                        variable.as_ref().map(|v| format!("'{}'", strings.get(v.0))).unwrap_or(String::from("<none>")),
                        indent(branch_body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                    )).collect::<Vec<String>>().join("\n"), 4),
                    indent(else_body.as_ref().map(|else_body| else_body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n")).unwrap_or(String::from("<none>")), 4)
                ),
            AstNodeVariant::Assignment { variable, value } => 
                format!("Assignment\n  variable = \n    {}\n  value = \n    {}",
                    indent(variable.to_string(strings), 4),
                    indent(value.to_string(strings), 4)
                ),
            AstNodeVariant::Return { value } =>
                format!("Return\n  value = \n    {}",
                    indent(value.to_string(strings), 4)
                ),
            AstNodeVariant::Call { called, arguments } =>
                format!("Call\n  called = \n    {}\n  arguments = \n    {}",
                    indent(called.to_string(strings), 4),
                    indent(arguments.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNodeVariant::Object { values } => 
                format!("Object\n  members = \n    {}",
                    indent(values.iter().map(|(member_name, member_value)| format!("member\n  name = '{}'\n  value = \n    {}",
                        strings.get(*member_name),
                        indent(member_value.to_string(strings), 4),
                    )).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNodeVariant::Array { values } =>
                format!("Array\n  values = \n    {}",
                    indent(values.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNodeVariant::ObjectAccess { object, member } => 
                format!("ObjectAccess\n  object = \n    {}\n  member = '{}'",
                    indent(object.to_string(strings), 4),
                    strings.get(*member)
                ),
            AstNodeVariant::ArrayAccess { array, index } =>
                format!("ArrayAccess\n  array = \n    {}\n  index = \n    {}",
                    indent(array.to_string(strings), 4),
                    indent(index.to_string(strings), 4)
                ),
            AstNodeVariant::VariableAccess { name } => format!("VariableAccess\n  name = '{}'", strings.get(*name)),
            AstNodeVariant::BooleanLiteral { value } => format!("BooleanLiteral\n  value = {}", value),
            AstNodeVariant::IntegerLiteral { value } => format!("IntegerLiteral\n  value = {}", value),
            AstNodeVariant::FloatLiteral { value } => format!("FloatLiteral\n  value = {}", value),
            AstNodeVariant::StringLiteral { value } => format!("StringLiteral\n  value = '{}'", strings.get(*value)),
            AstNodeVariant::UnitLiteral => format!("Unit"),
            AstNodeVariant::Add { a, b } =>
                format!("Add\n  a = \n    {}\n  b = \n    {}",
                    indent(a.to_string(strings), 4),
                    indent(b.to_string(strings), 4)
                ),
            AstNodeVariant::Subtract { a, b } =>
                format!("Subtract\n  a = \n    {}\n  b = \n    {}",
                    indent(a.to_string(strings), 4),
                    indent(b.to_string(strings), 4)
                ),
            AstNodeVariant::Multiply { a, b } =>
                format!("Multiply\n  a = \n    {}\n  b = \n    {}",
                    indent(a.to_string(strings), 4),
                    indent(b.to_string(strings), 4)
                ),
            AstNodeVariant::Divide { a, b } =>
                format!("Divide\n  a = \n    {}\n  b = \n    {}",
                    indent(a.to_string(strings), 4),
                    indent(b.to_string(strings), 4)
                ),
            AstNodeVariant::Modulo { a, b } =>
                format!("Modulo\n  a = \n    {}\n  b = \n    {}",
                    indent(a.to_string(strings), 4),
                    indent(b.to_string(strings), 4)
                ),
            AstNodeVariant::Negate { x } =>
                format!("Negate\n  x = \n    {}",
                    indent(x.to_string(strings), 4),
                ),
            AstNodeVariant::LessThan { a, b } =>
                format!("LessThan\n  a = \n    {}\n  b = \n    {}",
                    indent(a.to_string(strings), 4),
                    indent(b.to_string(strings), 4)
                ),
            AstNodeVariant::GreaterThan { a, b } =>
                format!("GreaterThan\n  a = \n    {}\n  b = \n    {}",
                    indent(a.to_string(strings), 4),
                    indent(b.to_string(strings), 4)
                ),
            AstNodeVariant::LessThanEqual { a, b } =>
                format!("LessThanEqual\n  a = \n    {}\n  b = \n    {}",
                    indent(a.to_string(strings), 4),
                    indent(b.to_string(strings), 4)
                ),
            AstNodeVariant::GreaterThanEqual { a, b } =>
                format!("GreaterThanEqual\n  a = \n    {}\n  b = \n    {}",
                    indent(a.to_string(strings), 4),
                    indent(b.to_string(strings), 4)
                ),
            AstNodeVariant::Equals { a, b } =>
                format!("Equals\n  a = \n    {}\n  b = \n    {}",
                    indent(a.to_string(strings), 4),
                    indent(b.to_string(strings), 4)
                ),
            AstNodeVariant::NotEquals { a, b } =>
                format!("NotEquals\n  a = \n    {}\n  b = \n    {}",
                    indent(a.to_string(strings), 4),
                    indent(b.to_string(strings), 4)
                ),
            AstNodeVariant::Not { x } =>
                format!("Not\n  x = \n    {}",
                    indent(x.to_string(strings), 4),
                ),
            AstNodeVariant::Or { a, b } =>
                format!("Or\n  a = \n    {}\n  b = \n    {}",
                    indent(a.to_string(strings), 4),
                    indent(b.to_string(strings), 4)
                ),
            AstNodeVariant::And { a, b } =>
                format!("And\n  a = \n    {}\n  b = \n    {}",
                    indent(a.to_string(strings), 4),
                    indent(b.to_string(strings), 4)
                ),
            AstNodeVariant::Module { path } =>
                format!("Module\n  path = {}",
                    path.display(strings)
                ),
            AstNodeVariant::ModuleAccess { path } =>
                format!("ModuleAccess\n  path = {}",
                    path.display(strings)
                ),
            AstNodeVariant::Use { paths } =>
                format!("Use\n  paths = \n    {}",
                    indent(paths.iter().map(|p| p.display(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNodeVariant::Variant { name, value } => 
                format!("Variant\n  name = '{}'\n  value = \n    {}",
                    strings.get(*name),
                    indent(value.to_string(strings), 4)
                ),
            AstNodeVariant::Static { value } =>
                format!("Static\n  value = \n    {}",
                    indent(value.to_string(strings), 4),
                ),
            AstNodeVariant::Target { target, body } =>
                format!("Target\n  target = {}\n  body = \n    {}",
                    strings.get(*target),
                    indent(body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
        }
    }
}