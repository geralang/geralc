
use crate::util::{strings::{StringIdx, StringMap}, source::SourceRange};

#[derive(Clone)]
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

    pub fn source(&self) -> &SourceRange {
        &self.source
    }
    pub fn node_variant_mut(&mut self) -> &mut AstNodeVariant<AstNode> { &mut self.variant }
}

impl HasAstNodeVariant<AstNode> for AstNode {
    fn node_variant(&self) -> &AstNodeVariant<AstNode> {
        &self.variant
    }
}

pub trait HasAstNodeVariant<T: Clone + HasAstNodeVariant<T>> {
    fn node_variant(&self) -> &AstNodeVariant<T>;
    fn to_string(&self, strings: &StringMap) -> String { self.node_variant().to_string(strings) }
}

#[derive(Clone)]
pub enum AstNodeVariant<T: Clone + HasAstNodeVariant<T>> {
    Procedure { name: StringIdx, arguments: Vec<StringIdx>, body: Vec<T> },
    Function { arguments: Vec<StringIdx>, body: Vec<T> },
    Variable { name: StringIdx, mutable: bool, value: Box<T> },
    CaseBranches { value: Box<T>, branches: Vec<(T, Vec<T>)> },
    CaseConditon { condition: Box<T>, body: Vec<T> },
    Assignment { variable: Box<T>, value: Box<T> },
    Return { value: Option<Box<T>> },
    Call { called: Box<T>, arguments: Vec<T> },
    Object { values: Vec<(StringIdx, T)> },
    Array { values: Vec<T> },
    ObjectAccess { object: Box<T>, member: StringIdx },
    ArrayAccess { array: Box<T>, index: Box<T> },
    VariableAccess { name: StringIdx },
    IntegerLiteral { value: u64 },
    FractionLiteral { value: f64 },
    StringLiteral { value: StringIdx },
    UnitLiteral
}

fn indent(input: String, amount: usize) -> String {
    input.replace("\n", &format!("\n{}", " ".repeat(amount)))
}

impl<T: Clone + HasAstNodeVariant<T>> AstNodeVariant<T> {
    pub fn to_string(&self, strings: &StringMap) -> String {
        match self {
            AstNodeVariant::Procedure { name, arguments, body } =>
                format!("Procedure\n  name = '{}'\n  arguments = [{}]\n  body = \n    {}",
                    strings.get(*name),
                    arguments.iter().map(|s| strings.get(*s).to_string()).collect::<Vec<String>>().join(", "),
                    indent(body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNodeVariant::Function { arguments, body } =>
                format!("Function\n  arguments = [{}]\n  body = \n    {}",
                    arguments.iter().map(|s| strings.get(*s).to_string()).collect::<Vec<String>>().join(", "),
                    indent(body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNodeVariant::Variable { name, mutable, value } => 
                format!("Variable\n  name = '{}'\n  mutable = {}\n  value = \n    {}",
                    strings.get(*name),
                    mutable,
                    indent(value.to_string(strings), 4)
                ),
            AstNodeVariant::CaseBranches { value, branches } =>
                format!("CaseBranches\n  value = \n    {}\n  branches = \n    {}",
                    indent(value.to_string(strings), 4),
                    indent(branches.iter().map(|(branch_value, branch_body)| format!("branch\n  value = \n    {}\n  body = \n    {}",
                        indent(branch_value.to_string(strings), 4),
                        indent(branch_body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                    )).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNodeVariant::CaseConditon { condition, body } =>
                format!("CaseConditon\n  condition = \n    {}\n  body = \n    {}",
                    indent(condition.to_string(strings), 4),
                    indent(body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNodeVariant::Assignment { variable, value } => 
                format!("Assignment\n  variable = \n    {}\n  value = \n    {}",
                    indent(variable.to_string(strings), 4),
                    indent(value.to_string(strings), 4)
                ),
            AstNodeVariant::Return { value } =>
                format!("Return\n  value = \n    {}",
                    indent(value.as_ref().map(|v| v.to_string(strings)).unwrap_or(String::from("<none>")), 4)
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
            AstNodeVariant::IntegerLiteral { value } => format!("IntegerLiteral\n  value = {}", value),
            AstNodeVariant::FractionLiteral { value } => format!("FractionLiteral\n  value = {}", value),
            AstNodeVariant::StringLiteral { value } => format!("StringLiteral\n  value = '{}'", strings.get(*value)),
            AstNodeVariant::UnitLiteral => format!("Unit")
        }
    }
}

