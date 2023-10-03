use crate::compiler::strings::{StringIdx, StringMap};

#[derive(Debug, Clone)]
pub enum AstNode {
    Procedure { name: StringIdx, arguments: Vec<StringIdx>, body: Vec<AstNode> },
    Function { arguments: Vec<StringIdx>, body: Vec<AstNode> },
    Variable { name: StringIdx, mutable: bool, value: Box<AstNode> },
    CaseBranches { value: Box<AstNode>, branches: Vec<(AstNode, Vec<AstNode>)> },
    CaseConditon { condition: Box<AstNode>, body: Vec<AstNode> },
    Assignment { variable: Box<AstNode>, value: Box<AstNode> },
    Return { value: Option<Box<AstNode>> },
    Call { called: Box<AstNode>, arguments: Vec<AstNode> },
    Object { values: Vec<(StringIdx, AstNode)> },
    Array { values: Vec<AstNode> },
    ObjectAccess { object: Box<AstNode>, member: StringIdx },
    ArrayAccess { array: Box<AstNode>, index: Box<AstNode> },
    VariableAccess { name: StringIdx },
    IntegerLiteral { value: u64 },
    FractionLiteral { value: f64 },
    StringLiteral { value: StringIdx },
    UnitLiteral
}

fn indent(input: String, amount: usize) -> String {
    input.replace("\n", &format!("\n{}", " ".repeat(amount)))
}

impl AstNode {
    pub fn to_string(&self, strings: &StringMap) -> String {
        match self {
            AstNode::Procedure { name, arguments, body } =>
                format!("Procedure\n  name = '{}'\n  arguments = [{}]\n  body = \n    {}",
                    strings.get(*name),
                    arguments.iter().map(|s| strings.get(*s).to_string()).collect::<Vec<String>>().join(", "),
                    indent(body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNode::Function { arguments, body } =>
                format!("Function\n  arguments = [{}]\n  body = \n    {}",
                    arguments.iter().map(|s| strings.get(*s).to_string()).collect::<Vec<String>>().join(", "),
                    indent(body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNode::Variable { name, mutable, value } => 
                format!("Variable\n  name = '{}'\n  mutable = {}\n  value = \n    {}",
                    strings.get(*name),
                    mutable,
                    indent(value.to_string(strings), 4)
                ),
            AstNode::CaseBranches { value, branches } =>
                format!("CaseBranches\n  value = \n    {}\n  branches = \n    {}",
                    indent(value.to_string(strings), 4),
                    indent(branches.iter().map(|(branch_value, branch_body)| format!("branch\n  value = \n    {}\n  body = \n    {}",
                        indent(branch_value.to_string(strings), 4),
                        indent(branch_body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                    )).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNode::CaseConditon { condition, body } =>
                format!("CaseConditon\n  condition = \n    {}\n  body = \n    {}",
                    indent(condition.to_string(strings), 4),
                    indent(body.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNode::Assignment { variable, value } => 
                format!("Assignment\n  variable = \n    {}\n  value = \n    {}",
                    indent(variable.to_string(strings), 4),
                    indent(value.to_string(strings), 4)
                ),
            AstNode::Return { value } =>
                format!("Return\n  value = \n    {}",
                    indent(value.as_ref().map(|v| v.to_string(strings)).unwrap_or(String::from("<none>")), 4)
                ),
            AstNode::Call { called, arguments } =>
                format!("Call\n  called = \n    {}\n  arguments = \n    {}",
                    indent(called.to_string(strings), 4),
                    indent(arguments.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNode::Object { values } => 
                format!("Object\n  members = \n    {}",
                    indent(values.iter().map(|(member_name, member_value)| format!("member\n  name = '{}'\n  value = \n    {}",
                        strings.get(*member_name),
                        indent(member_value.to_string(strings), 4),
                    )).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNode::Array { values } =>
                format!("Array\n  values = \n    {}",
                    indent(values.iter().map(|n| n.to_string(strings)).collect::<Vec<String>>().join("\n"), 4)
                ),
            AstNode::ObjectAccess { object, member } => 
                format!("ObjectAccess\n  object = \n    {}\n  member = '{}'",
                    indent(object.to_string(strings), 4),
                    strings.get(*member)
                ),
            AstNode::ArrayAccess { array, index } =>
                format!("ArrayAccess\n  array = \n    {}\n  index = \n    {}",
                    indent(array.to_string(strings), 4),
                    indent(index.to_string(strings), 4)
                ),
            AstNode::VariableAccess { name } => format!("VariableAccess\n  name = '{}'", strings.get(*name)),
            AstNode::IntegerLiteral { value } => format!("IntegerLiteral\n  value = {}", value),
            AstNode::FractionLiteral { value } => format!("FractionLiteral\n  value = {}", value),
            AstNode::StringLiteral { value } => format!("StringLiteral\n  value = '{}'", strings.get(*value)),
            AstNode::UnitLiteral => format!("Unit")
        }
    }
}

