
use crate::util::strings::StringMap;
use crate::frontend::ast::{AstNode, HasAstNodeVariant, AstNodeVariant};

pub fn process_target_blocks(nodes: &mut Vec<AstNode>, target_str: &str, strings: &StringMap) {
    let mut idx = 0;
    while idx < nodes.len() {
        let mut opt_node = Some(&mut nodes[idx]);
        let mut inserted_nodes = None;
        process_node(&mut opt_node, &mut inserted_nodes, target_str, strings);
        if let Some(_) = opt_node {
            idx += 1;
        } else {
            nodes.remove(idx);
        }
        if let Some(inserted_nodes) = inserted_nodes {
            for inserted_node in inserted_nodes.into_iter().rev() {
                nodes.insert(idx, inserted_node);
            }
        }
    }
} 

fn process_node(opt_node: &mut Option<&mut AstNode>, inserted_nodes: &mut Option<Vec<AstNode>>, target_str: &str, strings: &StringMap) {
    if let Some(node) = opt_node {
        match node.node_variant_mut() {
            AstNodeVariant::Procedure { public: _, name: _, arguments: _, body } => {
                process_target_blocks(body, target_str, strings);
            },
            AstNodeVariant::Function { arguments: _, body } => {
                process_target_blocks(body, target_str, strings);
            },
            AstNodeVariant::Variable { public: _, mutable: _, name: _, value_types: _, value } => {
                if let Some(value) = value {
                    process_node(&mut Some(&mut *value), &mut None, target_str, strings);
                }
            },
            AstNodeVariant::CaseBranches { value, branches, else_body } => {
                process_node(&mut Some(&mut *value), &mut None, target_str, strings);
                for branch in branches {
                    process_node(&mut Some(&mut branch.0), &mut None, target_str, strings);
                    process_target_blocks(&mut branch.1, target_str, strings);
                }
                process_target_blocks(else_body, target_str, strings);
            },
            AstNodeVariant::CaseConditon { condition, body, else_body } => {
                process_node(&mut Some(&mut *condition), &mut None, target_str, strings);
                process_target_blocks(body, target_str, strings);
                process_target_blocks(else_body, target_str, strings);
            },
            AstNodeVariant::CaseVariant { value, branches, else_body } => {
                process_node(&mut Some(&mut *value), &mut None, target_str, strings);
                for branch in branches {
                    process_target_blocks(&mut branch.2, target_str, strings);
                }
                if let Some(else_body) = else_body {
                    process_target_blocks(else_body, target_str, strings);
                }
            },
            AstNodeVariant::Assignment { variable, value } => {
                process_node(&mut Some(&mut *variable), &mut None, target_str, strings);
                process_node(&mut Some(&mut *value), &mut None, target_str, strings);
            },
            AstNodeVariant::Return { value } => {
                process_node(&mut Some(&mut *value), &mut None, target_str, strings);
            },
            AstNodeVariant::Call { called, arguments } => {
                process_node(&mut Some(&mut *called), &mut None, target_str, strings);
                process_target_blocks(arguments, target_str, strings);
            },
            AstNodeVariant::Object { values } => {
                for value in values {
                    process_node(&mut Some(&mut value.1), &mut None, target_str, strings);
                }
            },
            AstNodeVariant::Array { values } => {
                process_target_blocks(values, target_str, strings);
            },
            AstNodeVariant::ObjectAccess { object, member: _ } => {
                process_node(&mut Some(&mut *object), &mut None, target_str, strings);
            },
            AstNodeVariant::ArrayAccess { array, index } => {
                process_node(&mut Some(&mut *array), &mut None, target_str, strings);
                process_node(&mut Some(&mut *index), &mut None, target_str, strings);
            },
            AstNodeVariant::VariableAccess { name: _ } |
            AstNodeVariant::BooleanLiteral { value: _ } |
            AstNodeVariant::IntegerLiteral { value: _ } |
            AstNodeVariant::FloatLiteral { value: _ } |
            AstNodeVariant::StringLiteral { value: _ } |
            AstNodeVariant::UnitLiteral => {}
            AstNodeVariant::Add { a, b } |
            AstNodeVariant::Subtract { a, b } |
            AstNodeVariant::Multiply { a, b } |
            AstNodeVariant::Divide { a, b } |
            AstNodeVariant::Modulo { a, b } => {
                process_node(&mut Some(&mut *a), &mut None, target_str, strings);
                process_node(&mut Some(&mut *b), &mut None, target_str, strings);
            }
            AstNodeVariant::Negate { x } => {
                process_node(&mut Some(&mut *x), &mut None, target_str, strings);
            }
            AstNodeVariant::LessThan { a, b } |
            AstNodeVariant::LessThanEqual { a , b } |
            AstNodeVariant::GreaterThan { a, b } |
            AstNodeVariant::GreaterThanEqual { a, b } |
            AstNodeVariant::Equals { a, b } |
            AstNodeVariant::NotEquals { a, b } => {
                process_node(&mut Some(&mut *a), &mut None, target_str, strings);
                process_node(&mut Some(&mut *b), &mut None, target_str, strings);
            }
            AstNodeVariant::And { a, b } |
            AstNodeVariant::Or { a, b } => {
                process_node(&mut Some(&mut *a), &mut None, target_str, strings);
                process_node(&mut Some(&mut *b), &mut None, target_str, strings);
            }
            AstNodeVariant::Not { x } => {
                process_node(&mut Some(&mut *x), &mut None, target_str, strings);
            }
            AstNodeVariant::Module { path: _ } |
            AstNodeVariant::ModuleAccess { path: _ } |
            AstNodeVariant::Use { paths: _ } => {}
            AstNodeVariant::Variant { name: _, value } => {
                process_node(&mut Some(&mut *value), &mut None, target_str, strings);
            }
            AstNodeVariant::Static { value } => {
                process_node(&mut Some(&mut *value), &mut None, target_str, strings);
            }
            AstNodeVariant::Target { target, body } => {
                if strings.get(*target) == target_str {
                    let mut i = Vec::new();
                    i.append(body);
                    *inserted_nodes = Some(i);
                }
                *opt_node = None;
            }
        }
    }
}