use crate::util::{
    error::{Error, ErrorSection, ErrorType},
    source::HasSource
};
use crate::compiler::ast::{AstNode, HasAstNodeVariant, AstNodeVariant};


#[derive(Clone, Copy)]
pub enum ScopeType {
    GlobalStatement,
    Statement,
    Expression,
    Variable
}

impl ScopeType {
    fn index(&self) -> u8 {
        match self {
            ScopeType::GlobalStatement => 3,
            ScopeType::Statement => 2,
            ScopeType::Expression => 1,
            ScopeType::Variable => 0
        }
    }
    fn description(&self) -> &'static str {
        match self {
            ScopeType::GlobalStatement => "a statement in the global scope",
            ScopeType::Statement => "a statement",
            ScopeType::Expression => "an expression",
            ScopeType::Variable => "a variable"
        }
    }
}


pub fn check_grammar(nodes: &[AstNode], scope: ScopeType, errors: &mut Vec<Error>) {
    for node in nodes {
        check_grammar_singular(node, scope, errors);
    }
}

fn check_grammar_singular(node: &AstNode, scope: ScopeType, errors: &mut Vec<Error>) {
    macro_rules! enforce_min_scope {
        ($thing: expr, $enforced_scope: expr) => {
            if scope.index() < ($enforced_scope).index() {
                errors.push(Error::new([
                    ErrorSection::Error(ErrorType::InvalidContext($thing, ($enforced_scope).description(), scope.description())),
                    ErrorSection::Code(node.source().clone())
                ].into()));
            }
        };
    }
    macro_rules! enforce_max_scope {
        ($thing: expr, $enforced_scope: expr, $displayed_scope: expr) => {
            if scope.index() > ($enforced_scope).index() {
                errors.push(Error::new([
                    ErrorSection::Error(ErrorType::InvalidContext($thing, ($displayed_scope).description(), scope.description())),
                    ErrorSection::Code(node.source().clone())
                ].into()));
            }
        };
    }
    match node.node_variant() {
        AstNodeVariant::Procedure { public: _, name: _, arguments: _, body } => {
            enforce_min_scope!("'proc'", ScopeType::GlobalStatement);
            check_grammar(body, ScopeType::Statement, errors);
        },
        AstNodeVariant::Function { arguments: _, body } => {
            enforce_min_scope!("'func'", ScopeType::Expression);
            enforce_max_scope!("'func'", ScopeType::Statement, ScopeType::Expression);
            check_grammar(body, ScopeType::Statement, errors);
        },
        AstNodeVariant::Variable { public, mutable, name: _, value } => {
            enforce_min_scope!(match (*public, *mutable) {
                (true, true) => "'pub mut var'",
                (true, false) => "'pub var'",
                (false, true) => "'mut var'",
                (false, false) => "'var'"
            }, ScopeType::Statement);
            if *public { enforce_min_scope!("'pub var'", ScopeType::GlobalStatement); }
            if *mutable { enforce_max_scope!("'mut var'", ScopeType::Statement, ScopeType::Statement); }
            check_grammar_singular(&*value, ScopeType::Expression, errors);
        },
        AstNodeVariant::CaseBranches { value, branches, else_body } => {
            enforce_min_scope!("'case'", ScopeType::Statement);
            enforce_max_scope!("'case'", ScopeType::Statement, ScopeType::Statement);
            check_grammar_singular(&*value, ScopeType::Expression, errors);
            for branch in branches {
                check_grammar_singular(&branch.0, ScopeType::Expression, errors);
                check_grammar(&branch.1, ScopeType::Statement, errors);
            }
            check_grammar(else_body, ScopeType::Statement, errors);
        },
        AstNodeVariant::CaseConditon { condition, body, else_body } => {
            enforce_min_scope!("'case'", ScopeType::Statement);
            enforce_max_scope!("'case'", ScopeType::Statement, ScopeType::Statement);
            check_grammar_singular(&*condition, ScopeType::Expression, errors);
            check_grammar(body, ScopeType::Statement, errors);
            check_grammar(else_body, ScopeType::Statement, errors);
        },
        AstNodeVariant::Assignment { variable, value } => {
            enforce_min_scope!("Assignments", ScopeType::Statement);
            enforce_max_scope!("Assignments", ScopeType::Statement, ScopeType::Statement);
            check_grammar_singular(&*variable, ScopeType::Variable, errors);
            check_grammar_singular(&*value, ScopeType::Expression, errors);
        },
        AstNodeVariant::Return { value } => {
            enforce_min_scope!("'return'", ScopeType::Statement);
            enforce_max_scope!("'return'", ScopeType::Statement, ScopeType::Statement);
            check_grammar_singular(&*value, ScopeType::Expression, errors);
        },
        AstNodeVariant::Call { called, arguments } => {
            enforce_min_scope!("Calls", ScopeType::Expression);
            enforce_max_scope!("Calls", ScopeType::Statement, ScopeType::Expression);
            check_grammar_singular(&*called, ScopeType::Expression, errors);
            check_grammar(arguments, ScopeType::Expression, errors);
        },
        AstNodeVariant::Object { values } => {
            enforce_min_scope!("Object literals", ScopeType::Expression);
            enforce_max_scope!("Object literals", ScopeType::Statement, ScopeType::Expression);
            for value in values {
                check_grammar_singular(&value.1, ScopeType::Expression, errors);
            }
        },
        AstNodeVariant::Array { values } => {
            enforce_min_scope!("Array literals", ScopeType::Expression);
            enforce_max_scope!("Array literals", ScopeType::Statement, ScopeType::Expression);
            check_grammar(values, ScopeType::Expression, errors);
        },
        AstNodeVariant::ObjectAccess { object, member: _ } => {
            enforce_min_scope!("Object accesses", ScopeType::Variable);
            enforce_max_scope!("Object accesses", ScopeType::Statement, ScopeType::Expression);
            check_grammar_singular(&*object, ScopeType::Expression, errors);
        },
        AstNodeVariant::ArrayAccess { array, index } => {
            enforce_min_scope!("Object accesses", ScopeType::Variable);
            enforce_max_scope!("Object accesses", ScopeType::Statement, ScopeType::Expression);
            check_grammar_singular(&*array, ScopeType::Expression, errors);
            check_grammar_singular(&*index, ScopeType::Expression, errors);
        },
        AstNodeVariant::VariableAccess { name: _ } => {
            enforce_min_scope!("Variables", ScopeType::Variable);
            enforce_max_scope!("Variables", ScopeType::Statement, ScopeType::Expression);
        },
        AstNodeVariant::BooleanLiteral { value: _ } => {
            enforce_min_scope!("Boolean literals", ScopeType::Expression);
            enforce_max_scope!("Boolean literals", ScopeType::Statement, ScopeType::Expression);
        },
        AstNodeVariant::IntegerLiteral { value: _ } => {
            enforce_min_scope!("Integer literals", ScopeType::Expression);
            enforce_max_scope!("Integer literals", ScopeType::Statement, ScopeType::Expression);
        },
        AstNodeVariant::FloatLiteral { value: _ } => {
            enforce_min_scope!("Float literals", ScopeType::Expression);
            enforce_max_scope!("Float literals", ScopeType::Statement, ScopeType::Expression);
        },
        AstNodeVariant::StringLiteral { value: _ } => {
            enforce_min_scope!("String literals", ScopeType::Expression);
            enforce_max_scope!("String literals", ScopeType::Statement, ScopeType::Expression);
        },
        AstNodeVariant::UnitLiteral => {
            enforce_min_scope!("Unit value literals", ScopeType::Expression);
            enforce_max_scope!("Unit value literals", ScopeType::Statement, ScopeType::Expression);
        },
        AstNodeVariant::Add { a, b } |
        AstNodeVariant::Subtract { a, b } |
        AstNodeVariant::Multiply { a, b } |
        AstNodeVariant::Divide { a, b } |
        AstNodeVariant::Modulo { a, b } => {
            enforce_min_scope!("Arithmetic operations", ScopeType::Expression);
            enforce_max_scope!("Arithmetic operations", ScopeType::Statement, ScopeType::Expression);
            check_grammar_singular(&*a, ScopeType::Expression, errors);
            check_grammar_singular(&*b, ScopeType::Expression, errors);
        }
        AstNodeVariant::Negate { x } => {
            enforce_min_scope!("Arithmetic operations", ScopeType::Expression);
            enforce_max_scope!("Arithmetic operations", ScopeType::Statement, ScopeType::Expression);
            check_grammar_singular(&*x, ScopeType::Expression, errors);
        }
        AstNodeVariant::LessThan { a, b } |
        AstNodeVariant::LessThanEqual { a , b } |
        AstNodeVariant::GreaterThan { a, b } |
        AstNodeVariant::GreaterThanEqual { a, b } |
        AstNodeVariant::Equals { a, b } |
        AstNodeVariant::NotEquals { a, b } => {
            enforce_min_scope!("Comparative operations", ScopeType::Expression);
            enforce_max_scope!("Comparative operations", ScopeType::Statement, ScopeType::Expression);
            check_grammar_singular(&*a, ScopeType::Expression, errors);
            check_grammar_singular(&*b, ScopeType::Expression, errors);
        }
        AstNodeVariant::And { a, b } |
        AstNodeVariant::Or { a, b } => {
            enforce_min_scope!("Logical operations", ScopeType::Expression);
            enforce_max_scope!("Logical operations", ScopeType::Statement, ScopeType::Expression);
            check_grammar_singular(&*a, ScopeType::Expression, errors);
            check_grammar_singular(&*b, ScopeType::Expression, errors);
        }
        AstNodeVariant::Not { x } => {
            enforce_min_scope!("Logical operations", ScopeType::Expression);
            enforce_max_scope!("Logical operations", ScopeType::Statement, ScopeType::Expression);
            check_grammar_singular(&*x, ScopeType::Expression, errors);
        }
        AstNodeVariant::Module { path: _ } => {
            enforce_min_scope!("'mod'", ScopeType::GlobalStatement);
        }
        AstNodeVariant::ModuleAccess { path: _ } => {
            enforce_min_scope!("Variables", ScopeType::Variable);
            enforce_max_scope!("Variables", ScopeType::Statement, ScopeType::Expression);
        }
        AstNodeVariant::Use { paths: _ } => {
            enforce_min_scope!("'mod'", ScopeType::GlobalStatement);
        }
    }
}