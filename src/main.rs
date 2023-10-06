
#![allow(dead_code)]

mod util;
mod compiler;

use util::strings::StringMap;
use compiler::{lexer::Lexer, parser::Parser, ast::HasAstNodeVariant, grammar::{check_grammar, ScopeType}};

fn main() {
    let mut strings = StringMap::new();
    let mut lexer = Lexer::new(strings.insert("test.gera"), strings.insert(r#"

var τ = 6.28318

proc as_radians degrees {
    return degrees
        | * τ
        | / 360.0
}

"#), &mut strings);

    let mut parser = if let Some(parser) = Parser::new(&mut strings, &mut lexer) { parser } else { return };
    let tree = match parser.parse_block(&mut strings, &mut lexer) {
        Ok(tree) => tree,
        Err(error) => {
            println!("{}", error.display(&strings));
            return;
        }
    };

    for node in &tree { println!("{}", node.node_variant().to_string(&strings)); }
    println!();

    let mut grammar_errors = Vec::new();
    check_grammar(&tree, ScopeType::GlobalStatement, &mut grammar_errors);
    if grammar_errors.len() > 0 {
        for error in grammar_errors {
            println!("{}", error.display(&strings));
        }
        return;
    }
}

