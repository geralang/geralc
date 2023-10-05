
#![allow(dead_code)]

mod util;
mod compiler;

use util::strings::StringMap;
use compiler::{lexer::Lexer, parser::Parser};

fn main() {
    let mut strings = StringMap::new();
    let mut lexer = Lexer::new(strings.insert("test.gera"), strings.insert(r#"

case x {
    2 -> println "test"
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

    for node in tree { println!("{}", node.to_string(&strings)); }
}

