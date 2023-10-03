
#![allow(dead_code)]

mod compiler;

use compiler::{
    strings::StringMap,
    lexer::Lexer,
    parser::Parser
};

fn main() {
    let mut strings = StringMap::new();
    let mut lexer = Lexer::new(r#"

proc add x y {
    return x | + y
}

"#, "test.gera", &mut strings);
    let mut parser = if let Some(parser) = Parser::new(&mut strings, &mut lexer) { parser } else { return };
    let tree = match parser.parse_block(&mut strings, &mut lexer) {
        Ok(tree) => tree,
        Err(_) => panic!("Something went wrong while parsing"),
    };

    for node in tree { println!("{}", node.to_string(&strings)); }
}

