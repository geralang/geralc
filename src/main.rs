
#![allow(dead_code)]

mod util;
mod compiler;

use util::strings::StringMap;
use compiler::{lexer::Lexer, parser::Parser};

use util::error::{Error, ErrorType, ErrorSection};
use util::source::SourceRange;

fn main() {
    let mut strings = StringMap::new();
    let mut lexer = Lexer::new(strings.insert("test.gera"), strings.insert(r#"

    proc test x {
        return x
            | * 2
            | math::sqrt
            | / 2
    }
    
    "#), &mut strings);

    let mut parser = if let Some(parser) = Parser::new(&mut strings, &mut lexer) { parser } else { return };
    let tree = match parser.parse_block(&mut strings, &mut lexer) {
        Ok(tree) => tree,
        Err(_) => panic!("Something went wrong while parsing"),
    };

    for node in tree { println!("{}", node.to_string(&strings)); }
}

