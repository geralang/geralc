
#![allow(dead_code)]

mod compiler;

use compiler::lexer::Lexer;

fn main() {
    let mut lexer = Lexer::new(r#"
proc main {
    println "You could also say Gera is \"based\"."
}


    "#, "test.gera");
    loop {
        match lexer.next_token() {
            None => break,
            Some(token) => println!("{:?}(\"{}\")", token.token_type, token.content)
        }
    }
}

