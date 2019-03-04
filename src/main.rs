extern crate monkey;

use monkey::lexer::Lexer;
use monkey::parser::{Parser, Node};
use monkey::eval::{eval, Object};
use std::io;
use std::io::prelude::*;

fn main() {
    loop {
        print!(">>> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();

        io::stdin().read_line(&mut input).unwrap();

        match get_result(&input) {
            Ok(obj) => println!("{:?}", obj),
            Err(msg) => println!("error: {}", msg)
        }

    }
}

fn get_result(input: &str) -> Result<Object, String> {
    let lexer = Lexer::new(&input);
    let mut parser = Parser::new(lexer);
    let parsed = parser.parse_program();

    if let Result::Err(err) = parsed {
        return Result::Err(err.msg)
    }

    let obj = eval(Node::Program(parsed.unwrap()));

    Ok(obj)
}
