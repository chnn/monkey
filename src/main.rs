extern crate monkey;

use monkey::lexer::Lexer;
use monkey::parser::{ParseError, Parser};
use std::io;
use std::io::prelude::*;

fn main() {
    loop {
        print!(">>> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();

        io::stdin().read_line(&mut input).unwrap();

        let mut lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        match parser.parse_program() {
            Ok(program) => println!("{:?}", program),
            Err(ParseError { msg }) => println!("error: {}", msg),
        }
    }
}
