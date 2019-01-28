use std::io;
use std::io::prelude::*;

use monkey::lexer::Lexer;
use monkey::parser::Parser;

fn main() {
    loop {
        print!(">>> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();

        io::stdin().read_line(&mut input).unwrap();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        match parser.parse_file() {
            Ok(file) => println!("{:?}", file),
            Err(error) => println!("error: {}", error),
        }
    }
}
