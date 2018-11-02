extern crate monkey;

use monkey::lexer::{Lexer, Token};
use std::io;
use std::io::prelude::*;

fn main() {
    loop {
        print!(">>> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();

        io::stdin().read_line(&mut input).unwrap();

        let mut lexer = Lexer::new(&input);
        let mut token = lexer.next_token();

        while token != Token::EOF {
            println!("{:?}", token);
            token = lexer.next_token();
        }
    }
}
