#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal,
    EOF,

    // Identifiers + literals
    Ident(String),
    Int(u32),

    // Operators
    Assign,
    Plus,
    Bang,
    Minus,
    Times,
    FSlash,
    LessThan,
    MoreThan,
    Equal,
    NotEqual,

    // Delimiters
    Comma,
    Semicolon,

    // Pairs
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: '\u{000}',
        };

        lexer.read_char();

        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Times,
            '/' => Token::FSlash,
            '<' => Token::LessThan,
            '>' => Token::MoreThan,
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            x if Lexer::is_letter(x) => self.read_ident(),
            x if x.is_ascii_digit() => self.read_number(),
            '\u{000}' => Token::EOF,
            _ => Token::Illegal,
        };

        self.read_char();

        token
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_char(&mut self) {
        self.ch = if self.read_position >= self.input.len() {
            '\u{000}'
        } else {
            self.input[self.read_position]
        };

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            '\u{000}'
        } else {
            self.input[self.read_position]
        }
    }

    fn read_ident(&mut self) -> Token {
        let position = self.position;

        while Lexer::is_letter(self.peek_char()) {
            self.read_char();
        }

        let ident: String = self.input[position..self.position + 1].iter().collect();

        match ident.as_ref() {
            "fn" => Token::Function,
            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Ident(ident),
        }
    }

    fn read_number(&mut self) -> Token {
        let position = self.position;

        while self.peek_char().is_ascii_digit() {
            self.read_char();
        }

        let number = self.input[position..self.position + 1]
            .iter()
            .collect::<String>()
            .parse::<u32>()
            .expect("not a number");

        Token::Int(number)
    }

    fn is_letter(x: char) -> bool {
        x.is_ascii_alphabetic() || x == '_'
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next_token() {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}";

        let expected_tokens: Vec<Token> = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::FSlash,
            Token::Times,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::MoreThan,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input);

        for expected_token in expected_tokens {
            let actual_token = lexer.next_token();

            assert_eq!(expected_token, actual_token);
        }

        assert_eq!(2 + 2, 4);
    }
}
