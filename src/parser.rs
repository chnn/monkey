use super::lexer::{Lexer, Token};
use std::borrow::Borrow;
use std::fmt;
use std::rc::Rc;

#[derive(PartialEq)]
pub struct Program {
    statements: Vec<Statement>,
}

#[derive(PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

#[derive(PartialEq)]
pub struct LetStatement {
    name: Identifier,
    value: Expression,
}

#[derive(PartialEq)]
pub struct ReturnStatement {
    value: Expression,
}

#[derive(PartialEq)]
pub struct ExpressionStatement {
    expression: Expression,
}

#[derive(PartialEq)]
pub struct Expression {}

#[derive(PartialEq)]
pub struct Identifier {
    value: String,
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = self
            .statements
            .iter()
            .fold(String::new(), |out, stmt| format!("{}{:?}\n", out, stmt));

        f.write_str(&output)
    }
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(stmt) => write!(f, "{:?}", stmt),
            Statement::Return(stmt) => write!(f, "{:?}", stmt),
            Statement::Expression(stmt) => write!(f, "{:?}", stmt),
        }
    }
}

impl fmt::Debug for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "let {:?} = {:?};", self.name, self.value)
    }
}

impl fmt::Debug for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "return {:?};", self.value)
    }
}

impl fmt::Debug for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.expression)
    }
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("<expression>")
    }
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.value)
    }
}

#[derive(Debug)]
pub struct ParseError {
    msg: String,
}

impl ParseError {
    fn new(msg: &str) -> ParseError {
        ParseError {
            msg: msg.to_string(),
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Rc<Token>,
    peek_token: Rc<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut p = Parser {
            lexer,
            cur_token: Rc::new(Token::Illegal),
            peek_token: Rc::new(Token::Illegal),
        };

        p.next_token();
        p.next_token();

        p
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut statements: Vec<Statement> = Vec::new();

        while *self.cur_token != Token::EOF {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.cur_token.borrow() {
            Token::Let => Ok(Statement::Let(self.parse_let_statement()?)),
            Token::Return => Ok(Statement::Return(self.parse_return_statement()?)),
            _ => panic!("not implemented"),
        }
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, ParseError> {
        self.next_token();

        let name: String = match self.cur_token.borrow() {
            Token::Ident(ident) => ident.to_owned(),
            _ => return Err(ParseError::new("expected ident")),
        };

        self.next_token();

        match self.cur_token.borrow() {
            Token::Assign => {}
            _ => return Err(ParseError::new("expected assignment")),
        };

        loop {
            self.next_token();

            // TODO: Parse expression here
            if let Token::Semicolon = self.cur_token.borrow() {
                break;
            }
        }

        Ok(LetStatement {
            name: Identifier { value: name },
            value: Expression {},
        })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, ParseError> {
        self.next_token();

        loop {
            self.next_token();

            // TODO: Parse expression here
            if let Token::Semicolon = self.cur_token.borrow() {
                break;
            }
        }

        return Ok(ReturnStatement {
            value: Expression {},
        });
    }

    fn next_token(&mut self) {
        self.cur_token = Rc::clone(&self.peek_token);
        self.peek_token = Rc::new(self.lexer.next_token());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn let_statements_ast() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = Program {
            statements: vec![
                Statement::Let(LetStatement {
                    name: Identifier {
                        value: "x".to_string(),
                    },
                    value: Expression {},
                }),
                Statement::Let(LetStatement {
                    name: Identifier {
                        value: "y".to_string(),
                    },
                    value: Expression {},
                }),
                Statement::Let(LetStatement {
                    name: Identifier {
                        value: "foobar".to_string(),
                    },
                    value: Expression {},
                }),
            ],
        };

        let actual = parser.parse_program().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn let_statements_string() {
        let input = "
let x = 5;
let y      = 10;

let foobar = 838383;
";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = "let x = <expression>;
let y = <expression>;
let foobar = <expression>;
";

        let actual = format!("{:?}", parser.parse_program().unwrap());

        assert_eq!(actual, expected);
    }

    #[test]
    fn return_statements_ast() {
        let input = "
return 5;
return 10;
return 993322;
";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = Program {
            statements: vec![
                Statement::Return(ReturnStatement {
                    value: Expression {},
                }),
                Statement::Return(ReturnStatement {
                    value: Expression {},
                }),
                Statement::Return(ReturnStatement {
                    value: Expression {},
                }),
            ],
        };

        let actual = parser.parse_program().unwrap();

        assert_eq!(actual, expected);
    }

}
