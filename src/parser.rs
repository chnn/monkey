use super::lexer::{Lexer, Token};
use std::fmt;
use std::mem;

#[derive(PartialEq)]
pub struct Program {
    statements: Vec<Statement>,
}

#[derive(PartialEq)]
pub enum Statement {
    Let { name: String, value: Expression },
    Return(Expression),
    Expression(Expression),
}

#[derive(PartialEq)]
pub enum Expression {
    Ident(String),
    Int(i32),
    Bool(bool),
    Prefix {
        op: Operator,
        right: Box<Expression>,
    },
    Infix {
        op: Operator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    Function {
        params: Vec<Expression>, // Idents
        body: BlockStatement,
    },
    Call {
        function: Box<Expression>, // Function or Ident
        args: Vec<Expression>,
    },
}

#[derive(PartialEq)]
pub struct BlockStatement {
    statements: Vec<Statement>,
}

#[derive(PartialEq)]
pub enum Operator {
    Bang,
    Minus,
    Plus,
    Times,
    Divide,
    MoreThan,
    LessThan,
    Equal,
    NotEqual,
}

impl Operator {
    fn from(token: &Token) -> Operator {
        match token {
            Token::Bang => Operator::Bang,
            Token::Minus => Operator::Minus,
            Token::Plus => Operator::Plus,
            Token::Times => Operator::Times,
            Token::FSlash => Operator::Divide,
            Token::MoreThan => Operator::MoreThan,
            Token::LessThan => Operator::LessThan,
            Token::Equal => Operator::Equal,
            Token::NotEqual => Operator::NotEqual,
            x => panic!("no operator for token {:?}", x),
        }
    }
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precedence {
    fn from(token: &Token) -> Precedence {
        match token {
            Token::Equal => Precedence::Equals,
            Token::NotEqual => Precedence::Equals,
            Token::LessThan => Precedence::LessGreater,
            Token::MoreThan => Precedence::LessGreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::FSlash => Precedence::Product,
            Token::Times => Precedence::Product,
            Token::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub msg: String,
}

impl ParseError {
    fn new(msg: &str) -> ParseError {
        ParseError { msg: msg.to_string() }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    i: usize,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            // TODO: Iterate lazily over (current, next) tuples instead
            tokens: lexer.collect(),
            i: 0,
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut statements: Vec<Statement> = Vec::new();

        while self.has_token() {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.cur_token() {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        self.next_token();

        let name: String = match self.cur_token() {
            Token::Ident(ident) => ident.to_owned(),
            _ => return Err(ParseError::new("expected ident")),
        };

        self.expect_peek(&Token::Assign)?;
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        if let Token::Semicolon = self.peek_token() {
            self.next_token();
        }

        Ok(Statement::Let { name, value: expr })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        if let Token::Semicolon = self.peek_token() {
            self.next_token();
        }

        Ok(Statement::Return(expr))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expression = self.parse_expression(Precedence::Lowest)?;
        let statement = Statement::Expression(expression);

        if let Token::Semicolon = self.peek_token() {
            self.next_token();
        }

        Ok(statement)
    }

    fn parse_expression(&mut self, prec: Precedence) -> Result<Expression, ParseError> {
        let mut left = match self.cur_token() {
            Token::Bang => self.parse_prefix_expression()?,
            Token::Minus => self.parse_prefix_expression()?,
            Token::LParen => self.parse_grouped_expression()?,
            Token::If => self.parse_if_expression()?,
            Token::Function => self.parse_fn_expression()?,
            Token::Ident(value) => Expression::Ident(value.to_owned()),
            Token::True => Expression::Bool(true),
            Token::False => Expression::Bool(false),
            Token::Int(n) => Expression::Int(*n),
            x => {
                let msg = format!("no prefix parse function implemented for `{:?}`", x);

                return Err(ParseError::new(&msg));
            }
        };

        loop {
            if let Token::Semicolon = self.peek_token() {
                break;
            }

            if prec >= self.peek_precedence() {
                break;
            }

            left = match self.peek_token() {
                Token::Plus
                | Token::Minus
                | Token::FSlash
                | Token::Times
                | Token::Equal
                | Token::NotEqual
                | Token::MoreThan
                | Token::LessThan => {
                    self.next_token();
                    self.parse_infix_expression(left)?
                }
                Token::LParen => {
                    self.next_token();
                    self.parse_call_expression(left)?
                }
                _ => {
                    return Ok(left);
                }
            };
        }

        Ok(left)
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        let op = Operator::from(self.cur_token());

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::Prefix {
            op,
            right: Box::new(right),
        })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let op = Operator::from(self.cur_token());
        let precedence = self.cur_precedence();

        self.next_token();

        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix {
            op,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest);

        self.expect_peek(&Token::RParen)?;

        expr
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        self.expect_peek(&Token::LParen)?;
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(&Token::RParen)?;
        self.expect_peek(&Token::LBrace)?;

        let consequence = self.parse_block_statement()?;

        let mut alternative: Option<BlockStatement> = None;

        if self.expect_peek(&Token::Else).is_ok() {
            self.expect_peek(&Token::LBrace)?;

            alternative = Some(self.parse_block_statement()?);
        }

        Ok(Expression::If {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        self.next_token();

        let mut statements: Vec<Statement> = Vec::new();

        loop {
            if !self.has_token() {
                break;
            };

            if let Token::RBrace = self.cur_token() {
                break;
            }

            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(BlockStatement { statements })
    }

    fn parse_fn_expression(&mut self) -> Result<Expression, ParseError> {
        self.expect_peek(&Token::LParen)?;

        let params = self.parse_fn_params()?;

        self.expect_peek(&Token::LBrace)?;

        let body = self.parse_block_statement()?;

        Ok(Expression::Function { params, body })
    }

    fn parse_fn_params(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut idents: Vec<Expression> = Vec::new();

        if self.expect_peek(&Token::RParen).is_ok() {
            return Ok(idents);
        };

        self.next_token();

        let ident = match self.cur_token() {
            Token::Ident(ident) => ident.to_owned(),
            _ => return Err(ParseError::new("expected ident")),
        };

        idents.push(Expression::Ident(ident));

        while let Token::Comma = self.peek_token() {
            self.next_token();
            self.next_token();

            let ident = match self.cur_token() {
                Token::Ident(ident) => ident.to_owned(),
                _ => return Err(ParseError::new("expected ident")),
            };

            idents.push(Expression::Ident(ident));
        }

        self.expect_peek(&Token::RParen)?;

        Ok(idents)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParseError> {
        let args = self.parse_call_arguments()?;

        Ok(Expression::Call {
            function: Box::new(function),
            args,
        })
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut args: Vec<Expression> = Vec::new();

        if self.expect_peek(&Token::RParen).is_ok() {
            return Ok(args);
        };

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest)?);

        while let Token::Comma = self.peek_token() {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(&Token::RParen)?;

        Ok(args)
    }

    fn has_token(&self) -> bool {
        self.i < self.tokens.len()
    }

    fn cur_token(&self) -> &Token {
        &self.tokens[self.i]
    }

    fn peek_token(&self) -> &Token {
        &self.tokens[self.i + 1]
    }

    fn next_token(&mut self) {
        self.i += 1;
    }

    fn expect_peek(&mut self, token: &Token) -> Result<(), ParseError> {
        if mem::discriminant(self.peek_token()) == mem::discriminant(token) {
            self.next_token();
            Ok(())
        } else {
            let msg = format!("expected token `{:?}` but found `{:?}`", token, self.peek_token());

            Err(ParseError::new(&msg))
        }
    }

    fn cur_precedence(&self) -> Precedence {
        Precedence::from(self.cur_token())
    }

    fn peek_precedence(&self) -> Precedence {
        Precedence::from(self.peek_token())
    }
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = self
            .statements
            .iter()
            .fold(String::new(), |out, stmt| format!("{}{:?}\n", out, stmt));

        f.write_str(&output.trim_end())
    }
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let { name, value } => write!(f, "let {} = {:?};", name, value),
            Statement::Return(expr) => write!(f, "return {:?};", expr),
            Statement::Expression(expr) => write!(f, "{:?};", expr),
        }
    }
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Ident(x) => write!(f, "{}", x),
            Expression::Int(x) => write!(f, "{}", x),
            Expression::Bool(x) => write!(f, "{}", x),
            Expression::Prefix { op, right } => write!(f, "({:?}{:?})", op, right),
            Expression::Infix { op, left, right } => write!(f, "({:?} {:?} {:?})", left, op, right),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                if let Some(alt) = alternative {
                    write!(f, "if {:?} then {:?} else {:?}", condition, consequence, alt)
                } else {
                    write!(f, "if {:?} then {:?}", condition, consequence)
                }
            }
            Expression::Function { params, body } => {
                let params_list: String = params
                    .iter()
                    .map(|expr| format!("{:?}", expr))
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "fn({}) {:?}", params_list, body)
            }
            Expression::Call { function, args } => {
                let args_list: String = args
                    .iter()
                    .map(|expr| format!("{:?}", expr))
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "{:?}({})", function, args_list)
            }
        }
    }
}

impl fmt::Debug for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = self
            .statements
            .iter()
            .fold(String::new(), |out, stmt| format!("{}{:?}\n", out, stmt));

        write!(f, "{{ {} }}", output.trim_end())
    }
}

impl fmt::Debug for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operator::Bang => f.write_str("!"),
            Operator::Minus => f.write_str("-"),
            Operator::Plus => f.write_str("+"),
            Operator::Times => f.write_str("*"),
            Operator::Divide => f.write_str("/"),
            Operator::MoreThan => f.write_str(">"),
            Operator::LessThan => f.write_str("<"),
            Operator::Equal => f.write_str("=="),
            Operator::NotEqual => f.write_str("!="),
        }
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
                Statement::Let {
                    name: "x".to_string(),
                    value: Expression::Int(5),
                },
                Statement::Let {
                    name: "y".to_string(),
                    value: Expression::Int(10),
                },
                Statement::Let {
                    name: "foobar".to_string(),
                    value: Expression::Int(838383),
                },
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

        let expected = "let x = 5;
let y = 10;
let foobar = 838383;";

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
                Statement::Return(Expression::Int(5)),
                Statement::Return(Expression::Int(10)),
                Statement::Return(Expression::Int(993322)),
            ],
        };

        let actual = parser.parse_program().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn identifier_expression_ast() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = Program {
            statements: vec![Statement::Expression(Expression::Ident("foobar".to_string()))],
        };

        let actual = parser.parse_program().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn integer_expression() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = Program {
            statements: vec![Statement::Expression(Expression::Int(5))],
        };

        let actual = parser.parse_program().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn bool_expression_ast() {
        let input = "true;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = Program {
            statements: vec![Statement::Expression(Expression::Bool(true))],
        };

        let actual = parser.parse_program().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn prefix_expressions_ast() {
        struct Test {
            input: &'static str,
            op: Operator,
            right: Expression,
        }

        let tests = vec![
            Test {
                input: "!5;",
                op: Operator::Bang,
                right: Expression::Int(5),
            },
            Test {
                input: "!true;",
                op: Operator::Bang,
                right: Expression::Bool(true),
            },
            Test {
                input: "!false;",
                op: Operator::Bang,
                right: Expression::Bool(false),
            },
            Test {
                input: "-15;",
                op: Operator::Minus,
                right: Expression::Int(15),
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);

            let stmt = &parser.parse_program().unwrap().statements[0];

            if let Statement::Expression(Expression::Prefix { op, right }) = stmt {
                assert_eq!(*op, test.op);
                assert_eq!(**right, test.right);
            } else {
                panic!("could not find prefix expression");
            }
        }
    }

    #[test]
    fn infix_expressions_ast() {
        struct Test {
            input: &'static str,
            op: Operator,
            left: Expression,
            right: Expression,
        }

        let tests = vec![
            Test {
                input: "5 + 5;",
                op: Operator::Plus,
                left: Expression::Int(5),
                right: Expression::Int(5),
            },
            Test {
                input: "5 - 5;",
                op: Operator::Minus,
                left: Expression::Int(5),
                right: Expression::Int(5),
            },
            Test {
                input: "5 * 5;",
                op: Operator::Times,
                left: Expression::Int(5),
                right: Expression::Int(5),
            },
            Test {
                input: "5 / 5;",
                op: Operator::Divide,
                left: Expression::Int(5),
                right: Expression::Int(5),
            },
            Test {
                input: "5 > 5;",
                op: Operator::MoreThan,
                left: Expression::Int(5),
                right: Expression::Int(5),
            },
            Test {
                input: "5 < 5;",
                op: Operator::LessThan,
                left: Expression::Int(5),
                right: Expression::Int(5),
            },
            Test {
                input: "5 == 5;",
                op: Operator::Equal,
                left: Expression::Int(5),
                right: Expression::Int(5),
            },
            Test {
                input: "5 != 5;",
                op: Operator::NotEqual,
                left: Expression::Int(5),
                right: Expression::Int(5),
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);

            let stmt = &parser.parse_program().unwrap().statements[0];

            if let Statement::Expression(Expression::Infix { op, left, right }) = stmt {
                assert_eq!(*op, test.op);
                assert_eq!(**left, test.left);
                assert_eq!(**right, test.right);
            } else {
                panic!("could not find prefix expression");
            }
        }
    }

    #[test]
    fn operator_precedence() {
        let tests = vec![
            ("-a * b;", "((-a) * b);"),
            ("!-a;", "(!(-a));"),
            ("a + b + c;", "((a + b) + c);"),
            ("a + b - c;", "((a + b) - c);"),
            ("a * b * c;", "((a * b) * c);"),
            ("a * b / c;", "((a * b) / c);"),
            ("a + b / c;", "(a + (b / c));"),
            ("a + b * c + d / e - f;", "(((a + (b * c)) + (d / e)) - f);"),
            ("3 + 4; -5 * 5;", "(3 + 4);\n((-5) * 5);"),
            ("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4));"),
            ("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4));"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5;", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5;", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));"),
            ("true;", "true;"),
            ("false;", "false;"),
            ("3 > 5 == false;", "((3 > 5) == false);"),
            ("1 + (2 + 3) + 4;", "((1 + (2 + 3)) + 4);"),
            ("(5 + 5) * 2;", "((5 + 5) * 2);"),
            ("2 / (5 + 5);", "(2 / (5 + 5));"),
            ("-(5 + 5);", "(-(5 + 5));"),
            ("!(true == true);", "(!(true == true));"),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let actual = format!("{:?}", parser.parse_program().unwrap());

            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn if_expression_ast() {
        let input = "if (x < y) { x };";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = Program {
            statements: vec![Statement::Expression(Expression::If {
                condition: Box::new(Expression::Infix {
                    op: Operator::LessThan,
                    left: Box::new(Expression::Ident("x".to_string())),
                    right: Box::new(Expression::Ident("y".to_string())),
                }),
                consequence: BlockStatement {
                    statements: vec![Statement::Expression(Expression::Ident("x".to_string()))],
                },
                alternative: None,
            })],
        };

        let actual = parser.parse_program().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn if_else_expression_ast() {
        let input = "if (x < y) { x } else { y };";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = Program {
            statements: vec![Statement::Expression(Expression::If {
                condition: Box::new(Expression::Infix {
                    op: Operator::LessThan,
                    left: Box::new(Expression::Ident("x".to_string())),
                    right: Box::new(Expression::Ident("y".to_string())),
                }),
                consequence: BlockStatement {
                    statements: vec![Statement::Expression(Expression::Ident("x".to_string()))],
                },
                alternative: Some(BlockStatement {
                    statements: vec![Statement::Expression(Expression::Ident("y".to_string()))],
                }),
            })],
        };

        let actual = parser.parse_program().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn fn_expression_string() {
        let input = "
fn(x, y) {
    x + y
};
";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = "fn(x, y) { (x + y); };";
        let actual = format!("{:?}", parser.parse_program().unwrap());

        assert_eq!(actual, expected);
    }

    #[test]
    fn call_expression_ast() {
        let input = "add(1, 2);";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = Program {
            statements: vec![Statement::Expression(Expression::Call {
                function: Box::new(Expression::Ident("add".to_string())),
                args: vec![Expression::Int(1), Expression::Int(2)],
            })],
        };

        let actual = parser.parse_program().unwrap();

        assert_eq!(actual, expected);
    }
}
