use std::fmt::{self, Display};
use std::mem;

use crate::lexer::{Lexer, Token};

#[derive(PartialEq)]
pub struct File {
    pub statements: Vec<Stmt>,
}

#[derive(PartialEq)]
pub enum Stmt {
    Let(StmtLet),
    Return(StmtReturn),
    Expr(StmtExpr),
}

#[derive(PartialEq)]
pub struct StmtReturn {
    pub value: Expr,
}

#[derive(PartialEq)]
pub struct StmtLet {
    pub name: String,
    pub value: Expr,
}

#[derive(PartialEq)]
pub struct StmtExpr {
    pub value: Expr,
}

#[derive(PartialEq)]
pub enum Expr {
    Ident(ExprIdent),
    Int(ExprInt),
    Bool(ExprBool),
    Prefix(ExprPrefix),
    Infix(ExprInfix),
    If(ExprIf),
    Function(ExprFunction),
    Call(ExprCall),
}

#[derive(PartialEq)]
pub struct ExprIdent {
    pub value: String,
}

#[derive(PartialEq)]
pub struct ExprInt {
    pub value: i32,
}

#[derive(PartialEq)]
pub struct ExprBool {
    pub value: bool,
}

#[derive(PartialEq)]
pub struct ExprPrefix {
    pub op: Operator,
    pub right: Box<Expr>,
}

#[derive(PartialEq)]
pub struct ExprInfix {
    pub op: Operator,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(PartialEq)]
pub struct ExprIf {
    pub condition: Box<Expr>,
    pub consequence: Block,
    pub alternative: Option<Block>,
}

#[derive(PartialEq)]
pub struct ExprFunction {
    pub params: Vec<ExprIdent>, // Idents
    pub body: Block,
}

#[derive(PartialEq)]
pub struct ExprCall {
    pub function: Box<Expr>, // Function or Ident
    pub args: Vec<Expr>,
}

#[derive(PartialEq)]
pub struct Block {
    pub statements: Vec<Stmt>,
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

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    message: String,
}

impl Error {
    fn new<T: Display>(message: T) -> Self {
        Error {
            message: message.to_string(),
        }
    }
}

impl Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.message)
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    i: usize,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            tokens: lexer.collect(),
            i: 0,
        }
    }

    pub fn parse_file(&mut self) -> Result<File> {
        let mut statements: Vec<Stmt> = Vec::new();

        while self.has_token() {
            statements.push(self.parse_stmt()?);
            self.next_token();
        }

        Ok(File { statements })
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        let stmt = match self.cur_token() {
            Token::Let => Stmt::Let(self.parse_stmt_let()?),
            Token::Return => Stmt::Return(self.parse_stmt_return()?),
            _ => Stmt::Expr(self.parse_stmt_expr()?),
        };

        Ok(stmt)
    }

    fn parse_stmt_let(&mut self) -> Result<StmtLet> {
        self.next_token();

        let name: String = match self.cur_token() {
            Token::Ident(ident) => ident.to_owned(),
            _ => return Err(Error::new("expected ident")),
        };

        self.expect_peek(&Token::Assign)?;
        self.next_token();

        let expr = self.parse_expr(Precedence::Lowest)?;

        if let Token::Semicolon = self.peek_token() {
            self.next_token();
        }

        Ok(StmtLet { name, value: expr })
    }

    fn parse_stmt_return(&mut self) -> Result<StmtReturn> {
        self.next_token();

        let value = self.parse_expr(Precedence::Lowest)?;

        if let Token::Semicolon = self.peek_token() {
            self.next_token();
        }

        Ok(StmtReturn { value })
    }

    fn parse_stmt_expr(&mut self) -> Result<StmtExpr> {
        let value = self.parse_expr(Precedence::Lowest)?;
        let statement = StmtExpr { value };

        if let Token::Semicolon = self.peek_token() {
            self.next_token();
        }

        Ok(statement)
    }

    fn parse_expr(&mut self, prec: Precedence) -> Result<Expr> {
        let mut left = match self.cur_token() {
            Token::Bang => Expr::Prefix(self.parse_expr_prefix()?),
            Token::Minus => Expr::Prefix(self.parse_expr_prefix()?),
            Token::LParen => self.parse_grouped_expr()?,
            Token::If => Expr::If(self.parse_expr_if()?),
            Token::Function => Expr::Function(self.parse_expr_function()?),
            Token::Ident(value) => Expr::Ident(ExprIdent {
                value: value.to_owned(),
            }),
            Token::True => Expr::Bool(ExprBool { value: true }),
            Token::False => Expr::Bool(ExprBool { value: false }),
            Token::Int(n) => Expr::Int(ExprInt { value: *n }),
            x => {
                let msg = format!("no prefix parse function implemented for `{:?}`", x);

                return Err(Error::new(&msg));
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
                    Expr::Infix(self.parse_expr_infix(left)?)
                }
                Token::LParen => {
                    self.next_token();
                    Expr::Call(self.parse_expr_call(left)?)
                }
                _ => {
                    return Ok(left);
                }
            };
        }

        Ok(left)
    }

    fn parse_expr_prefix(&mut self) -> Result<ExprPrefix> {
        let op = Operator::from(self.cur_token());

        self.next_token();

        let right = self.parse_expr(Precedence::Prefix)?;

        Ok(ExprPrefix {
            op,
            right: Box::new(right),
        })
    }

    fn parse_expr_infix(&mut self, left: Expr) -> Result<ExprInfix> {
        let op = Operator::from(self.cur_token());
        let precedence = self.cur_precedence();

        self.next_token();

        let right = self.parse_expr(precedence)?;

        Ok(ExprInfix {
            op,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn parse_grouped_expr(&mut self) -> Result<Expr> {
        self.next_token();

        let expr = self.parse_expr(Precedence::Lowest);

        self.expect_peek(&Token::RParen)?;

        expr
    }

    fn parse_expr_if(&mut self) -> Result<ExprIf> {
        self.expect_peek(&Token::LParen)?;
        self.next_token();

        let condition = self.parse_expr(Precedence::Lowest)?;

        self.expect_peek(&Token::RParen)?;
        self.expect_peek(&Token::LBrace)?;

        let consequence = self.parse_block()?;

        let mut alternative: Option<Block> = None;

        if self.expect_peek(&Token::Else).is_ok() {
            self.expect_peek(&Token::LBrace)?;

            alternative = Some(self.parse_block()?);
        }

        Ok(ExprIf {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }

    fn parse_block(&mut self) -> Result<Block> {
        self.next_token();

        let mut statements: Vec<Stmt> = Vec::new();

        loop {
            if !self.has_token() {
                break;
            };

            if let Token::RBrace = self.cur_token() {
                break;
            }

            statements.push(self.parse_stmt()?);
            self.next_token();
        }

        Ok(Block { statements })
    }

    fn parse_expr_function(&mut self) -> Result<ExprFunction> {
        self.expect_peek(&Token::LParen)?;

        let params = self.parse_expr_function_params()?;

        self.expect_peek(&Token::LBrace)?;

        let body = self.parse_block()?;

        Ok(ExprFunction { params, body })
    }

    fn parse_expr_function_params(&mut self) -> Result<Vec<ExprIdent>> {
        let mut idents: Vec<ExprIdent> = Vec::new();

        if self.expect_peek(&Token::RParen).is_ok() {
            return Ok(idents);
        };

        self.next_token();

        let ident = match self.cur_token() {
            Token::Ident(ident) => ident.to_owned(),
            _ => return Err(Error::new("expected ident")),
        };

        idents.push(ExprIdent { value: ident });

        while let Token::Comma = self.peek_token() {
            self.next_token();
            self.next_token();

            let ident = match self.cur_token() {
                Token::Ident(ident) => ident.to_owned(),
                _ => return Err(Error::new("expected ident")),
            };

            idents.push(ExprIdent { value: ident });
        }

        self.expect_peek(&Token::RParen)?;

        Ok(idents)
    }

    fn parse_expr_call(&mut self, function: Expr) -> Result<ExprCall> {
        let args = self.parse_expr_call_args()?;

        Ok(ExprCall {
            function: Box::new(function),
            args,
        })
    }

    fn parse_expr_call_args(&mut self) -> Result<Vec<Expr>> {
        let mut args: Vec<Expr> = Vec::new();

        if self.expect_peek(&Token::RParen).is_ok() {
            return Ok(args);
        };

        self.next_token();
        args.push(self.parse_expr(Precedence::Lowest)?);

        while let Token::Comma = self.peek_token() {
            self.next_token();
            self.next_token();
            args.push(self.parse_expr(Precedence::Lowest)?);
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

    fn expect_peek(&mut self, token: &Token) -> Result<()> {
        if mem::discriminant(self.peek_token()) == mem::discriminant(token) {
            self.next_token();
            Ok(())
        } else {
            let msg = format!("expected token `{:?}` but found `{:?}`", token, self.peek_token());

            Err(Error::new(&msg))
        }
    }

    fn cur_precedence(&self) -> Precedence {
        Precedence::from(self.cur_token())
    }

    fn peek_precedence(&self) -> Precedence {
        Precedence::from(self.peek_token())
    }
}

impl fmt::Debug for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let output = self
            .statements
            .iter()
            .fold(String::new(), |out, stmt| format!("{}{:?}\n", out, stmt));

        f.write_str(&output.trim_end())
    }
}

impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Let(StmtLet { name, value }) => write!(f, "let {} = {:?};", name, value),
            Stmt::Return(StmtReturn { value }) => write!(f, "return {:?};", value),
            Stmt::Expr(StmtExpr { value }) => write!(f, "{:?};", value),
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ident(ExprIdent { value }) => write!(f, "{}", value),
            Expr::Int(ExprInt { value }) => write!(f, "{}", value),
            Expr::Bool(ExprBool { value }) => write!(f, "{}", value),
            Expr::Prefix(ExprPrefix { op, right }) => write!(f, "({:?}{:?})", op, right),
            Expr::Infix(ExprInfix { op, left, right }) => write!(f, "({:?} {:?} {:?})", left, op, right),
            Expr::If(ExprIf {
                condition,
                consequence,
                alternative,
            }) => {
                if let Some(alt) = alternative {
                    write!(f, "if {:?} then {:?} else {:?}", condition, consequence, alt)
                } else {
                    write!(f, "if {:?} then {:?}", condition, consequence)
                }
            }
            Expr::Function(ExprFunction { params, body }) => {
                let params_list: String = params
                    .iter()
                    .map(|expr| format!("{:?}", expr))
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "fn({}) {:?}", params_list, body)
            }
            Expr::Call(ExprCall { function, args }) => {
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

impl fmt::Debug for ExprIdent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let output = self
            .statements
            .iter()
            .fold(String::new(), |out, stmt| format!("{}{:?}\n", out, stmt));

        write!(f, "{{ {} }}", output.trim_end())
    }
}

impl fmt::Debug for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

        let expected = File {
            statements: vec![
                Stmt::Let(StmtLet {
                    name: "x".to_string(),
                    value: Expr::Int(ExprInt { value: 5 }),
                }),
                Stmt::Let(StmtLet {
                    name: "y".to_string(),
                    value: Expr::Int(ExprInt { value: 10 }),
                }),
                Stmt::Let(StmtLet {
                    name: "foobar".to_string(),
                    value: Expr::Int(ExprInt { value: 838383 }),
                }),
            ],
        };

        let actual = parser.parse_file().unwrap();

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

        let actual = format!("{:?}", parser.parse_file().unwrap());

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

        let expected = File {
            statements: vec![
                Stmt::Return(StmtReturn {
                    value: Expr::Int(ExprInt { value: 5 }),
                }),
                Stmt::Return(StmtReturn {
                    value: Expr::Int(ExprInt { value: 10 }),
                }),
                Stmt::Return(StmtReturn {
                    value: Expr::Int(ExprInt { value: 993322 }),
                }),
            ],
        };

        let actual = parser.parse_file().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn identifier_expression_ast() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = File {
            statements: vec![Stmt::Expr(StmtExpr {
                value: Expr::Ident(ExprIdent {
                    value: "foobar".to_string(),
                }),
            })],
        };

        let actual = parser.parse_file().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn integer_expression() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = File {
            statements: vec![Stmt::Expr(StmtExpr {
                value: Expr::Int(ExprInt { value: (5) }),
            })],
        };

        let actual = parser.parse_file().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn bool_expression_ast() {
        let input = "true;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = File {
            statements: vec![Stmt::Expr(StmtExpr {
                value: Expr::Bool(ExprBool { value: true }),
            })],
        };

        let actual = parser.parse_file().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn prefix_expressions_ast() {
        struct Test {
            input: &'static str,
            op: Operator,
            right: Expr,
        }

        let tests = vec![
            Test {
                input: "!5;",
                op: Operator::Bang,
                right: Expr::Int(ExprInt { value: 5 }),
            },
            Test {
                input: "!true;",
                op: Operator::Bang,
                right: Expr::Bool(ExprBool { value: true }),
            },
            Test {
                input: "!false;",
                op: Operator::Bang,
                right: Expr::Bool(ExprBool { value: false }),
            },
            Test {
                input: "-15;",
                op: Operator::Minus,
                right: Expr::Int(ExprInt { value: 15 }),
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);

            let stmt = &parser.parse_file().unwrap().statements[0];

            if let Stmt::Expr(StmtExpr {
                value: Expr::Prefix(ExprPrefix { op, right }),
            }) = stmt
            {
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
            left: Expr,
            right: Expr,
        }

        let tests = vec![
            Test {
                input: "5 + 5;",
                op: Operator::Plus,
                left: Expr::Int(ExprInt { value: 5 }),
                right: Expr::Int(ExprInt { value: 5 }),
            },
            Test {
                input: "5 - 5;",
                op: Operator::Minus,
                left: Expr::Int(ExprInt { value: 5 }),
                right: Expr::Int(ExprInt { value: 5 }),
            },
            Test {
                input: "5 * 5;",
                op: Operator::Times,
                left: Expr::Int(ExprInt { value: 5 }),
                right: Expr::Int(ExprInt { value: 5 }),
            },
            Test {
                input: "5 / 5;",
                op: Operator::Divide,
                left: Expr::Int(ExprInt { value: 5 }),
                right: Expr::Int(ExprInt { value: 5 }),
            },
            Test {
                input: "5 > 5;",
                op: Operator::MoreThan,
                left: Expr::Int(ExprInt { value: 5 }),
                right: Expr::Int(ExprInt { value: 5 }),
            },
            Test {
                input: "5 < 5;",
                op: Operator::LessThan,
                left: Expr::Int(ExprInt { value: 5 }),
                right: Expr::Int(ExprInt { value: 5 }),
            },
            Test {
                input: "5 == 5;",
                op: Operator::Equal,
                left: Expr::Int(ExprInt { value: 5 }),
                right: Expr::Int(ExprInt { value: 5 }),
            },
            Test {
                input: "5 != 5;",
                op: Operator::NotEqual,
                left: Expr::Int(ExprInt { value: 5 }),
                right: Expr::Int(ExprInt { value: 5 }),
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);

            let stmt = &parser.parse_file().unwrap().statements[0];

            if let Stmt::Expr(StmtExpr {
                value: Expr::Infix(ExprInfix { op, left, right }),
            }) = stmt
            {
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

            let actual = format!("{:?}", parser.parse_file().unwrap());

            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn if_expression_ast() {
        let input = "if (x < y) { x };";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = File {
            statements: vec![Stmt::Expr(StmtExpr {
                value: Expr::If(ExprIf {
                    condition: Box::new(Expr::Infix(ExprInfix {
                        op: Operator::LessThan,
                        left: Box::new(Expr::Ident(ExprIdent { value: "x".to_string() })),
                        right: Box::new(Expr::Ident(ExprIdent { value: "y".to_string() })),
                    })),
                    consequence: Block {
                        statements: vec![Stmt::Expr(StmtExpr {
                            value: Expr::Ident(ExprIdent { value: "x".to_string() }),
                        })],
                    },
                    alternative: None,
                }),
            })],
        };

        let actual = parser.parse_file().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn if_else_expression_ast() {
        let input = "if (x < y) { x } else { y };";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = File {
            statements: vec![Stmt::Expr(StmtExpr {
                value: Expr::If(ExprIf {
                    condition: Box::new(Expr::Infix(ExprInfix {
                        op: Operator::LessThan,
                        left: Box::new(Expr::Ident(ExprIdent { value: "x".to_string() })),
                        right: Box::new(Expr::Ident(ExprIdent { value: "y".to_string() })),
                    })),
                    consequence: Block {
                        statements: vec![Stmt::Expr(StmtExpr {
                            value: Expr::Ident(ExprIdent { value: "x".to_string() }),
                        })],
                    },
                    alternative: Some(Block {
                        statements: vec![Stmt::Expr(StmtExpr {
                            value: Expr::Ident(ExprIdent { value: "y".to_string() }),
                        })],
                    }),
                }),
            })],
        };

        let actual = parser.parse_file().unwrap();

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
        let actual = format!("{:?}", parser.parse_file().unwrap());

        assert_eq!(actual, expected);
    }

    #[test]
    fn call_expression_ast() {
        let input = "add(1, 2);";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expected = File {
            statements: vec![Stmt::Expr(StmtExpr {
                value: Expr::Call(ExprCall {
                    function: Box::new(Expr::Ident(ExprIdent {
                        value: "add".to_string(),
                    })),
                    args: vec![Expr::Int(ExprInt { value: 1 }), Expr::Int(ExprInt { value: 2 })],
                }),
            })],
        };

        let actual = parser.parse_file().unwrap();

        assert_eq!(actual, expected);
    }
}
