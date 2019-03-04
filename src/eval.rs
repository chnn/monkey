use super::parser::{Parser, Program, Statement, Node, Expression};
use super::lexer::Lexer;
use std::fmt;

pub enum Object {
    Null,
    Bool(bool),
    Int(i32),
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::Bool(val) => write!(f, "{:?}", val),
            Object::Int(val) => write!(f, "{:?}", val),
        }
    }
}

pub fn eval(node: Node) -> Object {
    match node {
        Node::Program(Program { statements }) => eval_statements(statements),
        Node::Statement(Statement::Expression(expr)) => eval(Node::Expression(expr)),
        Node::Expression(Expression::Int(k)) => Object::Int(k),
        _ => panic!("not implemented")
    }
}

fn eval_statements(statements: Vec<Statement>) -> Object {
    let mut result: Object = Object::Null;

    for statement in statements {
        result = eval(Node::Statement(statement));
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval_integers() {
        let tests: Vec<(&str, i32)> = vec![
            ("5;", 5),
            ("10;", 10)
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let actual = eval(Node::Program(parser.parse_program().unwrap()));

            if let Object::Int(k) = actual {
                assert_eq!(k, expected);
            } else {
                panic!("expected {:?} but got {:?}", Object::Int(expected), actual)
            }
        }
    }
}
