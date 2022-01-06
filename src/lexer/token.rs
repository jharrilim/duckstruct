use logos::Logos;
use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Logos, Debug, PartialEq, Clone, Copy, FromPrimitive, ToPrimitive)]
pub enum Token {
    #[token("f")]
    Function,

    #[token("class")]
    Class,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token("(")]
    LeftParenthesis,

    #[token(")")]
    RightParenthesis,

    #[token(".")]
    Period,

    #[regex(r"[0-9]+")]
    Number,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Asterisk,

    #[token("/")]
    ForwardSlash,

    #[token("=")]
    Equals,

    #[regex(r#""([^\\"]|\.)*""#)]
    String,

    #[token("\"")]
    DoubleQuote,

    #[token("'")]
    SingleQuote,

    #[token(";")]
    Semicolon,

    #[token(":")]
    Colon,

    #[regex(r"[a-zA-Z_][a-zA-Z\d_]+")]
    Identifier,

    #[regex(r"[\s]+", logos::skip)]
    Whitespace,

    #[error]
    Error,

    Root,
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    use super::*;
    use project_root::get_project_root;
    use std::fs::read_to_string;
    use std::path::Path;

    #[test]
    fn lexes_function_definition() {
        let code = r#"
            f bark() {

            }
        "#;
        let mut lex = Lexer::new(code);

        assert_eq!(lex.next(), Some((Token::Function, "f")));
        assert_eq!(lex.next(), Some((Token::Identifier, "bark")));
        assert_eq!(lex.next(), Some((Token::LeftParenthesis, "(")));
        assert_eq!(lex.next(), Some((Token::RightParenthesis, ")")));
        assert_eq!(lex.next(), Some((Token::LeftBrace, "{")));
        assert_eq!(lex.next(), Some((Token::RightBrace, "}")));
    }

    #[test]
    fn lexes_goal_input() {
        let root = get_project_root().unwrap();
        let input_path = Path::new("goal/input.ds");
        let f = read_to_string(root.join(input_path).as_path()).unwrap();
        let mut lex = Token::lexer(&f);

        assert_eq!(lex.next(), Some(Token::Class));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.next(), Some(Token::LeftBrace));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.next(), Some(Token::Equals));
        assert_eq!(lex.next(), Some(Token::String));

        assert_eq!(lex.next(), Some(Token::Semicolon));
    }
}
