use logos::Logos;

#[derive(
    Logos, Debug, PartialEq,
)]
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

    #[regex(r"[a-zA-Z_][a-zA-Z\d]+")]
    Identifier,

    #[regex(r"[\s]+", logos::skip)]
    Whitespace,

    #[error]
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::{read_to_string};
    use std::path::Path;
    use project_root::get_project_root;

    #[test]
    fn lexes_function_definition() {
        let code = r#"
            f bark() {

            }
        "#;
        let mut lex = Token::lexer(code);

        assert_eq!(lex.next(), Some(Token::Function));
        assert_eq!(lex.next(), Some(Token::Identifier));
        assert_eq!(lex.next(), Some(Token::LeftParenthesis));
        assert_eq!(lex.next(), Some(Token::RightParenthesis));
        assert_eq!(lex.next(), Some(Token::LeftBrace));
        assert_eq!(lex.next(), Some(Token::RightBrace));
    }

    #[test]
    fn lexes_goal_input() {
        let root  = get_project_root().unwrap();
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