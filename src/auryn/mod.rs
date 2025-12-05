pub mod ast;
pub mod parser;
pub mod syntax_tree;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperatorToken {
    Plus,
    Times,
}

impl BinaryOperatorToken {
    pub fn binding_power(self) -> u32 {
        match self {
            BinaryOperatorToken::Plus => 1,
            BinaryOperatorToken::Times => 2,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TokenKind {
    Number(i32),
    Plus,
    Times,
    Whitespace,
    Error,
    EndOfInput,
}

impl TokenKind {
    pub fn to_binary_operator(self) -> Option<BinaryOperatorToken> {
        match self {
            TokenKind::Plus => Some(BinaryOperatorToken::Plus),
            TokenKind::Times => Some(BinaryOperatorToken::Times),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub len: u32,
}

pub struct Tokenizer<'a> {
    input: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    fn advance(&mut self, num_bytes: usize) {
        self.input = &self.input[num_bytes..];
    }

    fn consume_while<F: FnMut(char) -> bool>(&self, mut predicate: F) -> Option<u32> {
        let Some((len, char)) = self
            .input
            .char_indices()
            .take_while(move |(_, char)| predicate(*char))
            .last()
        else {
            return None;
        };
        let len = len + char.len_utf8();
        let len = len.try_into().expect("Token too long");
        Some(len)
    }

    fn get_whitespace(&mut self) -> Option<Token> {
        let Some(len) = self.consume_while(char::is_whitespace) else {
            return None;
        };
        Some(Token {
            kind: TokenKind::Whitespace,
            len,
        })
    }

    fn get_number(&mut self) -> Option<Token> {
        let Some(len) = self.consume_while(|char| char.is_digit(10)) else {
            return None;
        };
        let value = self.input[..(len as usize)]
            .parse()
            .expect("Should be a valid number");
        Some(Token {
            kind: TokenKind::Number(value),
            len,
        })
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(first_char) = self.input.chars().next() else {
            return None;
        };

        let token = match first_char {
            '+' => Token {
                kind: TokenKind::Plus,
                len: 1,
            },
            '*' => Token {
                kind: TokenKind::Times,
                len: 1,
            },
            char if char.is_whitespace() => self
                .get_whitespace()
                .expect("Contains at least one whitespace character"),
            char if char.is_digit(10) => self.get_number().expect("Contains at least one digit"),
            _ => Token {
                kind: TokenKind::Error,
                len: 1,
            },
        };

        self.advance(token.len as usize);

        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use crate::auryn::{Token, Tokenizer};

    fn tokenize(input: &str) -> Vec<Token> {
        Tokenizer::new(input).collect()
    }

    #[test]
    fn test_parser() {
        insta::assert_debug_snapshot!(tokenize(" 1 +4  \n"));
    }
}
