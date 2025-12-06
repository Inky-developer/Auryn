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
    Number,
    Identifier,
    Plus,
    Times,
    ParensOpen,
    ParensClose,
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
pub struct Token<'a> {
    pub kind: TokenKind,
    pub text: &'a str,
}

pub struct Tokenizer<'a> {
    input: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    fn consume_while<F: FnMut(char) -> bool>(&mut self, mut predicate: F) -> &'a str {
        let first_non_matching_byte = match self.input.find(|char| !predicate(char)) {
            Some(index) => index,
            None => self.input.len(),
        };

        let (fit, rest) = self.input.split_at(first_non_matching_byte);
        self.input = rest;
        fit
    }

    fn consume_whitespace(&mut self) -> Option<Token<'a>> {
        Some(Token {
            kind: TokenKind::Whitespace,
            text: self.consume_while(char::is_whitespace),
        })
    }

    fn consume_number(&mut self) -> Option<Token<'a>> {
        Some(Token {
            kind: TokenKind::Number,
            text: self.consume_while(|char| char.is_digit(10)),
        })
    }

    fn consume_identifier(&mut self) -> Option<Token<'a>> {
        Some(Token {
            kind: TokenKind::Identifier,
            text: self
                .consume_while(|char| matches!(char, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_')),
        })
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(first_char) = self.input.chars().next() else {
            return None;
        };

        let kind = match first_char {
            '+' => TokenKind::Plus,
            '*' => TokenKind::Times,
            '(' => TokenKind::ParensOpen,
            ')' => TokenKind::ParensClose,
            'a'..='z' | 'A'..='Z' | '_' => return self.consume_identifier(),
            char if char.is_whitespace() => return self.consume_whitespace(),
            char if char.is_digit(10) => return self.consume_number(),
            _ => TokenKind::Error,
        };

        let (char_text, rest) = self.input.split_at(first_char.len_utf8());
        self.input = rest;

        Some(Token {
            kind,
            text: char_text,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{Token, Tokenizer};

    fn tokenize(input: &str) -> Vec<Token<'_>> {
        Tokenizer::new(input).collect()
    }

    #[test]
    fn test_parser() {
        insta::assert_debug_snapshot!(tokenize(" 1 +4  \n"));
        insta::assert_debug_snapshot!(tokenize(" hello_World(1 + 1)"));
    }
}
