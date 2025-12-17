#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperatorToken {
    Plus,
    Times,
    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
}

impl BinaryOperatorToken {
    pub fn binding_power(self) -> u32 {
        match self {
            Self::Equal | Self::NotEqual => 1,
            Self::GreaterOrEqual | Self::Greater | Self::LessOrEqual | Self::Less => 2,
            Self::Plus => 3,
            Self::Times => 4,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TokenKind {
    Number,
    Identifier,
    Plus,
    Times,
    Equal,
    DoubleEqual,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
    ParensOpen,
    ParensClose,
    BraceOpen,
    BraceClose,
    KeywordLet,
    KeywordIf,
    Whitespace,
    Newline,
    Error,
    EndOfInput,
    Comma,
}

impl TokenKind {
    pub fn to_binary_operator(self) -> Option<BinaryOperatorToken> {
        match self {
            TokenKind::Plus => Some(BinaryOperatorToken::Plus),
            TokenKind::Times => Some(BinaryOperatorToken::Times),
            TokenKind::DoubleEqual => Some(BinaryOperatorToken::Equal),
            TokenKind::NotEqual => Some(BinaryOperatorToken::NotEqual),
            TokenKind::Greater => Some(BinaryOperatorToken::Greater),
            TokenKind::GreaterOrEqual => Some(BinaryOperatorToken::GreaterOrEqual),
            TokenKind::Less => Some(BinaryOperatorToken::Less),
            TokenKind::LessOrEqual => Some(BinaryOperatorToken::LessOrEqual),
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
            text: self.consume_while(|char| char != '\n' && char.is_whitespace()),
        })
    }

    fn consume_number(&mut self) -> Option<Token<'a>> {
        Some(Token {
            kind: TokenKind::Number,
            text: self.consume_while(|char| char.is_ascii_digit()),
        })
    }

    fn consume_identifier(&mut self) -> Option<Token<'a>> {
        Some(Token {
            kind: TokenKind::Identifier,
            text: self
                .consume_while(|char| matches!(char, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_')),
        })
    }

    fn consume_text(&mut self, arg: &str) -> &'a str {
        let (text, rest) = self.input.split_at(arg.len());
        assert_eq!(
            text, arg,
            "Could not consume text, expected {arg} got {text}"
        );
        self.input = rest;
        text
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let first_char = self.input.chars().next()?;

        let kind = match first_char {
            '+' => TokenKind::Plus,
            '*' => TokenKind::Times,
            '=' => {
                if self.input.starts_with("==") {
                    return Some(Token {
                        kind: TokenKind::DoubleEqual,
                        text: self.consume_text("=="),
                    });
                }
                TokenKind::Equal
            }
            '!' if self.input.starts_with("!=") => {
                return Some(Token {
                    kind: TokenKind::NotEqual,
                    text: self.consume_text("!="),
                });
            }
            '>' if self.input.starts_with(">=") => {
                return Some(Token {
                    kind: TokenKind::GreaterOrEqual,
                    text: self.consume_text(">="),
                });
            }
            '>' => TokenKind::Greater,
            '<' if self.input.starts_with("<=") => {
                return Some(Token {
                    kind: TokenKind::LessOrEqual,
                    text: self.consume_text("<="),
                });
            }
            '<' => TokenKind::Less,
            '(' => TokenKind::ParensOpen,
            ')' => TokenKind::ParensClose,
            '{' => TokenKind::BraceOpen,
            '}' => TokenKind::BraceClose,
            ',' => TokenKind::Comma,
            '\n' => TokenKind::Newline,
            'l' if self.input.starts_with("let ") => {
                return Some(Token {
                    kind: TokenKind::KeywordLet,
                    text: self.consume_text("let "),
                });
            }
            'i' if self.input.starts_with("if ") => {
                return Some(Token {
                    kind: TokenKind::KeywordIf,
                    text: self.consume_text("if "),
                });
            }
            'a'..='z' | 'A'..='Z' | '_' => return self.consume_identifier(),
            char if char.is_whitespace() => return self.consume_whitespace(),
            char if char.is_ascii_digit() => return self.consume_number(),
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
        insta::assert_debug_snapshot!(tokenize(" hello_World(1 + 1, 2)"));
        insta::assert_debug_snapshot!(tokenize(" \n \t\n "));
        insta::assert_debug_snapshot!(tokenize("let some_text = if true { 2 } else { 0 }"));
        insta::assert_debug_snapshot!(tokenize(
            "comparisons = a == 1 && a != 2 && a > 3 && a >= 4 && a < 5 && a <= 6"
        ));
    }
}
