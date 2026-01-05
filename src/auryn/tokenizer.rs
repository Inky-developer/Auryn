use core::fmt;

use crate::{bitset_item, utils::bitset::Bitset};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperatorToken {
    Plus,
    Minus,
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
            Self::Plus | Self::Minus => 3,
            Self::Times => 4,
        }
    }
}

pub type TokenSet = Bitset<TokenKind>;

bitset_item! {
    #[derive(Debug, Clone, Copy, Eq, PartialEq)]
    pub enum TokenKind {
        NumberLiteral,
        StringLiteral,
        Identifier,
        Plus,
        Minus,
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
        BracketOpen,
        BracketClose,
        KeywordLet,
        KeywordLoop,
        KeywordBreak,
        KeywordIf,
        KeywordFn,
        KeywordReturn,
        KeywordUnsafe,
        KeywordExtern,
        KeywordStatic,
        KeywordType,
        Whitespace,
        Newline,
        Error,
        EndOfInput,
        Comma,
        Colon,
        Arrow,
        Dot,
    }
}

impl TokenKind {
    pub fn to_binary_operator(self) -> Option<BinaryOperatorToken> {
        match self {
            TokenKind::Plus => Some(BinaryOperatorToken::Plus),
            TokenKind::Minus => Some(BinaryOperatorToken::Minus),
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

    pub fn as_str(self) -> &'static str {
        match self {
            TokenKind::NumberLiteral => "<number literal>",
            TokenKind::StringLiteral => "<string literal>",
            TokenKind::Identifier => "<identifier>",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Times => "*",
            TokenKind::Equal => "=",
            TokenKind::DoubleEqual => "==",
            TokenKind::NotEqual => "!=",
            TokenKind::Greater => ">",
            TokenKind::GreaterOrEqual => ">=",
            TokenKind::Less => "<",
            TokenKind::LessOrEqual => "<=",
            TokenKind::ParensOpen => "(",
            TokenKind::ParensClose => ")",
            TokenKind::BraceOpen => "{",
            TokenKind::BraceClose => "}",
            TokenKind::BracketOpen => "[",
            TokenKind::BracketClose => "]",
            TokenKind::KeywordLet => "let",
            TokenKind::KeywordLoop => "loop",
            TokenKind::KeywordBreak => "break",
            TokenKind::KeywordIf => "if",
            TokenKind::KeywordFn => "fn",
            TokenKind::KeywordReturn => "return",
            TokenKind::KeywordUnsafe => "unsafe",
            TokenKind::KeywordExtern => "extern",
            TokenKind::KeywordStatic => "static",
            TokenKind::KeywordType => "type",
            TokenKind::Whitespace => "<whitespace>",
            TokenKind::Newline => "<newline>",
            TokenKind::Error => "<error>",
            TokenKind::EndOfInput => "<end of input>",
            TokenKind::Comma => ",",
            TokenKind::Colon => ":",
            TokenKind::Arrow => "->",
            TokenKind::Dot => ".",
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
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

    fn starts_with_keyword(&self, keyword: &str) -> bool {
        self.input.starts_with(keyword)
            && self.input[keyword.len()..]
                .chars()
                .next()
                .unwrap_or('\0')
                .is_ascii_whitespace()
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
            text: self.consume_while(|char| char != '\n' && char.is_ascii_whitespace()),
        })
    }

    fn consume_number(&mut self) -> Option<Token<'a>> {
        Some(Token {
            kind: TokenKind::NumberLiteral,
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

    fn consume_string_literal(&mut self) -> Token<'a> {
        let mut len = 1;
        let mut found_end = false;
        for (size, char) in self.input.char_indices().skip(1) {
            len = size;
            if found_end {
                found_end = false;
                break;
            }
            if char == '"' {
                found_end = true;
            }
        }
        // If we found the end but there was no next char, the literal takes the full len of our input
        if found_end {
            len = self.input.len();
        }

        let (fit, rest) = self.input.split_at(len);
        self.input = rest;
        Token {
            kind: TokenKind::StringLiteral,
            text: fit,
        }
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
            '-' if self.input.starts_with("->") => {
                return Some(Token {
                    kind: TokenKind::Arrow,
                    text: self.consume_text("->"),
                });
            }
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
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
            '[' => TokenKind::BracketOpen,
            ']' => TokenKind::BracketClose,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            '.' => TokenKind::Dot,
            '\n' => TokenKind::Newline,
            'l' if self.starts_with_keyword("let") => {
                return Some(Token {
                    kind: TokenKind::KeywordLet,
                    text: self.consume_text("let"),
                });
            }
            'l' if self.starts_with_keyword("loop") => {
                return Some(Token {
                    kind: TokenKind::KeywordLoop,
                    text: self.consume_text("loop"),
                });
            }
            'b' if self.starts_with_keyword("break") => {
                return Some(Token {
                    kind: TokenKind::KeywordBreak,
                    text: self.consume_text("break"),
                });
            }
            'e' if self.starts_with_keyword("extern") => {
                return Some(Token {
                    kind: TokenKind::KeywordExtern,
                    text: self.consume_text("extern"),
                });
            }
            'f' if self.starts_with_keyword("fn") => {
                return Some(Token {
                    kind: TokenKind::KeywordFn,
                    text: self.consume_text("fn"),
                });
            }
            'i' if self.starts_with_keyword("if") => {
                return Some(Token {
                    kind: TokenKind::KeywordIf,
                    text: self.consume_text("if"),
                });
            }
            'r' if self.starts_with_keyword("return") => {
                return Some(Token {
                    kind: TokenKind::KeywordReturn,
                    text: self.consume_text("return"),
                });
            }
            's' if self.starts_with_keyword("static") => {
                return Some(Token {
                    kind: TokenKind::KeywordStatic,
                    text: self.consume_text("static"),
                });
            }
            't' if self.starts_with_keyword("type") => {
                return Some(Token {
                    kind: TokenKind::KeywordType,
                    text: self.consume_text("type"),
                });
            }
            'u' if self.starts_with_keyword("unsafe") => {
                return Some(Token {
                    kind: TokenKind::KeywordUnsafe,
                    text: self.consume_text("unsafe"),
                });
            }
            '"' => return Some(self.consume_string_literal()),
            'a'..='z' | 'A'..='Z' | '_' => return self.consume_identifier(),
            char if char.is_ascii_whitespace() => return self.consume_whitespace(),
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
    use proptest::{prop_assert, prop_assert_eq, proptest};

    use super::{Token, Tokenizer};

    fn tokenize(input: &str) -> Vec<Token<'_>> {
        Tokenizer::new(input).collect()
    }

    #[test]
    fn test_parser() {
        insta::assert_debug_snapshot!(tokenize(" 1 +4  \n"));
        insta::assert_debug_snapshot!(tokenize(" hello_World(1 + 1, 1 -2)"));
        insta::assert_debug_snapshot!(tokenize(" \n \t\n "));
        insta::assert_debug_snapshot!(tokenize(
            "loop let some_text = if true { 2 } else { break return } fn [foo.bar]"
        ));
        insta::assert_debug_snapshot!(tokenize(
            "comparisons = a == 1 && a != 2 && a > 3 && a >= 4 && a < 5 && a <= 6 -> a"
        ));
        insta::assert_debug_snapshot!(tokenize("( \"Hello, World!\" ) && \"test\""));
        insta::assert_debug_snapshot!(tokenize("unsafe extern type Foo { static let bar }"));
    }

    #[test]
    fn handles_invalid_string_literal() {
        insta::assert_debug_snapshot!(tokenize("\""));
    }

    #[test]
    fn rejects_unicode_whitespace() {
        // non breaking space
        insta::assert_debug_snapshot!(tokenize("\u{a0}"));
    }

    proptest! {
        #[test]
        fn as_str_equivalent(s in "[[:ascii:]]*") {
            let tokens = Tokenizer::new(&s);
            for token in tokens {
                prop_assert!(!token.text.is_empty());
                let str = token.kind.as_str();
                if !str.starts_with("<") {
                    prop_assert_eq!(token.text, str);
                }
            }
        }
    }
}
