use std::iter::Peekable;

use crate::auryn::{
    syntax_tree::{SyntaxNode, SyntaxNodeKind, SyntaxTree},
    tokenizer::{BinaryOperatorToken, Token, TokenKind, Tokenizer},
};

#[derive(Debug, Clone, Copy)]
pub enum DiagnosticError {
    UnknownError,
    ExpectedNumber { got: TokenKind },
    UnexpectedToken { expected: TokenKind, got: TokenKind },
    ExpectedBinaryOperator { got: TokenKind },
    InvalidNumber,
}

#[derive(Debug, Clone, Copy)]
pub enum DiagnosticKind {
    Error(DiagnosticError),
}

impl From<DiagnosticError> for DiagnosticKind {
    fn from(value: DiagnosticError) -> Self {
        Self::Error(value)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub offset: u32,
    pub len: u32,
}

#[derive(Debug)]
pub struct ParserOutput {
    pub syntax_tree: Option<SyntaxTree>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug)]
struct ParserStackNode {
    len: u32,
    children: Vec<SyntaxNode>,
}

pub struct Parser<'a> {
    input: Peekable<Tokenizer<'a>>,
    diagnostics: Vec<Diagnostic>,
    node_stack: Vec<ParserStackNode>,
}

type ParseResult<T = ()> = Result<T, ()>;

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: Tokenizer::new(input).into_iter().peekable(),
            diagnostics: Vec::new(),
            node_stack: Vec::new(),
        }
    }

    pub fn parse(mut self) -> ParserOutput {
        self.push_node();

        let leading_whitespace = self.consume_whitespace();
        if let Err(()) = self.parse_expression() {
            self.diagnostic(DiagnosticError::UnknownError);
        }
        let next_token_kind = self.peek().kind;
        if next_token_kind != TokenKind::EndOfInput {
            self.diagnostic(DiagnosticError::UnexpectedToken {
                expected: TokenKind::EndOfInput,
                got: next_token_kind,
            });
        }
        let Some(root_node) = self.pop_node(SyntaxNodeKind::Root) else {
            return ParserOutput {
                syntax_tree: None,
                diagnostics: self.diagnostics,
            };
        };

        ParserOutput {
            syntax_tree: Some(SyntaxTree {
                root_node,
                leading_whitespace,
            }),
            diagnostics: self.diagnostics,
        }
    }
}

/// Utility methods
impl<'a> Parser<'a> {
    fn current_offset(&self) -> u32 {
        let mut offset = 0u32;
        for node in &self.node_stack {
            offset += node.len;
            for child in &node.children {
                offset += child.len;
            }
        }
        offset
    }

    fn diagnostic(&mut self, kind: impl Into<DiagnosticKind>) {
        let kind = kind.into();
        let offset = self.current_offset();
        let len = 1;
        self.diagnostics.push(Diagnostic { kind, offset, len });
    }

    fn peek(&mut self) -> Token<'a> {
        self.input.peek().copied().unwrap_or(Token {
            kind: TokenKind::EndOfInput,
            text: "",
        })
    }

    fn consume(&mut self) -> Token<'a> {
        let token = self.input.next().unwrap_or(Token {
            kind: TokenKind::EndOfInput,
            text: "",
        });
        let len: u32 = token.text.len().try_into().expect("Token too long");
        self.node_stack.last_mut().expect("Should have a node").len += len;
        token
    }

    fn consume_if<T, F: FnOnce(Token<'a>) -> Option<T>>(
        &mut self,
        predicate: F,
    ) -> Result<T, Token<'a>> {
        let received = self.peek();
        if let Some(result) = predicate(received) {
            self.consume();
            Ok(result)
        } else {
            Err(received)
        }
    }

    fn consume_whitespace(&mut self) -> u32 {
        match self.consume_if(|token| (token.kind == TokenKind::Whitespace).then_some(token)) {
            Ok(token) => token.text.len().try_into().expect("Token too long"),
            Err(_) => 0,
        }
    }

    fn expect(&mut self, kind: TokenKind) -> ParseResult<Token<'a>> {
        match self.consume_if(|token| (token.kind == kind).then_some(token)) {
            Ok(token) => Ok(token),
            Err(token) => {
                let kind = token.kind;
                self.diagnostic(DiagnosticError::UnexpectedToken {
                    expected: kind,
                    got: kind,
                });
                Err(())
            }
        }
    }

    fn push_node(&mut self) -> &mut ParserStackNode {
        self.node_stack.push(ParserStackNode {
            len: 0,
            children: Vec::new(),
        });
        self.node_stack.last_mut().expect("Was just pushed")
    }

    fn pop_node(&mut self, kind: SyntaxNodeKind) -> Option<SyntaxNode> {
        let Some(ParserStackNode { children, len }) = self.node_stack.pop() else {
            return None;
        };

        let trailing_whitespace = self.consume_whitespace();

        Some(SyntaxNode {
            kind,
            len: len + children.iter().map(|child| child.len).sum::<u32>(),
            trailing_whitespace,
            children: children.into_boxed_slice(),
        })
    }

    fn finish_node(&mut self, kind: SyntaxNodeKind) {
        let Some(node) = self.pop_node(kind) else {
            panic!("Expected node")
        };
        let parent = self.node_stack.last_mut().expect("Parent should exist");
        parent.children.push(node);
    }

    fn finish_node_with_error(&mut self) {
        self.finish_node(SyntaxNodeKind::Error);
    }
}

/// Parsing methods
impl Parser<'_> {
    fn parse_expression(&mut self) -> ParseResult {
        self.parse_expression_pratt(0)
    }

    fn parse_expression_pratt(&mut self, mut min_binding_power: u32) -> ParseResult {
        self.push_node();

        let is_parenthesis = self.peek().kind == TokenKind::ParensOpen;
        if is_parenthesis {
            self.consume();
            min_binding_power = 0;
        }

        self.parse_expression_pratt_inner(min_binding_power)?;

        if is_parenthesis {
            self.expect(TokenKind::ParensClose)?;
        }

        self.finish_node(SyntaxNodeKind::Expression);

        Ok(())
    }

    fn parse_expression_pratt_inner(&mut self, min_binding_power: u32) -> ParseResult {
        self.parse_value()?;

        loop {
            let Some(operator) = self.peek().kind.to_binary_operator() else {
                return Ok(());
            };
            let binding_power = operator.binding_power();
            if binding_power < min_binding_power {
                break;
            }

            let node = self
                .pop_node(SyntaxNodeKind::Expression)
                .expect("Node was started");
            let parent = self.push_node();
            parent.children.push(node);

            self.parse_binary_operator()?;
            self.parse_expression_pratt(binding_power)?;
        }

        Ok(())
    }

    fn parse_binary_operator(&mut self) -> ParseResult<BinaryOperatorToken> {
        self.push_node();

        let op = match self.consume_if(|token| token.kind.to_binary_operator()) {
            Ok(op) => op,
            Err(token) => {
                self.diagnostic(DiagnosticError::ExpectedBinaryOperator { got: token.kind });
                self.finish_node_with_error();
                return Err(());
            }
        };

        self.finish_node(SyntaxNodeKind::BinaryOperator(op));

        Ok(op)
    }

    fn parse_value(&mut self) -> ParseResult {
        self.push_node();

        let value = match self.consume() {
            Token {
                kind: TokenKind::Number,
                text,
            } => {
                let Ok(value) = text.parse() else {
                    self.diagnostic(DiagnosticError::InvalidNumber);
                    self.finish_node_with_error();
                    return Err(());
                };
                value
            }
            other => {
                let kind = other.kind;
                self.diagnostic(DiagnosticError::ExpectedNumber { got: kind });
                self.finish_node_with_error();
                return Err(());
            }
        };

        self.finish_node(SyntaxNodeKind::Number(value));

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use crate::auryn::parser::{Parser, ParserOutput};

    fn parse(input: &str) -> ParserOutput {
        Parser::new(input).parse()
    }

    struct AnnotatedParserOutput<'a> {
        output: ParserOutput,
        input: &'a str,
    }

    impl Debug for AnnotatedParserOutput<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let ParserOutput {
                syntax_tree: ast,
                diagnostics,
            } = &self.output;
            f.debug_struct("AnnotatedParserOutput")
                .field("ast", &ast.as_ref().map(|ast| ast.display(self.input)))
                .field("diagnostics", diagnostics)
                .finish()
        }
    }

    fn verify(input: &str) -> impl Debug {
        let output = parse(input);
        AnnotatedParserOutput { output, input }
    }

    #[test]
    fn test_parse_expression() {
        insta::assert_debug_snapshot!(verify("1 + 2 * 3"));
        insta::assert_debug_snapshot!(verify("1 * 2 + 3"));
    }

    #[test]
    fn test_parse_parenthesis() {
        insta::assert_debug_snapshot!(verify("(3)"));
        insta::assert_debug_snapshot!(verify("1 + (2 + 3)"));
        insta::assert_debug_snapshot!(verify("1 * (2 + 3)"));
    }

    #[test]
    fn test_reject_extra_input() {
        insta::assert_debug_snapshot!(verify("1 1 1 1 11 1"));
    }

    #[test]
    fn test_reject_large_number() {
        insta::assert_debug_snapshot!(verify("9999999999999"));
    }
}
