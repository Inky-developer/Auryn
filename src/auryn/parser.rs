use std::{cell::Cell, iter::Peekable, ops::Deref, rc::Rc};

use crate::auryn::{
    Span,
    syntax_tree::{SyntaxItem, SyntaxNode, SyntaxNodeKind, SyntaxToken, SyntaxTree},
    tokenizer::{BinaryOperatorToken, Token, TokenKind, Tokenizer},
};

#[derive(Debug, Clone, Copy)]
pub enum DiagnosticError {
    ExpectedNumber { got: TokenKind },
    UnexpectedToken { expected: TokenKind, got: TokenKind },
    ExpectedBinaryOperator { got: TokenKind },
    InvalidNumber,
    ExpectedValue { got: TokenKind },
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
    children: Vec<SyntaxItem>,
}

#[derive(Debug, Clone)]
struct ParserSkippedFrames(Rc<Cell<u32>>);

impl ParserSkippedFrames {
    fn get_watcher(&self) -> ParserFrameWatcher {
        ParserFrameWatcher(self.clone())
    }
}

impl Deref for ParserSkippedFrames {
    type Target = Cell<u32>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Type to keep track of current frames in the parser that automatically marks them as exited with error, in case
/// an error happened.
#[derive(Debug)]
struct ParserFrameWatcher(ParserSkippedFrames);

impl ParserFrameWatcher {
    fn finish_successfully(self) {
        let Self(_) = self;
    }
}

/// When this is dropped, it means that the current frame was not finished without error.
/// In this case, mark the latest frame as error.
impl Drop for ParserFrameWatcher {
    fn drop(&mut self) {
        self.0.update(|dropped_frames| dropped_frames + 1);
    }
}

pub struct Parser<'a> {
    input: Peekable<Tokenizer<'a>>,
    diagnostics: Vec<Diagnostic>,
    node_stack: Vec<ParserStackNode>,
    skipped_frames: ParserSkippedFrames,
}

type ParseResult<T = ()> = Result<T, ()>;

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: Tokenizer::new(input).into_iter().peekable(),
            diagnostics: Vec::new(),
            node_stack: Vec::new(),
            skipped_frames: ParserSkippedFrames(Rc::new(Cell::new(0))),
        }
    }

    pub fn parse(mut self) -> ParserOutput {
        let watcher = self.push_node();

        self.consume_whitespace();
        // Nothing left to recover if there is an error
        let _ = self.parse_expression();

        let next_token_kind = self.peek().kind;
        if next_token_kind != TokenKind::EndOfInput {
            self.diagnostic(DiagnosticError::UnexpectedToken {
                expected: TokenKind::EndOfInput,
                got: next_token_kind,
            });
        }
        let Some(root_node) = self.pop_node(watcher, SyntaxNodeKind::Root) else {
            return ParserOutput {
                syntax_tree: None,
                diagnostics: self.diagnostics,
            };
        };

        ParserOutput {
            syntax_tree: Some(SyntaxTree { root_node }),
            diagnostics: self.diagnostics,
        }
    }
}

/// Utility methods
impl<'a> Parser<'a> {
    fn current_offset(&self) -> u32 {
        let mut offset = 0u32;
        for node in &self.node_stack {
            for child in &node.children {
                offset += child.span().len;
            }
        }
        offset
    }

    fn diagnostic(&mut self, kind: impl Into<DiagnosticKind>) {
        let kind = kind.into();
        let offset = self.current_offset();
        let len = self.peek().text.len().try_into().expect("Token too long");
        self.consume();
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
        let node = self.node_stack.last_mut().expect("Should have a node");
        node.children.push(SyntaxItem::Token(SyntaxToken {
            kind: token.kind,
            span: Span { len },
        }));
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

    fn expect(&mut self, expected: TokenKind) -> ParseResult<&'a str> {
        match self.consume_if(|token| (token.kind == expected).then_some(token)) {
            Ok(token) => Ok(token.text),
            Err(token) => {
                let kind = token.kind;
                self.diagnostic(DiagnosticError::UnexpectedToken {
                    expected,
                    got: kind,
                });
                Err(())
            }
        }
    }

    fn peek_expect(&mut self, expected: TokenKind) -> ParseResult<&'a str> {
        let token = self.peek();
        if token.kind != expected {
            self.diagnostic(DiagnosticError::UnexpectedToken {
                expected,
                got: token.kind,
            });
            return Err(());
        }

        Ok(token.text)
    }

    #[must_use]
    fn push_node(&mut self) -> ParserFrameWatcher {
        self.node_stack.push(ParserStackNode {
            children: Vec::new(),
        });
        self.skipped_frames.get_watcher()
    }

    #[must_use]
    fn pop_node(
        &mut self,
        watcher: ParserFrameWatcher,
        kind: SyntaxNodeKind,
    ) -> Option<SyntaxNode> {
        self.consume_whitespace();
        watcher.finish_successfully();
        let Some(ParserStackNode { children }) = self.node_stack.pop() else {
            return None;
        };

        let span = children.iter().map(|child| child.span()).sum::<Span>();

        Some(SyntaxNode {
            kind,
            span,
            children: children.into_boxed_slice(),
        })
    }

    fn finish_node(&mut self, watcher: ParserFrameWatcher, kind: SyntaxNodeKind) {
        let Some(node) = self.pop_node(watcher, kind) else {
            panic!("Expected node")
        };
        let parent = self.node_stack.last_mut().expect("Parent should exist");
        parent.children.push(SyntaxItem::Node(node));
    }
}

/// Parsing methods
impl Parser<'_> {
    fn parse_expression(&mut self) -> ParseResult {
        self.parse_expression_pratt(0)
    }

    fn parse_expression_pratt(&mut self, min_binding_power: u32) -> ParseResult {
        fn inner(
            this: &mut Parser<'_>,
            mut watcher: ParserFrameWatcher,
            min_binding_power: u32,
        ) -> ParseResult<ParserFrameWatcher> {
            this.parse_value()?;

            loop {
                let Some(operator) = this.peek().kind.to_binary_operator() else {
                    break;
                };
                let binding_power = operator.binding_power();
                if binding_power < min_binding_power {
                    break;
                }

                let node = this
                    .pop_node(watcher, SyntaxNodeKind::Expression)
                    .expect("Node was started");
                watcher = this.push_node();
                let parent = this.node_stack.last_mut().expect("Was just pushed");
                parent.children.push(SyntaxItem::Node(node));

                this.parse_binary_operator()?;
                this.parse_expression_pratt(binding_power)?;
            }

            Ok(watcher)
        }

        let watcher = self.push_node();
        let watcher = inner(self, watcher, min_binding_power)?;
        self.finish_node(watcher, SyntaxNodeKind::Expression);

        Ok(())
    }

    fn parse_binary_operator(&mut self) -> ParseResult<BinaryOperatorToken> {
        let watcher = self.push_node();
        let op = self
            .consume_if(|token| token.kind.to_binary_operator())
            .map_err(|_| ())?;
        self.finish_node(watcher, SyntaxNodeKind::BinaryOperator(op));

        Ok(op)
    }

    fn parse_value(&mut self) -> ParseResult {
        match self.peek().kind {
            TokenKind::Identifier => self.parse_identifier_or_function_call()?,
            TokenKind::Number => self.parse_number()?,
            TokenKind::ParensOpen => self.parse_parenthesis()?,
            other => {
                self.diagnostic(DiagnosticError::ExpectedValue { got: other });
                return Err(());
            }
        };

        Ok(())
    }

    fn parse_identifier_or_function_call(&mut self) -> ParseResult {
        let watcher = self.push_node();

        let text = self.expect(TokenKind::Identifier)?;
        self.expect(TokenKind::ParensOpen)?;
        if let Err(()) = self.parse_parameter_list() {
            // self.recover(|k| matches!(k, TokenKind::ParensClose))?;
        }
        self.expect(TokenKind::ParensClose)?;

        self.finish_node(watcher, SyntaxNodeKind::FunctionCall(text.to_string()));

        Ok(())
    }

    fn parse_parameter_list(&mut self) -> ParseResult {
        Ok(())
    }

    fn parse_number(&mut self) -> ParseResult {
        let watcher = self.push_node();

        let text = self.peek_expect(TokenKind::Number)?;
        let Ok(value) = text.parse() else {
            self.diagnostic(DiagnosticError::InvalidNumber);
            return Err(());
        };
        self.consume();

        self.finish_node(watcher, SyntaxNodeKind::Number(value));
        Ok(())
    }

    fn parse_parenthesis(&mut self) -> ParseResult {
        let watcher = self.push_node();
        self.expect(TokenKind::ParensOpen)?;
        self.parse_expression()?;
        self.expect(TokenKind::ParensClose)?;
        self.finish_node(watcher, SyntaxNodeKind::Parenthesis);
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
        insta::assert_debug_snapshot!(verify("((3))"));
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
