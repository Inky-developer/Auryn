use std::{cell::Cell, fmt::Debug, ops::Deref, rc::Rc};

use crate::auryn::{
    diagnostic::{DiagnosticError, DiagnosticKind},
    file_id::FileId,
    syntax_id::SyntaxId,
    syntax_tree::{ErrorNode, SyntaxItem, SyntaxNode, SyntaxNodeKind, SyntaxToken, SyntaxTree},
    tokenizer::{Token, TokenKind, Tokenizer},
};

#[derive(Debug)]
pub struct ParserOutput {
    pub syntax_tree: Option<SyntaxTree>,
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
    input: Vec<Token<'a>>,
    file_id: FileId,
    index: usize,
    node_stack: Vec<ParserStackNode>,
    skipped_frames: ParserSkippedFrames,
}

type ParseResult<T = ()> = Result<T, ()>;

impl<'a> Parser<'a> {
    pub fn new(file_id: FileId, input: &'a str) -> Self {
        Self {
            input: Tokenizer::new(input).collect(),
            file_id,
            index: 0,
            node_stack: Vec::new(),
            skipped_frames: ParserSkippedFrames(Rc::new(Cell::new(0))),
        }
    }

    pub fn parse(mut self) -> ParserOutput {
        let watcher = self.push_node();

        self.consume_whitespace();
        // Nothing left to recover if there is an error
        let _ = self.parse_block(|kind| kind == TokenKind::EndOfInput);
        let _ = self.peek_expect(TokenKind::EndOfInput);
        let Some(mut root_node) = self.pop_node(watcher, SyntaxNodeKind::Root) else {
            return ParserOutput { syntax_tree: None };
        };

        root_node.assign_ids(SyntaxId::NUMBER_RANGE);
        ParserOutput {
            syntax_tree: Some(SyntaxTree { root_node }),
        }
    }
}

/// Utility methods
impl<'a> Parser<'a> {
    fn peek(&self) -> Token<'a> {
        self.input.get(self.index).copied().unwrap_or(Token {
            kind: TokenKind::EndOfInput,
            text: "",
        })
    }

    fn multipeek<const N: usize>(&self) -> [TokenKind; N] {
        let mut iter = self
            .input
            .iter()
            .skip(self.index)
            .copied()
            .filter(|token| !matches!(token.kind, TokenKind::Whitespace));
        std::array::from_fn(|_| {
            iter.next()
                .map(|token| token.kind)
                .unwrap_or(TokenKind::EndOfInput)
        })
    }

    fn push_error(&mut self, diagnostic: impl Into<DiagnosticKind>) {
        let node = self.node_stack.last_mut().expect("Should have a node");
        node.children.push(SyntaxItem::Error(Box::new(ErrorNode {
            id: SyntaxId::new_unset(self.file_id),
            text: "".into(),
            diagnostic: diagnostic.into(),
        })));
    }

    fn consume(&mut self) -> Token<'a> {
        let token = self.input.get(self.index).copied().unwrap_or(Token {
            kind: TokenKind::EndOfInput,
            text: "",
        });
        self.index = usize::min(self.input.len(), self.index + 1);
        let node = self.node_stack.last_mut().expect("Should have a node");
        node.children.push(SyntaxItem::Token(SyntaxToken {
            id: SyntaxId::new_unset(self.file_id),
            kind: token.kind,
            text: token.text.into(),
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

    fn consume_whitespace_and_newlines(&mut self) {
        while self
            .consume_if(|token| {
                matches!(token.kind, TokenKind::Whitespace | TokenKind::Newline).then_some(token)
            })
            .is_ok()
        {}
    }

    fn consume_statement_separator(&mut self) -> bool {
        let mut consumed = false;
        loop {
            if !matches!(self.peek().kind, TokenKind::Whitespace | TokenKind::Newline) {
                break;
            }
            self.consume();
            consumed = true;
        }
        consumed || self.peek().kind == TokenKind::EndOfInput
    }

    fn expect(&mut self, expected: TokenKind) -> ParseResult<&'a str> {
        match self.consume_if(|token| (token.kind == expected).then_some(token)) {
            Ok(token) => Ok(token.text),
            Err(token) => {
                let kind = token.kind;
                self.push_error(DiagnosticError::UnexpectedToken {
                    expected,
                    got: kind,
                });
                Err(())
            }
        }
    }

    fn peek_expect(&mut self, expected: TokenKind) -> ParseResult<&'a str> {
        let token = self.peek();
        let kind = token.kind;
        if kind != expected {
            self.push_error(DiagnosticError::UnexpectedToken {
                expected,
                got: kind,
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
        watcher.finish_successfully();
        let ParserStackNode { children } = self.node_stack.pop()?;
        let len = children.iter().map(|child| child.len()).sum::<u32>();

        Some(SyntaxNode {
            id: SyntaxId::new_unset(self.file_id),
            len,
            kind,
            children: children.into_boxed_slice(),
        })
    }

    fn finish_node(&mut self, watcher: ParserFrameWatcher, kind: SyntaxNodeKind) {
        let Some(node) = self.pop_node(watcher, kind) else {
            panic!("Expected node")
        };
        let parent = self.node_stack.last_mut().expect("Parent should exist");
        parent.children.push(SyntaxItem::Node(node));
        self.consume_whitespace();
    }
}

/// Parsing methods
impl Parser<'_> {
    fn parse_block(&mut self, end_set: impl Fn(TokenKind) -> bool) -> ParseResult {
        let watcher = self.push_node();

        loop {
            self.consume_whitespace_and_newlines();
            if end_set(self.peek().kind) {
                break;
            }
            self.parse_statement()?;
            if !self.consume_statement_separator() && !end_set(self.peek().kind) {
                self.push_error(DiagnosticError::ExpectedNewline);
            }
        }

        self.finish_node(watcher, SyntaxNodeKind::Block);

        Ok(())
    }

    fn parse_statement(&mut self) -> ParseResult {
        let watcher = self.push_node();

        match self.peek().kind {
            TokenKind::KeywordLet => self.parse_assignment()?,
            TokenKind::KeywordIf => self.parse_if_statement()?,
            TokenKind::KeywordLoop => self.parse_loop()?,
            TokenKind::KeywordBreak => self.parse_break()?,
            _ => {
                if let [TokenKind::Identifier, TokenKind::Equal] = self.multipeek() {
                    self.parse_variable_update()?;
                } else {
                    self.parse_expression()?
                }
            }
        }

        self.finish_node(watcher, SyntaxNodeKind::Statement);
        Ok(())
    }

    fn parse_assignment(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordLet)?;
        self.consume_whitespace();
        self.expect(TokenKind::Identifier)?;
        self.consume_whitespace();
        self.expect(TokenKind::Equal)?;
        self.consume_whitespace();
        self.parse_expression()?;

        self.finish_node(watcher, SyntaxNodeKind::Assignment);
        Ok(())
    }

    fn parse_if_statement(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordIf)?;
        self.consume_whitespace();
        self.parse_expression()?;
        self.expect(TokenKind::BraceOpen)?;
        self.consume_whitespace();
        self.parse_block(|kind| kind == TokenKind::BraceClose)?;
        self.expect(TokenKind::BraceClose)?;

        self.finish_node(watcher, SyntaxNodeKind::IfStatement);

        Ok(())
    }

    fn parse_loop(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordLoop)?;
        self.consume_whitespace();
        self.expect(TokenKind::BraceOpen)?;
        self.consume_whitespace();
        self.parse_block(|kind| kind == TokenKind::BraceClose)?;
        self.expect(TokenKind::BraceClose)?;

        self.finish_node(watcher, SyntaxNodeKind::Loop);

        Ok(())
    }

    fn parse_break(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordBreak)?;

        self.finish_node(watcher, SyntaxNodeKind::Break);

        Ok(())
    }

    fn parse_variable_update(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::Identifier)?;
        self.consume_whitespace();
        self.expect(TokenKind::Equal)?;
        self.consume_whitespace();
        self.parse_expression()?;

        self.finish_node(watcher, SyntaxNodeKind::VariableUpdate);

        Ok(())
    }

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
                if binding_power <= min_binding_power {
                    break;
                }

                let node = this
                    .pop_node(watcher, SyntaxNodeKind::Expression)
                    .expect("Node was started");
                watcher = this.push_node();
                let parent = this.node_stack.last_mut().expect("Was just pushed");
                parent.children.push(SyntaxItem::Node(node));

                this.parse_binary_operator_token()?;
                this.consume_whitespace();
                this.parse_expression_pratt(binding_power)?;

                let node = this
                    .pop_node(watcher, SyntaxNodeKind::BinaryOperation)
                    .expect("Node was started");
                watcher = this.push_node();
                let parent = this.node_stack.last_mut().expect("Was just pushed");
                parent.children.push(SyntaxItem::Node(node));
            }

            Ok(watcher)
        }

        let watcher = self.push_node();
        let watcher = inner(self, watcher, min_binding_power)?;
        self.finish_node(watcher, SyntaxNodeKind::Expression);

        Ok(())
    }

    fn parse_binary_operator_token(&mut self) -> ParseResult {
        self.consume_if(|token| token.kind.to_binary_operator())
            .map_err(|_| ())?;

        Ok(())
    }

    fn parse_value(&mut self) -> ParseResult {
        let watcher = self.push_node();

        match self.peek().kind {
            TokenKind::Identifier => self.parse_identifier_or_function_call()?,
            TokenKind::Number => self.parse_number()?,
            TokenKind::ParensOpen => self.parse_parenthesis()?,
            other => {
                self.push_error(DiagnosticError::ExpectedValue { got: other });
                return Err(());
            }
        };

        self.finish_node(watcher, SyntaxNodeKind::Value);

        Ok(())
    }

    fn parse_identifier_or_function_call(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::Identifier)?;
        self.consume_whitespace();

        match self.peek().kind {
            TokenKind::ParensOpen => {
                self.expect(TokenKind::ParensOpen)?;
                if let Err(()) = self.parse_argument_list() {
                    // self.recover(|k| matches!(k, TokenKind::ParensClose))?;
                }
                self.expect(TokenKind::ParensClose)?;

                self.finish_node(watcher, SyntaxNodeKind::FunctionCall);
            }
            _ => {
                self.finish_node(watcher, SyntaxNodeKind::Ident);
            }
        }

        Ok(())
    }

    fn parse_argument_list(&mut self) -> ParseResult {
        let watcher = self.push_node();
        loop {
            self.parse_expression()?;
            if self.peek().kind != TokenKind::Comma {
                break;
            }

            self.consume();
        }

        self.finish_node(watcher, SyntaxNodeKind::ArgumentList);

        Ok(())
    }

    fn parse_number(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::Number)?;

        self.finish_node(watcher, SyntaxNodeKind::Number);
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

    use crate::auryn::{
        diagnostic::ComputedDiagnostic,
        file_id::FileId,
        parser::{Parser, ParserOutput},
    };

    fn parse(input: &str) -> ParserOutput {
        Parser::new(FileId::MAIN_FILE, input).parse()
    }

    struct AnnotatedParserOutput<'a> {
        output: ParserOutput,
        diagnostics: Vec<ComputedDiagnostic>,
        input: &'a str,
    }

    impl Debug for AnnotatedParserOutput<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let ParserOutput { syntax_tree: ast } = &self.output;
            let diagnostics = &self.diagnostics;
            f.debug_struct("AnnotatedParserOutput")
                .field("ast", &ast.as_ref().map(|ast| ast.display(self.input)))
                .field("diagnostics", diagnostics)
                .finish()
        }
    }

    fn verify(input: &str) -> impl Debug {
        let output = parse(input);
        let diagnostics = output
            .syntax_tree
            .as_ref()
            .map(|it| it.collect_diagnostics())
            .unwrap_or_default();
        AnnotatedParserOutput {
            output,
            input,
            diagnostics,
        }
    }

    #[test]
    fn test_parse_expression() {
        insta::assert_debug_snapshot!(verify("1 + 2 * 3"));
        insta::assert_debug_snapshot!(verify("1 * 2 + 3"));
    }

    #[test]
    fn test_parse_large_number() {
        insta::assert_debug_snapshot!(verify("9999999999999"));
    }

    #[test]
    fn test_parse_parenthesis() {
        insta::assert_debug_snapshot!(verify("(3)"));
        insta::assert_debug_snapshot!(verify("((3))"));
        insta::assert_debug_snapshot!(verify("1 + (2 + 3)"));
        insta::assert_debug_snapshot!(verify("1 * (2 + 3)"));
    }

    #[test]
    fn test_parse_function_call() {
        insta::assert_debug_snapshot!(verify("print(1)"));
        insta::assert_debug_snapshot!(verify("print(1)\n"));
    }

    #[test]
    fn test_parse_assignment() {
        insta::assert_debug_snapshot!(verify("let helloworld = 1\n"));
    }

    #[test]
    fn test_variable() {
        insta::assert_debug_snapshot!(verify("print(hello)"));
    }

    #[test]
    fn test_variable_update() {
        insta::assert_debug_snapshot!(verify("a = 3"));
    }

    #[test]
    fn test_comparison() {
        insta::assert_debug_snapshot!(verify("1 == 1"));
    }

    #[test]
    fn test_if_statemt() {
        insta::assert_debug_snapshot!(verify("if 1 { print(42) }"));
    }

    #[test]
    fn test_loop() {
        insta::assert_debug_snapshot!(verify("loop { 1 + 2 }"));
        insta::assert_debug_snapshot!(verify("loop { break }"));
        insta::assert_debug_snapshot!(verify("\t\nloop {\t\n\t\n\tprint(1)\t\n\t}\t\n\t"))
    }

    #[test]
    fn test_multiple_statements() {
        insta::assert_debug_snapshot!(verify("let a = 1\nlet b = 2"))
    }

    #[test]
    fn test_reject_extra_input() {
        insta::assert_debug_snapshot!(verify("1 1 1 1 11 1"));
    }
}
