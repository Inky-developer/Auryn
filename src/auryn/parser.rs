use std::{cell::Cell, fmt::Debug, ops::Deref, rc::Rc};

use crate::auryn::{
    diagnostic::{Diagnostic, DiagnosticError, DiagnosticKind},
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

    pub fn parse(self) -> ParserOutput {
        self.parse_with(Self::parse_file)
    }

    pub fn parse_statements(self) -> ParserOutput {
        self.parse_with(|this| this.parse_block(|it| it == TokenKind::EndOfInput))
    }

    fn parse_with(mut self, parse: impl FnOnce(&mut Self) -> ParseResult) -> ParserOutput {
        let watcher = self.push_node();

        self.consume_whitespace();
        // Nothing left to recover if there is an error
        let _ = parse(&mut self);
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

    #[track_caller]
    fn push_error(&mut self, diagnostic: impl Into<DiagnosticKind>) {
        let node = self.node_stack.last_mut().expect("Should have a node");
        let id = SyntaxId::new_unset(Some(self.file_id));
        node.children.push(SyntaxItem::Error(Box::new(ErrorNode {
            id,
            text: "".into(),
            diagnostic: Diagnostic::new(id, diagnostic),
        })));
    }

    /// Consumes a single token and does nothing else, unlike [`Self::consume`], which also
    /// consumes whitespace after the consumed token
    fn consume_single(&mut self) -> Token<'a> {
        let token = self.input.get(self.index).copied().unwrap_or(Token {
            kind: TokenKind::EndOfInput,
            text: "",
        });
        self.index = usize::min(self.input.len(), self.index + 1);
        let node = self.node_stack.last_mut().expect("Should have a node");
        node.children.push(SyntaxItem::Token(SyntaxToken {
            id: SyntaxId::new_unset(Some(self.file_id)),
            kind: token.kind,
            text: token.text.into(),
        }));
        token
    }

    /// Like [`Self::consume_no_whitespace`], but also consume whitespace after the consumed token
    fn consume(&mut self) -> Token<'a> {
        let result = self.consume_single();
        self.consume_whitespace();
        result
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

    fn consume_whitespace(&mut self) {
        if self.peek().kind == TokenKind::Whitespace {
            self.consume_single();
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

    #[track_caller]
    fn expect(&mut self, expected: TokenKind) -> ParseResult<&'a str> {
        match self.consume_if(|token| (token.kind == expected).then_some(token)) {
            Ok(token) => Ok(token.text),
            Err(token) => {
                let got = token.text.into();
                self.push_error(DiagnosticError::UnexpectedToken {
                    expected: expected.as_str(),
                    got,
                });
                Err(())
            }
        }
    }

    #[track_caller]
    fn peek_expect(&mut self, expected: TokenKind) -> ParseResult<&'a str> {
        let token = self.peek();
        let kind = token.kind;
        if kind != expected {
            let got = token.text.into();
            self.push_error(DiagnosticError::UnexpectedToken {
                expected: expected.as_str(),
                got,
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
            id: SyntaxId::new_unset(Some(self.file_id)),
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

    fn parse_newline_separated(
        &mut self,
        end_set: impl Fn(TokenKind) -> bool,
        parse: impl Fn(&mut Self) -> ParseResult,
    ) -> ParseResult {
        loop {
            self.consume_whitespace_and_newlines();
            if end_set(self.peek().kind) {
                break;
            }
            parse(self)?;
            if !self.consume_statement_separator() && !end_set(self.peek().kind) {
                self.push_error(DiagnosticError::ExpectedNewline);
            }
        }

        Ok(())
    }
}

/// Parsing methods
impl Parser<'_> {
    fn parse_file(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.parse_newline_separated(|kind| kind == TokenKind::EndOfInput, Self::parse_item)?;

        self.finish_node(watcher, SyntaxNodeKind::File);
        Ok(())
    }

    fn parse_item(&mut self) -> ParseResult {
        let watcher = self.push_node();

        match self.multipeek() {
            [TokenKind::KeywordFn, _] => self.parse_function_definition()?,
            [TokenKind::KeywordUnsafe, TokenKind::KeywordExtern] => self.parse_extern_block()?,
            [_, _] => {
                let got = self.peek().text.into();
                self.push_error(DiagnosticError::ExpectedItem { got });
                return Err(());
            }
        }

        self.finish_node(watcher, SyntaxNodeKind::Item);
        Ok(())
    }

    fn parse_extern_block(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordUnsafe)?;
        self.expect(TokenKind::KeywordExtern)?;
        self.expect(TokenKind::StringLiteral)?;

        self.expect(TokenKind::BraceOpen)?;
        self.parse_newline_separated(
            |kind| kind == TokenKind::BraceClose,
            Self::parse_extern_item,
        )?;
        self.expect(TokenKind::BraceClose)?;

        self.finish_node(watcher, SyntaxNodeKind::ExternBlock);
        Ok(())
    }

    fn parse_extern_item(&mut self) -> ParseResult {
        let watcher = self.push_node();

        if self.peek().kind == TokenKind::BracketOpen {
            self.parse_item_metadata()?;
            self.consume_whitespace_and_newlines();
        }

        let inner = self.push_node();
        match self.peek().kind {
            TokenKind::KeywordType => self.parse_extern_type()?,
            _ => {
                let got = self.peek().text.into();
                self.push_error(DiagnosticError::ExpectedExternItem { got });
                return Err(());
            }
        }
        self.finish_node(inner, SyntaxNodeKind::ExternBlockItemKind);

        self.finish_node(watcher, SyntaxNodeKind::ExternBlockItem);

        Ok(())
    }

    fn parse_extern_type(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordType)?;
        self.expect(TokenKind::Identifier)?;

        self.parse_extern_type_body()?;

        self.finish_node(watcher, SyntaxNodeKind::ExternType);
        Ok(())
    }

    fn parse_extern_type_body(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::BraceOpen)?;
        self.parse_newline_separated(
            |kind| kind == TokenKind::BraceClose,
            Self::parse_extern_type_body_item,
        )?;
        self.expect(TokenKind::BraceClose)?;

        self.finish_node(watcher, SyntaxNodeKind::ExternTypeBody);
        Ok(())
    }

    fn parse_extern_type_body_item(&mut self) -> ParseResult {
        let watcher = self.push_node();

        if self.peek().kind == TokenKind::BracketOpen {
            self.parse_item_metadata()?;
            self.consume_whitespace_and_newlines();
        }

        let inner_watcher = self.push_node();
        match self.peek().kind {
            TokenKind::KeywordStatic => self.parse_extern_type_body_static_let()?,
            _ => {
                self.push_error(DiagnosticError::ExpectedExternTypeBodyItem {
                    got: self.peek().text.into(),
                });
                return Err(());
            }
        }
        self.finish_node(inner_watcher, SyntaxNodeKind::ExternTypeBodyItemKind);

        self.finish_node(watcher, SyntaxNodeKind::ExternTypeBodyItem);
        Ok(())
    }

    fn parse_extern_type_body_static_let(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordStatic)?;
        self.expect(TokenKind::KeywordLet)?;
        self.expect(TokenKind::Identifier)?;
        self.expect(TokenKind::Colon)?;
        self.parse_type()?;

        self.finish_node(watcher, SyntaxNodeKind::ExternTypeStaticLet);
        Ok(())
    }

    fn parse_item_metadata(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::BracketOpen)?;
        self.expect(TokenKind::StringLiteral)?;
        self.expect(TokenKind::BracketClose)?;

        self.finish_node(watcher, SyntaxNodeKind::ItemMetadata);
        Ok(())
    }

    fn parse_function_definition(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordFn)?;
        self.expect(TokenKind::Identifier)?;
        self.parse_parameter_list()?;
        if self.peek().kind == TokenKind::Arrow {
            self.parse_return_type()?;
        }
        self.expect(TokenKind::BraceOpen)?;
        self.parse_block(|it| it == TokenKind::BraceClose)?;
        self.expect(TokenKind::BraceClose)?;

        self.finish_node(watcher, SyntaxNodeKind::FunctionDefinition);
        Ok(())
    }

    fn parse_parameter_list(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::ParensOpen)?;
        loop {
            if self.peek().kind == TokenKind::ParensClose {
                break;
            }
            self.parse_parameter_definition()?;
            if self.peek().kind != TokenKind::Comma {
                break;
            }
            self.expect(TokenKind::Comma)?;
        }
        self.expect(TokenKind::ParensClose)?;

        self.finish_node(watcher, SyntaxNodeKind::ParameterList);
        Ok(())
    }

    fn parse_return_type(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::Arrow)?;
        self.parse_type()?;

        self.finish_node(watcher, SyntaxNodeKind::ReturnType);
        Ok(())
    }

    fn parse_parameter_definition(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::Identifier)?;
        self.expect(TokenKind::Colon)?;
        self.parse_type()?;

        self.finish_node(watcher, SyntaxNodeKind::ParameterDefinition);
        Ok(())
    }

    fn parse_type(&mut self) -> ParseResult {
        let watcher = self.push_node();

        match self.peek().kind {
            TokenKind::BracketOpen => self.parse_array_type()?,
            TokenKind::Identifier => self.parse_identifier()?,
            _ => {
                self.push_error(DiagnosticError::ExpectedType {
                    got: self.peek().text.into(),
                });
                return Err(());
            }
        }

        self.finish_node(watcher, SyntaxNodeKind::Type);
        Ok(())
    }

    fn parse_array_type(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::BracketOpen)?;
        self.expect(TokenKind::BracketClose)?;
        self.parse_type()?;

        self.finish_node(watcher, SyntaxNodeKind::ArrayType);
        Ok(())
    }

    fn parse_block(&mut self, end_set: impl Fn(TokenKind) -> bool) -> ParseResult {
        let watcher = self.push_node();

        self.parse_newline_separated(end_set, Self::parse_statement)?;

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
            TokenKind::KeywordReturn => self.parse_return()?,
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
        self.expect(TokenKind::Identifier)?;
        self.expect(TokenKind::Equal)?;
        self.parse_expression()?;

        self.finish_node(watcher, SyntaxNodeKind::Assignment);
        Ok(())
    }

    fn parse_if_statement(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordIf)?;
        self.parse_expression()?;
        self.expect(TokenKind::BraceOpen)?;
        self.parse_block(|kind| kind == TokenKind::BraceClose)?;
        self.expect(TokenKind::BraceClose)?;

        self.finish_node(watcher, SyntaxNodeKind::IfStatement);
        Ok(())
    }

    fn parse_loop(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordLoop)?;
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

    fn parse_return(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordReturn)?;
        if is_expression_start(self.peek().kind) {
            self.parse_expression()?;
        }

        self.finish_node(watcher, SyntaxNodeKind::Return);
        Ok(())
    }

    fn parse_variable_update(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::Identifier)?;
        self.expect(TokenKind::Equal)?;
        self.parse_expression()?;

        self.finish_node(watcher, SyntaxNodeKind::VariableUpdate);
        Ok(())
    }

    fn parse_expression(&mut self) -> ParseResult {
        if !is_expression_start(self.peek().kind) {
            self.push_error(DiagnosticError::ExpectedExpression {
                got: self.peek().text.into(),
            });
            return Err(());
        }
        self.parse_expression_pratt(0)
    }

    fn parse_expression_pratt(&mut self, min_binding_power: u32) -> ParseResult {
        fn inner(
            this: &mut Parser<'_>,
            mut watcher: ParserFrameWatcher,
            min_binding_power: u32,
        ) -> ParseResult<ParserFrameWatcher> {
            this.parse_value_or_postfix()?;

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

    fn parse_value_or_postfix(&mut self) -> ParseResult {
        let mut watcher = self.push_node();

        match self.peek().kind {
            TokenKind::Identifier => self.parse_identifier()?,
            TokenKind::NumberLiteral => self.parse_number()?,
            TokenKind::StringLiteral => self.parse_string_literal()?,
            TokenKind::ParensOpen => self.parse_parenthesis()?,
            _ => {
                self.push_error(DiagnosticError::ExpectedValue {
                    got: self.peek().text.into(),
                });
                return Err(());
            }
        };

        if !is_postfix_start(self.peek().kind) {
            self.finish_node(watcher, SyntaxNodeKind::Value);
            return Ok(());
        };

        let mut current_node_kind = SyntaxNodeKind::Value;
        while is_postfix_start(self.peek().kind) {
            let postfix_node = self.pop_node(watcher, current_node_kind).unwrap();
            watcher = self.push_node();
            current_node_kind = SyntaxNodeKind::PostfixOperation;
            self.node_stack
                .last_mut()
                .unwrap()
                .children
                .push(SyntaxItem::Node(postfix_node));
            self.parse_postfix()?;
        }

        self.finish_node(watcher, SyntaxNodeKind::PostfixOperation);

        Ok(())
    }

    fn parse_postfix(&mut self) -> ParseResult {
        let watcher = self.push_node();

        match self.peek().kind {
            TokenKind::ParensOpen => self.parse_argument_list()?,
            TokenKind::Dot => self.parse_accessor()?,
            other => {
                unreachable!("Cannot parse {other:?} as postfix");
            }
        }

        self.finish_node(watcher, SyntaxNodeKind::PostfixOperator);
        Ok(())
    }

    fn parse_accessor(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::Dot)?;
        self.expect(TokenKind::Identifier)?;

        self.finish_node(watcher, SyntaxNodeKind::Accessor);
        Ok(())
    }

    fn parse_identifier(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::Identifier)?;

        self.finish_node(watcher, SyntaxNodeKind::Ident);
        Ok(())
    }

    fn parse_argument_list(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::ParensOpen)?;
        loop {
            if self.peek().kind == TokenKind::ParensClose {
                break;
            }
            self.parse_expression()?;
            if self.peek().kind != TokenKind::Comma {
                break;
            }
            self.expect(TokenKind::Comma)?;
        }
        self.expect(TokenKind::ParensClose)?;

        self.finish_node(watcher, SyntaxNodeKind::ArgumentList);
        Ok(())
    }

    fn parse_number(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::NumberLiteral)?;

        self.finish_node(watcher, SyntaxNodeKind::NumberLiteral);
        Ok(())
    }

    fn parse_string_literal(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::StringLiteral)?;

        self.finish_node(watcher, SyntaxNodeKind::StringLiteral);
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

/// Not nice, must be in sync with [`Parser::parse_value`].
fn is_expression_start(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Identifier
            | TokenKind::NumberLiteral
            | TokenKind::StringLiteral
            | TokenKind::ParensOpen
    )
}

/// Must be in sync with [`Parser::parse_postfix_op`]
fn is_postfix_start(kind: TokenKind) -> bool {
    matches!(kind, TokenKind::ParensOpen | TokenKind::Dot)
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use crate::auryn::{
        diagnostic::Diagnostic,
        file_id::FileId,
        parser::{Parser, ParserOutput},
    };

    struct AnnotatedParserOutput<'a> {
        output: ParserOutput,
        diagnostics: Vec<Diagnostic>,
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

    fn verify_block(input: &str) -> impl Debug {
        let output = Parser::new(FileId::MAIN_FILE, input).parse_statements();
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

    fn verify(input: &str) -> impl Debug {
        let output = Parser::new(FileId::MAIN_FILE, input).parse();
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
        insta::assert_debug_snapshot!(verify_block("1 + 2 * 3"));
        insta::assert_debug_snapshot!(verify_block("1 * 2 + 3"));
        insta::assert_debug_snapshot!(verify_block("1 * \"test\""));
    }

    #[test]
    fn test_parse_postfix_operator() {
        insta::assert_debug_snapshot!(verify_block("a()"));
        insta::assert_debug_snapshot!(verify_block("a.b"));
        insta::assert_debug_snapshot!(verify_block("a.b()()"));
    }

    #[test]
    fn test_parse_large_number() {
        insta::assert_debug_snapshot!(verify_block("9999999999999"));
    }

    #[test]
    fn test_parse_parenthesis() {
        insta::assert_debug_snapshot!(verify_block("(3)"));
        insta::assert_debug_snapshot!(verify_block("((3))"));
        insta::assert_debug_snapshot!(verify_block("1 + (2 + 3)"));
        insta::assert_debug_snapshot!(verify_block("1 * (2 + 3)"));
    }

    #[test]
    fn test_parse_function_call() {
        insta::assert_debug_snapshot!(verify_block("print(1)"));
        insta::assert_debug_snapshot!(verify_block("print(1)\n"));
        insta::assert_debug_snapshot!(verify_block("print()"));
    }

    #[test]
    fn test_parse_assignment() {
        insta::assert_debug_snapshot!(verify_block("let helloworld = 1\n"));
    }

    #[test]
    fn test_variable() {
        insta::assert_debug_snapshot!(verify_block("print(hello)"));
    }

    #[test]
    fn test_variable_update() {
        insta::assert_debug_snapshot!(verify_block("a = 3"));
    }

    #[test]
    fn test_comparison() {
        insta::assert_debug_snapshot!(verify_block("1 == 1"));
    }

    #[test]
    fn test_if_statemt() {
        insta::assert_debug_snapshot!(verify_block("if 1 { print(42) }"));
    }

    #[test]
    fn test_loop() {
        insta::assert_debug_snapshot!(verify_block("loop { 1 + 2 }"));
        insta::assert_debug_snapshot!(verify_block("loop { break }"));
        insta::assert_debug_snapshot!(verify_block("\t\nloop {\t\n\t\n\tprint(1)\t\n\t}\t\n\t"))
    }

    #[test]
    fn test_function() {
        insta::assert_debug_snapshot!(verify(
            "fn foo(a: Int, b: []String,) -> Null { print(9000) }"
        ));
    }

    #[test]
    fn test_return() {
        insta::assert_debug_snapshot!(verify("fn foo() { return }"));
        insta::assert_debug_snapshot!(verify("fn foo() { return 15 * 15 }"));
    }

    #[test]
    fn test_multiple_statements() {
        insta::assert_debug_snapshot!(verify_block("let a = 1\nlet b = 2"))
    }

    #[test]
    fn test_extern_items() {
        insta::assert_debug_snapshot!(verify(
            "unsafe extern \"java\" {\n[\"java/lang/Foo\"]\ntype Foo { [\"bar\"] static let bar: Int }\n}"
        ));
    }

    #[test]
    fn test_reject_extra_input() {
        insta::assert_debug_snapshot!(verify_block("1 1 1 1 11 1"));
    }
}
