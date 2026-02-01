use std::fmt::Debug;

use stdx::{bitset, default};

use crate::auryn::{
    diagnostic::{Diagnostic, DiagnosticError, DiagnosticKind},
    file_id::FileId,
    syntax_id::SyntaxId,
    syntax_tree::{ErrorNode, SyntaxItem, SyntaxNode, SyntaxNodeKind, SyntaxToken, SyntaxTree},
    tokenizer::{Token, TokenKind, TokenSet, Tokenizer, UpdateOperatorToken},
};

#[derive(Debug)]
pub struct ParserOutput {
    pub syntax_tree: SyntaxTree,
}

#[derive(Debug, Clone, Copy)]
struct ParseOptions {
    newlines_are_whitespace: bool,
    allow_named_struct_literals: bool,
}

impl Default for ParseOptions {
    fn default() -> Self {
        Self {
            newlines_are_whitespace: false,
            allow_named_struct_literals: true,
        }
    }
}

#[derive(Debug)]
struct ParserStackNode {
    children: Vec<SyntaxItem>,
}

/// Type to keep track of current frames in the parser that automatically marks them as exited with error, in case
/// an error happened.
#[must_use]
#[derive(Debug)]
struct ParserFrameWatcher {
    stack_level: usize,
}

pub struct Parser<'a> {
    input: Vec<Token<'a>>,
    file_id: FileId,
    index: usize,
    node_stack: Vec<ParserStackNode>,
    option_stack: Vec<(usize, ParseOptions)>,
    recovery_stack: Vec<TokenSet>,
}

type ParseResult<T = ()> = Result<T, ()>;

impl<'a> Parser<'a> {
    pub fn new(file_id: FileId, input: &'a str) -> Self {
        Self {
            input: Tokenizer::new(input).collect(),
            file_id,
            index: 0,
            node_stack: default(),
            option_stack: default(),
            recovery_stack: default(),
        }
    }

    pub fn parse(self) -> ParserOutput {
        self.parse_with(Self::parse_file)
    }

    pub fn parse_statements(self) -> ParserOutput {
        self.parse_with(|this| this.parse_block(bitset![TokenKind::EndOfInput]))
    }

    fn parse_with(mut self, parse: impl FnOnce(&mut Self) -> ParseResult) -> ParserOutput {
        let watcher = self.push_node();

        self.consume_whitespace();
        // Nothing left to recover if there is an error
        self.parse_recoverable(bitset![TokenKind::EndOfInput], parse)
            .expect("Should always recover at the top level");
        let mut root_node = match self.pop_node(watcher, SyntaxNodeKind::Root) {
            Some(root_node) => root_node,
            None => SyntaxNode {
                kind: SyntaxNodeKind::Root,
                id: SyntaxId::new_unset(Some(self.file_id)),
                len: 0,
                children: Box::new([]),
            },
        };

        root_node.assign_ids(SyntaxId::NUMBER_RANGE);
        ParserOutput {
            syntax_tree: SyntaxTree { root_node },
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
        let allowed_whitespace = if self.options().newlines_are_whitespace {
            Self::WHITESPACE_LIKE
        } else {
            bitset![TokenKind::Whitespace]
        };
        while allowed_whitespace.contains(self.peek().kind) {
            self.consume_single();
        }
    }

    const WHITESPACE_LIKE: TokenSet = bitset![
        TokenKind::Whitespace,
        TokenKind::Newline,
        TokenKind::Comment
    ];
    fn consume_whitespace_and_newlines(&mut self) {
        while self
            .consume_if(|token| Self::WHITESPACE_LIKE.contains(token.kind).then_some(token))
            .is_ok()
        {}
    }

    fn consume_statement_separator(&mut self) -> bool {
        let mut consumed = false;
        loop {
            if !Self::WHITESPACE_LIKE.contains(self.peek().kind) {
                break;
            }
            let kind = self.consume().kind;
            consumed |= kind == TokenKind::Newline;
        }
        consumed || self.peek().kind == TokenKind::EndOfInput
    }

    #[track_caller]
    fn expect(&mut self, expected: impl Into<TokenSet>) -> ParseResult<&'a str> {
        let expected = expected.into();
        match self.consume_if(|token| expected.contains(token.kind).then_some(token)) {
            Ok(token) => Ok(token.text),
            Err(token) => {
                let got = token.text.into();
                self.push_error(DiagnosticError::UnexpectedToken { expected, got });
                self.recover_to(self.current_stack_level(), expected)?;
                Ok(self.consume().text)
            }
        }
    }

    fn options(&self) -> ParseOptions {
        self.option_stack
            .last()
            .map_or(ParseOptions::default(), |it| it.1)
    }

    fn push_options(&mut self, options: ParseOptions) {
        let level = self.current_stack_level().stack_level;
        self.option_stack.push((level, options))
    }

    fn pop_options(&mut self) {
        self.option_stack.pop().unwrap();
    }

    fn push_node(&mut self) -> ParserFrameWatcher {
        let stack_level = self.node_stack.len();
        self.node_stack.push(ParserStackNode {
            children: Vec::new(),
        });
        ParserFrameWatcher { stack_level }
    }

    #[must_use]
    fn pop_node(
        &mut self,
        watcher: ParserFrameWatcher,
        kind: SyntaxNodeKind,
    ) -> Option<SyntaxNode> {
        let ParserStackNode { children } = self.node_stack.pop()?;
        debug_assert_eq!(self.node_stack.len(), watcher.stack_level);
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

    /// Restores the node stack to the expected depth, closing all unclosed nodes as errors.
    /// Also makes sure to restore the options stack to the expected level.
    fn pop_to_level(&mut self, watcher: ParserFrameWatcher) {
        while self.node_stack.len() > watcher.stack_level + 1 {
            let watcher = ParserFrameWatcher {
                stack_level: self.node_stack.len() - 1,
            };
            self.finish_node(watcher, SyntaxNodeKind::Error);
        }

        while let Some((level, _)) = self.option_stack.last()
            && *level > watcher.stack_level
        {
            self.option_stack.pop().unwrap();
        }
    }

    /// Tries to advance the parser to a set of known good tokens, emitting each unexpected token in the process.
    /// If a good token is found, it is not consumed.
    /// If a good token is found, the stack is also adjusted to match the level of the passed watcher.
    /// Returns [`Err`] if a bad token is found.
    fn recover_to(&mut self, watcher: ParserFrameWatcher, good_tokens: TokenSet) -> ParseResult {
        let bad_tokens = self.recovery_stack.last().copied().unwrap_or_default();
        loop {
            let kind = self.peek().kind;
            if good_tokens.contains(kind) {
                self.pop_to_level(watcher);
                return Ok(());
            }
            if bad_tokens.contains(kind) {
                return Err(());
            }

            self.consume_single();
        }
    }

    fn current_stack_level(&self) -> ParserFrameWatcher {
        ParserFrameWatcher {
            stack_level: self.node_stack.len().strict_sub(1),
        }
    }

    /// Executes the parse function and tries to recover to the current stack level if an error occured.
    /// Returns [`Err`] if `parse_fn` returned an error and recovery was not possible
    fn parse_recoverable(
        &mut self,
        recoverable_tokens: TokenSet,
        parse_fn: impl FnOnce(&mut Self) -> ParseResult,
    ) -> ParseResult {
        let stack_level = self.current_stack_level();
        let parent_set = self.recovery_stack.last().copied().unwrap_or_default();
        self.recovery_stack
            .push(parent_set.union(&recoverable_tokens));
        let result = parse_fn(self);
        self.recovery_stack.pop().unwrap();

        if let Err(()) = result {
            return self.recover_to(stack_level, recoverable_tokens);
        }

        Ok(())
    }

    fn parse_newline_separated(
        &mut self,
        end_set: impl Into<TokenSet>,
        parse: impl Fn(&mut Self) -> ParseResult,
    ) -> ParseResult {
        let end_set = end_set.into();
        self.push_options(ParseOptions {
            newlines_are_whitespace: false,
            ..self.options()
        });
        loop {
            self.consume_whitespace_and_newlines();
            if end_set.contains(self.peek().kind) {
                break;
            }
            self.parse_recoverable(end_set + TokenKind::Newline, &parse)?;
            if !self.consume_statement_separator() && !end_set.contains(self.peek().kind) {
                self.push_error(DiagnosticError::ExpectedNewline);
            }
        }
        self.pop_options();

        Ok(())
    }

    #[track_caller]
    fn parse_separated(
        &mut self,
        end_set: impl Into<TokenSet>,
        separator_tokens: impl Into<TokenSet>,
        mut parse_fn: impl FnMut(&mut Self) -> ParseResult,
    ) -> ParseResult {
        let end_set = end_set.into();
        let separator_tokens = separator_tokens.into();
        self.parse_recoverable(end_set, |parser| {
            loop {
                if end_set.contains(parser.peek().kind) {
                    break;
                }
                parser.parse_recoverable(end_set.union(&separator_tokens), &mut parse_fn)?;
                if !separator_tokens.contains(parser.peek().kind) {
                    break;
                }
                parser.expect(separator_tokens)?;
            }
            Ok(())
        })
    }

    #[track_caller]
    fn parse_surrounded(
        &mut self,
        start: TokenKind,
        end: TokenKind,
        parse_fn: impl FnOnce(&mut Self) -> ParseResult,
    ) -> ParseResult {
        self.push_options(ParseOptions {
            newlines_are_whitespace: true,
            ..self.options()
        });
        self.expect(start)?;
        self.parse_recoverable(end.into(), parse_fn)?;
        self.pop_options();
        self.expect(end)?;
        Ok(())
    }
}

/// Parsing methods
impl Parser<'_> {
    fn parse_file(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.parse_newline_separated(bitset![TokenKind::EndOfInput], Self::parse_item)?;

        self.finish_node(watcher, SyntaxNodeKind::File);
        Ok(())
    }

    fn parse_item(&mut self) -> ParseResult {
        let watcher = self.push_node();

        match self.multipeek() {
            [TokenKind::KeywordUnsafe, TokenKind::KeywordExtern] => self.parse_extern_block()?,
            [TokenKind::KeywordFn, ..] => self.parse_function_definition()?,
            [TokenKind::KeywordType, ..] => self.parse_type_alias()?,
            [TokenKind::KeywordStruct, ..] => self.parse_struct()?,
            [..] => {
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

        self.parse_surrounded(TokenKind::BraceOpen, TokenKind::BraceClose, |parser| {
            parser.parse_newline_separated(TokenKind::BraceClose, Self::parse_extern_item)
        })?;

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

        self.parse_surrounded(TokenKind::BraceOpen, TokenKind::BraceClose, |parser| {
            parser.parse_newline_separated(TokenKind::BraceClose, Self::parse_extern_type_body_item)
        })?;

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
        match self.multipeek() {
            [TokenKind::KeywordStatic, TokenKind::KeywordLet] => {
                self.parse_extern_type_body_static_let()?
            }
            [TokenKind::KeywordFn, ..] | [TokenKind::KeywordStatic, TokenKind::KeywordFn] => {
                self.parse_extern_type_body_function()?
            }
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

    fn parse_extern_type_body_function(&mut self) -> ParseResult {
        let watcher = self.push_node();

        if self.peek().kind == TokenKind::KeywordStatic {
            self.consume();
        }

        self.expect(TokenKind::KeywordFn)?;
        self.expect(TokenKind::Identifier)?;
        self.parse_parameter_list()?;
        if self.peek().kind == TokenKind::Arrow {
            self.parse_return_type()?;
        }

        self.finish_node(watcher, SyntaxNodeKind::ExternTypeFunction);
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
        self.parse_recoverable(
            bitset![TokenKind::Arrow, TokenKind::BraceOpen],
            Self::parse_parameter_list,
        )?;
        if self.peek().kind == TokenKind::Arrow {
            self.parse_return_type()?;
        }
        self.parse_braced_block()?;

        self.finish_node(watcher, SyntaxNodeKind::FunctionDefinition);
        Ok(())
    }

    fn parse_parameter_list(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.parse_surrounded(TokenKind::ParensOpen, TokenKind::ParensClose, |parser| {
            parser.parse_separated(
                TokenKind::ParensClose,
                TokenKind::Comma,
                Self::parse_parameter_definition,
            )
        })?;

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

    fn parse_type_alias(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordType)?;
        self.expect(TokenKind::Identifier)?;
        self.expect(TokenKind::Equal)?;
        self.parse_type()?;

        self.finish_node(watcher, SyntaxNodeKind::TypeAlias);
        Ok(())
    }

    fn parse_struct(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordStruct)?;
        self.expect(TokenKind::Identifier)?;
        self.parse_surrounded(
            TokenKind::BraceOpen,
            TokenKind::BraceClose,
            Self::parse_struct_body,
        )?;

        self.finish_node(watcher, SyntaxNodeKind::Struct);
        Ok(())
    }

    fn parse_struct_body(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.parse_separated(
            TokenKind::BraceClose,
            TokenKind::Comma,
            Self::parse_structural_type_field,
        )?;

        self.finish_node(watcher, SyntaxNodeKind::StructBody);
        Ok(())
    }

    fn parse_type(&mut self) -> ParseResult {
        let watcher = self.push_node();

        match self.peek().kind {
            TokenKind::BracketOpen => self.parse_array_type()?,
            TokenKind::ParensOpen => self.parse_unit_type()?,
            TokenKind::BraceOpen => self.parse_structural_type()?,
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

    fn parse_structural_type(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.parse_surrounded(TokenKind::BraceOpen, TokenKind::BraceClose, |parser| {
            parser.parse_separated(
                TokenKind::BraceClose,
                TokenKind::Comma,
                Self::parse_structural_type_field,
            )
        })?;

        self.finish_node(watcher, SyntaxNodeKind::StructuralType);
        Ok(())
    }

    fn parse_structural_type_field(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::Identifier)?;
        self.expect(TokenKind::Colon)?;
        self.parse_type()?;

        self.finish_node(watcher, SyntaxNodeKind::StructuralTypeField);
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

    fn parse_unit_type(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::ParensOpen)?;
        self.expect(TokenKind::ParensClose)?;

        self.finish_node(watcher, SyntaxNodeKind::UnitType);
        Ok(())
    }

    fn parse_block(&mut self, end_set: TokenSet) -> ParseResult {
        let watcher = self.push_node();

        self.parse_recoverable(end_set, |parser| {
            parser.parse_newline_separated(end_set, Self::parse_statement)
        })?;

        self.finish_node(watcher, SyntaxNodeKind::Block);
        Ok(())
    }

    fn parse_braced_block(&mut self) -> ParseResult {
        self.parse_surrounded(TokenKind::BraceOpen, TokenKind::BraceClose, |parser| {
            parser.parse_block(TokenKind::BraceClose.into())
        })?;

        Ok(())
    }

    fn parse_statement(&mut self) -> ParseResult {
        let watcher = self.push_node();

        match self.peek().kind {
            TokenKind::KeywordLet => self.parse_assignment(),
            TokenKind::KeywordIf => self.parse_if_statement(),
            TokenKind::KeywordLoop => self.parse_loop(),
            TokenKind::KeywordWhile => self.parse_while(),
            TokenKind::KeywordBreak => self.parse_break(),
            TokenKind::KeywordContinue => self.parse_continue(),
            TokenKind::KeywordReturn => self.parse_return(),
            _ => self.parse_expression_or_update(),
        }?;

        self.finish_node(watcher, SyntaxNodeKind::Statement);
        Ok(())
    }

    fn parse_assignment(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordLet)?;
        self.expect(TokenKind::Identifier)?;
        if self.peek().kind == TokenKind::Colon {
            self.consume();
            self.parse_type()?;
        }
        self.expect(TokenKind::Equal)?;
        self.parse_expression()?;

        self.finish_node(watcher, SyntaxNodeKind::Assignment);
        Ok(())
    }

    fn parse_if_statement(&mut self) -> ParseResult {
        let watcher = self.push_node();
        self.push_options(ParseOptions {
            allow_named_struct_literals: false,
            ..self.options()
        });

        self.expect(TokenKind::KeywordIf)?;
        self.parse_expression()?;
        self.pop_options();
        self.parse_braced_block()?;

        if self.peek().kind == TokenKind::KeywordElse {
            let watcher = self.push_node();

            self.consume();

            if self.peek().kind == TokenKind::KeywordIf {
                self.parse_statement()?;
            } else {
                self.parse_braced_block()?;
            }

            self.finish_node(watcher, SyntaxNodeKind::IfStatementElse);
        }

        self.finish_node(watcher, SyntaxNodeKind::IfStatement);
        Ok(())
    }

    fn parse_loop(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordLoop)?;
        self.parse_braced_block()?;

        self.finish_node(watcher, SyntaxNodeKind::Loop);
        Ok(())
    }

    fn parse_while(&mut self) -> ParseResult {
        let watcher = self.push_node();
        self.push_options(ParseOptions {
            allow_named_struct_literals: false,
            ..self.options()
        });

        self.expect(TokenKind::KeywordWhile)?;
        self.parse_recoverable(TokenKind::BraceOpen.into(), Self::parse_expression)?;
        self.pop_options();
        self.parse_braced_block()?;

        self.finish_node(watcher, SyntaxNodeKind::WhileLoop);
        Ok(())
    }

    fn parse_break(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordBreak)?;

        self.finish_node(watcher, SyntaxNodeKind::Break);
        Ok(())
    }

    fn parse_continue(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordContinue)?;

        self.finish_node(watcher, SyntaxNodeKind::Continue);
        Ok(())
    }

    fn parse_return(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordReturn)?;
        if Self::EXPRESSION_START.contains(self.peek().kind) {
            self.parse_expression()?;
        }

        self.finish_node(watcher, SyntaxNodeKind::Return);
        Ok(())
    }

    fn parse_expression_or_update(&mut self) -> ParseResult {
        if !Self::EXPRESSION_START.contains(self.peek().kind) {
            self.push_error(DiagnosticError::ExpectedExpression {
                got: self.peek().text.into(),
                valid_tokens: Self::EXPRESSION_START,
            });
            return Err(());
        }

        let watcher = self.push_node();
        let watcher = self.parse_expression_pratt_inner(watcher, 0)?;

        if self.peek().kind.to_assignment_operator().is_some() {
            let expression_node = self
                .pop_node(watcher, SyntaxNodeKind::Expression)
                .expect("Node was started");
            let watcher = self.push_node();
            let accessor_watcher = self.push_node();
            let parent = self.node_stack.last_mut().expect("Was just pushed");
            parent.children.push(SyntaxItem::Node(expression_node));
            self.finish_node(accessor_watcher, SyntaxNodeKind::Path);

            self.consume();
            self.parse_expression()?;

            self.finish_node(watcher, SyntaxNodeKind::VariableUpdate);
        } else if let [_, TokenKind::Equal] = self.multipeek() {
            self.push_error(DiagnosticError::UnexpectedToken {
                expected: UpdateOperatorToken::TOKEN_SET,
                got: self.peek().text.into(),
            });
            // This branch is just for better error messages in case the user typed a update operator token
            // that does not exist
            self.consume();
            return Err(());
        } else {
            self.finish_node(watcher, SyntaxNodeKind::Expression);
        }

        Ok(())
    }

    fn parse_expression(&mut self) -> ParseResult {
        if !Self::EXPRESSION_START.contains(self.peek().kind) {
            self.push_error(DiagnosticError::ExpectedExpression {
                got: self.peek().text.into(),
                valid_tokens: Self::EXPRESSION_START,
            });
            return Err(());
        }

        self.parse_expression_pratt(0)
    }

    fn parse_expression_pratt(&mut self, min_binding_power: u32) -> ParseResult {
        let watcher = self.push_node();
        let watcher = self.parse_expression_pratt_inner(watcher, min_binding_power)?;
        self.finish_node(watcher, SyntaxNodeKind::Expression);
        Ok(())
    }

    fn parse_expression_pratt_inner(
        &mut self,
        mut watcher: ParserFrameWatcher,
        min_binding_power: u32,
    ) -> ParseResult<ParserFrameWatcher> {
        self.parse_value_or_postfix()?;

        loop {
            let Some(operator) = self.peek().kind.to_binary_operator() else {
                break;
            };
            let binding_power = operator.binding_power();
            if binding_power <= min_binding_power {
                break;
            }

            let node = self
                .pop_node(watcher, SyntaxNodeKind::Expression)
                .expect("Node was started");
            watcher = self.push_node();
            let parent = self.node_stack.last_mut().expect("Was just pushed");
            parent.children.push(SyntaxItem::Node(node));

            self.parse_binary_operator_token()?;
            self.parse_expression_pratt(binding_power)?;

            let node = self
                .pop_node(watcher, SyntaxNodeKind::BinaryOperation)
                .expect("Node was started");
            watcher = self.push_node();
            let parent = self.node_stack.last_mut().expect("Was just pushed");
            parent.children.push(SyntaxItem::Node(node));
        }

        Ok(watcher)
    }

    fn parse_binary_operator_token(&mut self) -> ParseResult {
        self.consume_if(|token| token.kind.to_binary_operator())
            .map_err(|_| ())?;
        Ok(())
    }

    // must be in sync with `parse_value_or_postfix`
    const EXPRESSION_START: TokenSet = bitset![
        TokenKind::Identifier,
        TokenKind::NumberLiteral,
        TokenKind::StringLiteral,
        TokenKind::ParensOpen,
        TokenKind::BraceOpen,
        TokenKind::KeywordTrue,
        TokenKind::KeywordFalse,
        TokenKind::KeywordNot,
    ];
    fn parse_value_or_postfix(&mut self) -> ParseResult {
        let mut watcher = self.push_node();

        match self.multipeek() {
            [TokenKind::Identifier, TokenKind::BraceOpen]
                if self.options().allow_named_struct_literals =>
            {
                self.parse_struct_literal()?
            }
            [TokenKind::BraceOpen, ..] => self.parse_struct_literal()?,
            [TokenKind::Identifier, ..] => self.parse_identifier()?,
            [TokenKind::NumberLiteral, ..] => self.parse_number()?,
            [TokenKind::StringLiteral, ..] => self.parse_string_literal()?,
            [TokenKind::ParensOpen, ..] => self.parse_parenthesis()?,
            [TokenKind::KeywordTrue, ..] | [TokenKind::KeywordFalse, ..] => {
                self.parse_boolean_literal()?
            }
            [TokenKind::KeywordNot, ..] => self.parse_not()?,
            _ => {
                self.push_error(DiagnosticError::ExpectedValue {
                    got: self.peek().text.into(),
                });
                return Err(());
            }
        };

        if !Self::POSTFIX_START.contains(self.peek().kind) {
            self.finish_node(watcher, SyntaxNodeKind::Value);
            return Ok(());
        };

        let mut current_node_kind = SyntaxNodeKind::Value;
        while Self::POSTFIX_START.contains(self.peek().kind) {
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

    // must be kept in sync with `parse_postfix`
    const POSTFIX_START: TokenSet = bitset![TokenKind::ParensOpen, TokenKind::Dot];
    fn parse_postfix(&mut self) -> ParseResult {
        let watcher = self.push_node();

        assert!(Self::POSTFIX_START.contains(self.peek().kind));
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

        self.parse_surrounded(TokenKind::ParensOpen, TokenKind::ParensClose, |parser| {
            parser.parse_separated(
                TokenKind::ParensClose,
                TokenKind::Comma,
                Self::parse_expression,
            )
        })?;

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

    fn parse_boolean_literal(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(bitset![TokenKind::KeywordTrue, TokenKind::KeywordFalse])?;

        self.finish_node(watcher, SyntaxNodeKind::BooleanLiteral);
        Ok(())
    }

    fn parse_parenthesis(&mut self) -> ParseResult {
        let watcher = self.push_node();
        self.push_options(ParseOptions {
            allow_named_struct_literals: true,
            ..self.options()
        });
        self.parse_surrounded(
            TokenKind::ParensOpen,
            TokenKind::ParensClose,
            Self::parse_expression,
        )?;
        self.pop_options();
        self.finish_node(watcher, SyntaxNodeKind::Parenthesis);
        Ok(())
    }

    fn parse_struct_literal(&mut self) -> ParseResult {
        let watcher = self.push_node();
        if self.peek().kind == TokenKind::Identifier {
            self.consume();
        }
        self.parse_surrounded(TokenKind::BraceOpen, TokenKind::BraceClose, |parser| {
            parser.parse_separated(
                TokenKind::BraceClose,
                TokenKind::Comma,
                Self::parse_struct_literal_field,
            )
        })?;

        self.finish_node(watcher, SyntaxNodeKind::StructLiteral);
        Ok(())
    }

    fn parse_struct_literal_field(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::Identifier)?;
        self.expect(TokenKind::Colon)?;
        self.parse_expression()?;

        self.finish_node(watcher, SyntaxNodeKind::StructLiteralField);
        Ok(())
    }

    fn parse_not(&mut self) -> ParseResult {
        let watcher = self.push_node();

        self.expect(TokenKind::KeywordNot)?;
        self.parse_value_or_postfix()?;

        self.finish_node(watcher, SyntaxNodeKind::PrefixNot);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use crate::auryn::{
        diagnostic::{Diagnostic, Diagnostics},
        diagnostic_display::DisplayOptions,
        file_id::FileId,
        input_files::InputFiles,
        parser::{Parser, ParserOutput},
    };

    struct AnnotatedParserOutput {
        diagnostics: Vec<Diagnostic>,
        input_files: InputFiles,
    }

    impl Debug for AnnotatedParserOutput {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let ast = self.input_files.get(FileId::MAIN_FILE).syntax_tree();
            let diagnostics: Diagnostics = self.diagnostics.iter().cloned().collect();
            writeln!(
                f,
                "syntax_tree:\n{}\ndiagnostics:\n{}",
                ast.display(self.input_files.get(FileId::MAIN_FILE).source.as_ref()),
                diagnostics.to_display(&self.input_files, DisplayOptions::FOR_TESTING)
            )
        }
    }

    fn verify_with(input: &str, parse_fn: impl FnOnce(Parser) -> ParserOutput) -> impl Debug {
        let mut input_files = InputFiles::default();
        input_files.add("main".into(), input.into());

        let output = parse_fn(Parser::new(
            FileId::MAIN_FILE,
            &input_files.get(FileId::MAIN_FILE).source,
        ));
        input_files
            .get(FileId::MAIN_FILE)
            .parser_output
            .set(output)
            .unwrap();
        let output = input_files.get(FileId::MAIN_FILE).syntax_tree();
        let diagnostics = output.collect_diagnostics();
        AnnotatedParserOutput {
            input_files,
            diagnostics,
        }
    }

    fn verify_block(input: &str) -> impl Debug {
        verify_with(input, |parser| parser.parse_statements())
    }

    fn verify(input: &str) -> impl Debug {
        verify_with(input, |parser| parser.parse())
    }

    #[test]
    fn test_parse_expression() {
        insta::assert_debug_snapshot!(verify_block("1 + 2 * 3"));
        insta::assert_debug_snapshot!(verify_block("1 * 2 + 3"));
        insta::assert_debug_snapshot!(verify_block("1 * \"test\""));
        insta::assert_debug_snapshot!(verify_block("1 + 1 == 2 or true and false"));
        insta::assert_debug_snapshot!(verify_block("{a: 1,\nb: 2 * 3}"));
    }

    #[test]
    fn test_parse_postfix_operator() {
        insta::assert_debug_snapshot!(verify_block("a()"));
        insta::assert_debug_snapshot!(verify_block("a.b"));
        insta::assert_debug_snapshot!(verify_block("a.b()()"));
    }

    #[test]
    fn test_prefix_operator() {
        insta::assert_debug_snapshot!(verify_block("not true"));
        insta::assert_debug_snapshot!(verify_block("not true or false"));
        insta::assert_debug_snapshot!(verify_block("not some_function()"));
    }

    #[test]
    fn test_parse_large_number() {
        insta::assert_debug_snapshot!(verify_block("9999999999999"));
    }

    #[test]
    fn test_parse_parenthesis() {
        insta::assert_debug_snapshot!(verify_block("(3)"));
        insta::assert_debug_snapshot!(verify_block("((3))"));
        insta::assert_debug_snapshot!(verify_block("1 + (\n2\n+\n3\n)"));
        insta::assert_debug_snapshot!(verify_block("1 * (2 + 3)"));
    }

    #[test]
    fn test_parse_function_call() {
        insta::assert_debug_snapshot!(verify_block("print(\n1)"));
        insta::assert_debug_snapshot!(verify_block("print(1)\n"));
        insta::assert_debug_snapshot!(verify_block("print()"));
    }

    #[test]
    fn test_parse_assignment() {
        insta::assert_debug_snapshot!(verify_block("let helloworld = 1\n"));
        insta::assert_debug_snapshot!(verify_block("let helloworld: I32 = 1\n"));
    }

    #[test]
    fn test_variable() {
        insta::assert_debug_snapshot!(verify_block("print(hello)"));
    }

    #[test]
    fn test_variable_update() {
        insta::assert_debug_snapshot!(verify_block("a = 3"));
        insta::assert_debug_snapshot!(verify_block("a *= 3"));
        insta::assert_debug_snapshot!(verify_block("print(test).a *= 3"));
    }

    #[test]
    fn test_comparison() {
        insta::assert_debug_snapshot!(verify_block("1 == 1"));
    }

    #[test]
    fn test_if_statemt() {
        insta::assert_debug_snapshot!(verify_block("if 1 { print(42) }"));
        insta::assert_debug_snapshot!(verify_block(
            "if true { print(false) } else { print(true) }"
        ));
        insta::assert_debug_snapshot!(verify_block(
            "if true { print(false) } else if false { print(true) } else { print(69) }"
        ));
    }

    #[test]
    fn test_loop() {
        insta::assert_debug_snapshot!(verify_block("loop { 1 + 2 }"));
        insta::assert_debug_snapshot!(verify_block("loop { break }"));
        insta::assert_debug_snapshot!(verify_block("\t\nloop {\t\n\t\n\tprint(1)\t\n\t}\t\n\t"));
        insta::assert_debug_snapshot!(verify_block("loop { continue}"));
    }

    #[test]
    fn test_whilel() {
        insta::assert_debug_snapshot!(verify_block("while 1 > 1 { print(1) }"));
    }

    #[test]
    fn test_function() {
        insta::assert_debug_snapshot!(verify(
            "fn foo(a: Int,\nb: []String, c: ()) -> Null { print(9000) }"
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
            r#"
            unsafe extern "java" {
                ["java/lang/Foo"]
                type Foo {
                    ["bar"]
                    static let bar: Int

                    ["baz"] fn baz(value: Int) -> String

                    ["static"] static fn slkfdj()
                }

                ["java/lang/System"]
                type System {
                    ["out"]
                    static let out: Foo
                }
            }
            "#
        ));
    }

    #[test]
    fn test_parse_whitespace() {
        insta::assert_debug_snapshot!(verify(
            r#"
            // This is the main function
            fn main() {
                // We will now initialize a variable called 'a' to 1
                let a = 1 // this is the assignment
                // This is the end of the function
            }
            "#
        ))
    }

    #[test]
    fn test_type_literal() {
        insta::assert_debug_snapshot!(verify_block(
            r#"
            let a: {
                a: I32,
                b: I64
            } = {
                a: 10,
                b: 20 * 2
            }
            "#
        ));
        insta::assert_debug_snapshot!(verify_block(
            r#"
            let a = {a: false, }
            "#
        ));
        insta::assert_debug_snapshot!(verify_block(
            r#"
            let a = Foo {a: false, }
            "#
        ));
        insta::assert_debug_snapshot!(verify_block(
            r#"
            if Foo {}
            "#
        ));
        insta::assert_debug_snapshot!(verify_block(
            r#"
            if (Foo {}) {}
            "#
        ));
        insta::assert_debug_snapshot!(verify_block(
            r#"
            while Foo {}
            "#
        ));
    }

    #[test]
    fn test_type_alias() {
        insta::assert_debug_snapshot!(verify(
            r#"
            type Alias = {a: I32, b: Bool}
            "#
        ));
    }

    #[test]
    fn test_struct() {
        insta::assert_debug_snapshot!(verify(
            r#"
            struct Foo {
                some_value: I32,
                b: ldaksjf
            }
            "#
        ));
    }

    #[test]
    fn test_reject_extra_input() {
        insta::assert_debug_snapshot!(verify_block("1 1 1 1 11 1"));
    }
}
