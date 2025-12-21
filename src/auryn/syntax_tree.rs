use std::{
    fmt::{Debug, Display},
    num::NonZeroU64,
    ops::Range,
};

use crate::{
    auryn::{
        diagnostic::{ComputedDiagnostic, ComputedSpan, Diagnostic, DiagnosticKind},
        syntax_id::SyntaxId,
        tokenizer::TokenKind,
    },
    utils::small_string::SmallString,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SyntaxNodeKind {
    Root,
    Block,
    Statement,
    Expression,
    Value,
    Number,
    BinaryOperation,
    Error,
    Parenthesis,
    FunctionCall,
    ArgumentList,
    Assignment,
    Ident,
    VariableUpdate,
    IfStatement,
    Loop,
    Break,
}

#[derive(Debug)]
pub struct SyntaxNode {
    pub kind: SyntaxNodeKind,
    /// Unique id of this node, can be used to retrive this node.
    /// This id number of every child is greater than the id number of this node,
    /// and the id number of every child is greater than the id number of its previous sibling.
    pub id: SyntaxId,
    /// The length in bytes of the source code that represents this node
    pub len: u32,
    pub children: Box<[SyntaxItem]>,
}

impl SyntaxNode {
    pub fn node_children(&self) -> impl Iterator<Item = &SyntaxNode> {
        self.children.iter().filter_map(SyntaxItem::as_node)
    }

    pub fn collect_diagnostics(&self, mut offset: u32, buf: &mut Vec<ComputedDiagnostic>) {
        for child in &self.children {
            child.collect_diagnostics(offset, buf);
            offset += child.len();
        }
    }

    /// Assigns ids so that `self.id.number()` does not return null.
    pub(super) fn assign_ids(&mut self, mut range: Range<NonZeroU64>) {
        self.id.set_number(range.start);

        if self.children.is_empty() {
            return;
        }

        range.start = range.start.checked_add(1).unwrap();
        assert!(!range.is_empty(), "Range should not be empty");
        let available_space = range.end.get() - range.start.get();
        assert!(
            available_space as usize > self.children.len(),
            "Ran out of ids D:"
        );
        let stepsize = available_space / self.children.len() as u64;

        for child in &mut self.children {
            let end = range.start.checked_add(stepsize).unwrap();
            child.assign_ids(range.start..end);
            range.start = end;
        }
    }

    /// Returns the span of the node that belongs to the given `syntax_id`.
    /// Panics if the syntax id is not in this subtree.
    pub fn get_span(&self, syntax_id: SyntaxId, mut offset: u32) -> ComputedSpan {
        if syntax_id == self.id {
            return ComputedSpan {
                offset,
                len: self.len,
            };
        }
        assert!(syntax_id.number() > self.id.number());
        assert!(syntax_id.file_id() == self.id.file_id());
        for windows in self.children.windows(2) {
            let [child, next_child] = windows else {
                panic!("lol");
            };
            if syntax_id.number() < next_child.id().number() {
                return child.get_span(syntax_id, offset);
            }

            offset += child.len();
        }

        // Since the id belongs to this node and is the equal to the id of this node, it must be the id of a child
        self.children.last().unwrap().get_span(syntax_id, offset)
    }
}

#[derive(Debug)]
pub struct SyntaxToken {
    pub kind: TokenKind,
    pub id: SyntaxId,
    pub text: SmallString,
}

impl SyntaxToken {}

#[derive(Debug)]
pub struct ErrorNode {
    pub id: SyntaxId,
    pub text: SmallString,
    pub diagnostic: DiagnosticKind,
}

impl ErrorNode {
    pub fn collect_diagnostics(&self, offset: u32, buf: &mut Vec<ComputedDiagnostic>) {
        buf.push(ComputedDiagnostic {
            inner: Diagnostic::new(self.id, self.diagnostic.clone()),
            span: ComputedSpan {
                offset,
                len: self.text.len().try_into().unwrap(),
            },
        });
    }
}

#[derive(Debug)]
pub enum SyntaxItem {
    Node(SyntaxNode),
    Token(SyntaxToken),
    Error(Box<ErrorNode>),
}

impl SyntaxItem {
    pub fn id(&self) -> SyntaxId {
        match self {
            SyntaxItem::Node(syntax_node) => syntax_node.id,
            SyntaxItem::Token(syntax_token) => syntax_token.id,
            SyntaxItem::Error(error) => error.id,
        }
    }

    pub fn len(&self) -> u32 {
        match self {
            SyntaxItem::Node(node) => node.len,
            SyntaxItem::Token(token) => token.text.len().try_into().unwrap(),
            SyntaxItem::Error(error) => error.text.len().try_into().unwrap(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn as_node(&self) -> Option<&SyntaxNode> {
        match self {
            SyntaxItem::Node(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_token(&self) -> Option<&SyntaxToken> {
        match self {
            SyntaxItem::Token(token) => Some(token),
            _ => None,
        }
    }

    pub fn collect_diagnostics(&self, offset: u32, buf: &mut Vec<ComputedDiagnostic>) {
        match self {
            SyntaxItem::Node(node) => node.collect_diagnostics(offset, buf),
            SyntaxItem::Token(_) => {}
            SyntaxItem::Error(error) => error.collect_diagnostics(offset, buf),
        }
    }

    fn assign_ids(&mut self, range: Range<NonZeroU64>) {
        match self {
            SyntaxItem::Node(syntax_node) => syntax_node.assign_ids(range),
            SyntaxItem::Token(SyntaxToken { id, .. }) => {
                id.set_number(range.start);
            }
            SyntaxItem::Error(error_node) => {
                error_node.id.set_number(range.start);
            }
        }
    }

    fn get_span(&self, syntax_id: SyntaxId, offset: u32) -> ComputedSpan {
        match self {
            SyntaxItem::Node(syntax_node) => syntax_node.get_span(syntax_id, offset),
            SyntaxItem::Token(syntax_token) => {
                assert!(syntax_id == syntax_token.id);
                ComputedSpan {
                    offset,
                    len: syntax_token.text.len().try_into().unwrap(),
                }
            }
            SyntaxItem::Error(error_node) => {
                assert!(syntax_id == error_node.id);
                ComputedSpan {
                    offset,
                    len: error_node.text.len().try_into().unwrap(),
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct SyntaxTree {
    pub root_node: SyntaxNode,
}

impl SyntaxTree {
    pub fn display<'a, 'b>(&'a self, source: &'b str) -> SyntaxNodeDisplay<'a, 'b> {
        SyntaxNodeDisplay {
            node: &self.root_node,
            source,
            offset: 0,
            depth: 0,
        }
    }

    pub fn collect_diagnostics(&self) -> Vec<ComputedDiagnostic> {
        let mut buf = Vec::new();
        self.root_node.collect_diagnostics(0, &mut buf);
        buf
    }

    pub fn get_span(&self, syntax_id: SyntaxId) -> ComputedSpan {
        self.root_node.get_span(syntax_id, 0)
    }
}

pub struct SyntaxNodeDisplay<'a, 'b> {
    node: &'a SyntaxNode,
    source: &'b str,
    offset: u32,
    depth: u32,
}

impl Display for SyntaxNodeDisplay<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let depth = self.depth as usize;
        let offset = self.offset;
        let end = offset + self.node.len;
        let range = format_args!("{offset}..{end}");
        let kind = &self.node.kind;
        for _ in 0..depth {
            write!(f, "|")?;
        }
        write!(f, "{range} {kind:?}")?;

        if self.node.children.is_empty() {
            write!(f, " '")?;
            f.write_str(&self.source[(offset as usize)..(end as usize)])?;
            writeln!(f, "'")?;
        } else {
            writeln!(f)?;
            let mut offset = offset;
            for child in &self.node.children {
                match child {
                    SyntaxItem::Node(node) => {
                        let display_node = SyntaxNodeDisplay {
                            node,
                            source: self.source,
                            offset,
                            depth: self.depth + 1,
                        };

                        Display::fmt(&display_node, f)?;
                    }
                    SyntaxItem::Token(token) => {
                        for _ in 0..(depth + 1) {
                            write!(f, "|")?;
                        }
                        writeln!(f, "{:?}", token.kind)?;
                    }
                    SyntaxItem::Error(error) => {
                        for _ in 0..(depth + 1) {
                            write!(f, "|")?;
                        }
                        writeln!(f, "{:?}", error.diagnostic)?;
                    }
                }
                offset += child.len();
            }
        }

        Ok(())
    }
}

impl Debug for SyntaxNodeDisplay<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as Display>::fmt(self, f)
    }
}
