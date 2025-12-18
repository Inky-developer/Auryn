use std::fmt::{Debug, Display};

use crate::auryn::{
    ComputedSpan, Span,
    parser::DiagnosticKind,
    tokenizer::{BinaryOperatorToken, TokenKind},
};

#[derive(Debug)]
pub struct ComputedDiagnostic {
    pub kind: DiagnosticKind,
    pub span: ComputedSpan,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SyntaxNodeKind {
    Root,
    Block,
    Statement,
    Expression,
    Value,
    Number { value: i32 },
    BinaryOperation,
    BinaryOperator(BinaryOperatorToken),
    Error,
    Parenthesis,
    FunctionCall { ident: String },
    StatementList,
    ParameterList,
    Assignment { ident: String },
    Ident { ident: String },
    VariableUpdate { ident: String },
    IfStatement,
    Loop,
    Break,
}

#[derive(Debug)]
pub struct SyntaxNode {
    pub kind: SyntaxNodeKind,
    pub span: Span,
    pub children: Box<[SyntaxItem]>,
}

impl SyntaxNode {
    pub fn node_children(&self) -> impl Iterator<Item = &SyntaxNode> {
        self.children.iter().filter_map(SyntaxItem::as_node)
    }

    pub fn collect_diagnostics(&self, mut offset: u32, buf: &mut Vec<ComputedDiagnostic>) {
        for child in &self.children {
            child.collect_diagnostics(offset, buf);
            offset += child.span().len;
        }
    }
}

#[derive(Debug)]
pub struct SyntaxToken {
    pub kind: TokenKind,
    pub span: Span,
    pub diagnostics: Vec<DiagnosticKind>,
}

impl SyntaxToken {
    pub fn collect_diagnostics(&self, offset: u32, buf: &mut Vec<ComputedDiagnostic>) {
        for kind in &self.diagnostics {
            let kind = *kind;
            buf.push(ComputedDiagnostic {
                kind,
                span: self.span.at_offset(offset),
            })
        }
    }
}

#[derive(Debug)]
pub enum SyntaxItem {
    Node(SyntaxNode),
    Token(SyntaxToken),
}

impl SyntaxItem {
    pub fn span(&self) -> Span {
        match self {
            SyntaxItem::Node(syntax_node) => syntax_node.span,
            SyntaxItem::Token(syntax_token) => syntax_token.span,
        }
    }

    pub fn as_node(&self) -> Option<&SyntaxNode> {
        match self {
            SyntaxItem::Node(node) => Some(node),
            SyntaxItem::Token(_) => None,
        }
    }

    pub fn as_mut_token(&mut self) -> Option<&mut SyntaxToken> {
        match self {
            SyntaxItem::Node(_) => None,
            SyntaxItem::Token(token) => Some(token),
        }
    }

    pub fn collect_diagnostics(&self, offset: u32, buf: &mut Vec<ComputedDiagnostic>) {
        match self {
            SyntaxItem::Node(node) => node.collect_diagnostics(offset, buf),
            SyntaxItem::Token(token) => token.collect_diagnostics(offset, buf),
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
        let end = offset + self.node.span.len;
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
                        write!(f, "{:?}", token.kind)?;
                        if !token.diagnostics.is_empty() {
                            write!(f, " {:?}", token.diagnostics)?;
                        }
                        writeln!(f)?;
                    }
                }
                offset += child.span().len;
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
