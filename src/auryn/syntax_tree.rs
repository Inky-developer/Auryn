use std::fmt::{Debug, Display};

use crate::auryn::BinaryOperatorToken;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum SyntaxNodeKind {
    Root,
    Number(i32),
    Expression,
    BinaryOperator(BinaryOperatorToken),
    Error,
}

#[derive(Debug)]
pub struct SyntaxNode {
    pub kind: SyntaxNodeKind,
    pub len: u32,
    pub trailing_whitespace: u32,
    pub children: Box<[SyntaxNode]>,
}

#[derive(Debug)]
pub struct SyntaxTree {
    pub root_node: SyntaxNode,
    pub leading_whitespace: u32,
}

impl SyntaxTree {
    pub fn display<'a, 'b>(&'a self, source: &'b str) -> SyntaxNodeDisplay<'a, 'b> {
        SyntaxNodeDisplay {
            node: &self.root_node,
            source,
            offset: self.leading_whitespace,
            depth: 0,
        }
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
        let kind = self.node.kind;
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
                let node = SyntaxNodeDisplay {
                    node: child,
                    source: self.source,
                    offset,
                    depth: self.depth + 1,
                };
                Display::fmt(&node, f)?;
                offset += child.len + child.trailing_whitespace;
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
