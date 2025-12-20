use std::iter::Sum;

pub mod air;
pub mod ast;
pub mod codegen_java;
pub mod parser;
pub mod syntax_tree;
pub mod tokenizer;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub len: u32,
}

impl Span {
    pub const EMPTY: Span = Span { len: 0 };

    fn add(self, other: Span) -> Span {
        Span {
            len: self.len + other.len,
        }
    }

    fn at_offset(self, offset: u32) -> ComputedSpan {
        let Span { len } = self;
        ComputedSpan { offset, len }
    }
}

impl Sum for Span {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(|acc, next| acc.add(next))
            .unwrap_or(Span::EMPTY)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ComputedSpan {
    pub offset: u32,
    pub len: u32,
}
