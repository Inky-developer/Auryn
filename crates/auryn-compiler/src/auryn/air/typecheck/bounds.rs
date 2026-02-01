use std::fmt::Display;

use crate::auryn::air::typecheck::{
    type_context::{FromTypeContext, TypeContext, TypeId},
    types::{Type, TypeView, TypeViewKind},
};

/// Represents either a bound or a type
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum MaybeBounded {
    Bounded(Bound),
    Type(Type),
}

#[derive(Debug, Clone, Copy)]
pub enum MaybeBoundedView<'a> {
    Bounded(BoundView<'a>),
    Type(TypeView<'a>),
}

impl MaybeBounded {
    pub fn as_view(self, ctx: &'_ TypeContext) -> MaybeBoundedView<'_> {
        match self {
            MaybeBounded::Bounded(bound) => MaybeBoundedView::Bounded(bound.as_view(ctx)),
            MaybeBounded::Type(ty) => MaybeBoundedView::Type(ty.as_view(ctx)),
        }
    }

    pub fn as_type(self) -> Option<Type> {
        match self {
            MaybeBounded::Type(ty) => Some(ty),
            _ => None,
        }
    }
}

impl MaybeBoundedView<'_> {
    pub fn contains(&self, other: TypeView) -> bool {
        match self {
            MaybeBoundedView::Bounded(bound) => bound.contains(other),
            MaybeBoundedView::Type(ty) => {
                ty.as_type() == other.as_type()
                    || matches!(ty, TypeView::Error)
                    || matches!(other, TypeView::Error)
            }
        }
    }
}

impl Display for MaybeBoundedView<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MaybeBoundedView::Bounded(bound_view) => Display::fmt(bound_view, f),
            MaybeBoundedView::Type(type_view) => Display::fmt(type_view, f),
        }
    }
}

/// A [`Bound`] restricts, how which types are accepted, as defined in
/// [`BoundView::contains`].
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Bound {
    Top,
    Number,
    Array(TypeId<ArrayBound>),
}

#[derive(Debug, Clone, Copy)]
pub enum BoundView<'a> {
    Top,
    Number,
    Array(TypeViewKind<'a, ArrayBound>),
}

impl Bound {
    pub fn as_view(self, ctx: &TypeContext) -> BoundView<'_> {
        use Bound::*;
        match self {
            Top => BoundView::Top,
            Number => BoundView::Number,
            Array(type_id) => BoundView::Array(TypeViewKind {
                id: type_id,
                value: ctx.get(type_id),
                ctx,
            }),
        }
    }

    pub fn as_bounded(self) -> MaybeBounded {
        MaybeBounded::Bounded(self)
    }
}

impl BoundView<'_> {
    pub fn contains(self, other: TypeView) -> bool {
        use BoundView::*;

        if matches!(other, TypeView::Error) {
            return true;
        }

        match self {
            Top => true,
            Number => matches!(
                other,
                TypeView::I32 | TypeView::I64 | TypeView::NumberLiteral(_)
            ),
            Array(bound) => match other {
                TypeView::Array(array) => bound.element().contains(array.element()),
                _ => false,
            },
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct ArrayBound {
    pub element_bound: Bound,
}

impl FromTypeContext for ArrayBound {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_array_bound(id)
    }
}

impl<'a> TypeViewKind<'a, ArrayBound> {
    fn element(self) -> BoundView<'a> {
        self.value.element_bound.as_view(self.ctx)
    }
}

impl Display for BoundView<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BoundView::*;
        match self {
            Top => write!(f, "Any"),
            Number => write!(f, "Number"),
            Array(type_view_kind) => {
                write!(f, "[]")?;
                type_view_kind.element().fmt(f)
            }
        }
    }
}
