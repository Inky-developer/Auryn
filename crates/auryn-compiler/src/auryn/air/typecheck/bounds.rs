use std::fmt::Display;

use stdx::{FastIndexMap, SmallString};

use crate::auryn::{
    air::typecheck::{
        type_context::{TypeContext, TypeId},
        types::{Type, TypeData, TypeView, TypeViewKind},
    },
    syntax_id::Spanned,
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
                ty.as_type() == other.as_type() || ty.is_erroneous() || other.is_erroneous()
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
    Structural(TypeId<StructuralBound>),
}

#[derive(Debug, Clone, Copy)]
pub enum BoundView<'a> {
    Top,
    Number,
    Array(TypeViewKind<'a, ArrayBound>),
    Structural(TypeViewKind<'a, StructuralBound>),
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
            Structural(type_id) => BoundView::Structural(TypeViewKind {
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

        let contains = match self {
            Top => true,
            Number => matches!(
                other,
                TypeView::I32 | TypeView::I64 | TypeView::NumberLiteral(_)
            ),
            Array(bound) => match other {
                TypeView::Array(array) => bound.element().contains(array.element()),
                _ => false,
            },
            Structural(bound) => match other {
                TypeView::Structural(structural) => {
                    let bound_has_fields = structural
                        .fields
                        .keys()
                        .all(|key| bound.fields.contains_key(key));
                    if !bound_has_fields {
                        return false;
                    }

                    for (ident, inner_bound) in &bound.fields {
                        let Some(struct_field) = structural.fields.get(ident) else {
                            return false;
                        };
                        if !inner_bound
                            .as_view(bound.ctx)
                            .contains(struct_field.as_view(bound.ctx))
                        {
                            return false;
                        }
                    }

                    true
                }
                _ => false,
            },
        };

        contains || other.is_erroneous()
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct ArrayBound {
    pub element_bound: Bound,
}

impl TypeData for ArrayBound {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_array_bound(id)
    }

    fn visit(&self, _visitor: &mut impl FnMut(Type)) {}
}

impl<'a> TypeViewKind<'a, ArrayBound> {
    fn element(self) -> BoundView<'a> {
        self.value.element_bound.as_view(self.ctx)
    }
}

pub trait HasStructuralFields {
    fn fields(&self) -> impl Iterator<Item = (&Spanned<SmallString>, MaybeBounded)>;
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StructuralBound {
    pub fields: FastIndexMap<Spanned<SmallString>, MaybeBounded>,
}

impl TypeData for StructuralBound {
    fn from_context(id: TypeId<Self>, ctx: &TypeContext) -> &Self {
        ctx.get_structural_bound(id)
    }

    fn visit(&self, _: &mut impl FnMut(Type)) {}
}

impl HasStructuralFields for StructuralBound {
    fn fields(&self) -> impl Iterator<Item = (&Spanned<SmallString>, MaybeBounded)> {
        self.fields.iter().map(|(ident, bound)| (ident, *bound))
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
            Structural(structural) => {
                write!(f, "{{ ")?;
                for (index, (name, bound)) in structural.fields.iter().enumerate() {
                    if index != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name.value, bound.as_view(structural.ctx))?;
                }
                write!(f, "}}")
            }
        }
    }
}
