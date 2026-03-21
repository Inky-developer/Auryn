use stdx::{FastMap, SmallString, default};

use crate::auryn::air::{
    data::AirFunctionId,
    typecheck::{
        type_context::TypeContext,
        types::{Type, TypeView},
    },
};

#[derive(Debug, Default)]
pub struct ImplementationBlock {
    functions: FastMap<SmallString, AirFunctionId>,
}

#[derive(Debug)]
pub struct ImplementationInfo<'a, const N: usize> {
    implementations: [Option<&'a ImplementationBlock>; N],
}

impl<'a, const N: usize> ImplementationInfo<'a, N> {
    pub fn get(&self, ident: &str) -> Option<AirFunctionId> {
        self.implementations
            .iter()
            .flatten()
            .find_map(|block| block.functions.get(ident))
            .copied()
    }
}

#[derive(Debug)]
pub enum InsertionError {
    TypeIsTooGeneric,
}

/// Stores implementation blocks (`impl SomeType {...}`) in an efficient to look-up way.
#[derive(Debug, Default)]
pub struct Implementations {
    /// Stores implementations for a concrete block
    by_type: FastMap<Type, ImplementationBlock>,
}

impl Implementations {
    pub fn get<'a>(&'a self, ty: TypeView<'_>) -> ImplementationInfo<'a, 2> {
        let by_type = self.by_type.get(&ty.as_type());
        let by_type_producer = if let TypeView::Application(application) = ty {
            let type_producer = Type::TypeProducer(application.r#type);
            self.by_type.get(&type_producer)
        } else {
            None
        };
        ImplementationInfo {
            implementations: [by_type, by_type_producer],
        }
    }

    /// Adds the function with the given ident to the implementation list for the type.
    /// Returns the previous function on that type, if it exists
    pub fn add(
        &mut self,
        ty: Type,
        ty_ctx: &TypeContext,
        ident: SmallString,
        id: AirFunctionId,
    ) -> Result<Option<AirFunctionId>, InsertionError> {
        let effective_type = match ty {
            Type::Application(application)
                if ty_ctx
                    .get(application)
                    .arguments
                    .iter()
                    .all(|arg| matches!(*arg, Type::Generic(_))) =>
            {
                Type::TypeProducer(ty_ctx.get(application).r#type)
            }
            _ => ty,
        };

        // Since the lookup is by exact type, we cannot add generic types here, because they would not match.
        // Instead supported generic types are special cased, like application types above
        let mut visited_types = default();
        ty.as_view(ty_ctx).visit(&mut visited_types);
        if visited_types
            .iter()
            .any(|it| matches!(it, Type::Generic(_)))
        {
            return Err(InsertionError::TypeIsTooGeneric);
        }

        Ok(self
            .by_type
            .entry(effective_type)
            .or_default()
            .functions
            .insert(ident, id))
    }
}
