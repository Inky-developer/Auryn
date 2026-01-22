use std::fmt::{Display, Write};

use crate::{
    auryn::air::typecheck::{
        type_context::TypeId,
        types::{FunctionItemType, StructuralType, TypeView, TypeViewKind},
    },
    java::{
        class::{ConstantPoolIndex, PrimitiveType, TypeCategory, VerificationTypeInfo},
        constant_pool_builder::ConstantPoolBuilder,
    },
    utils::{fast_map::FastMap, small_string::SmallString},
};

/// The representation of a type in the jvm, can be converted into field descriptors and verification type info
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Representation {
    Integer,
    Long,
    Boolean,
    Array(Box<Representation>),
    Object(SmallString),
}

impl Representation {
    pub fn string() -> Self {
        Representation::Object("java/lang/String".into())
    }

    pub fn to_primitive_type_or_object(self) -> PrimitiveOrObject {
        match self {
            Representation::Integer => PrimitiveOrObject::Primitive(PrimitiveType::Int),
            Representation::Long => PrimitiveOrObject::Primitive(PrimitiveType::Long),
            Representation::Boolean => PrimitiveOrObject::Primitive(PrimitiveType::Boolean),
            Representation::Object(descriptor) => PrimitiveOrObject::Object(descriptor),
            Representation::Array(inner) => PrimitiveOrObject::Object(
                FieldDescriptor::Array {
                    dimension_count: 1,
                    descriptor: Box::new(inner.into_field_descriptor()),
                }
                .to_string()
                .into(),
            ),
        }
    }

    pub fn into_field_descriptor(self) -> FieldDescriptor {
        match self {
            Representation::Integer => FieldDescriptor::Integer,
            Representation::Long => FieldDescriptor::Long,
            Representation::Boolean => FieldDescriptor::Boolean,
            Representation::Object(r#type) => FieldDescriptor::Object(r#type),
            Representation::Array(element_type) => FieldDescriptor::Array {
                dimension_count: 1,
                descriptor: Box::new(element_type.into_field_descriptor()),
            },
        }
    }

    pub fn into_verification_type(self, pool: &mut ConstantPoolBuilder) -> VerificationTypeInfo {
        match self {
            Representation::Integer => VerificationTypeInfo::Integer,
            Representation::Long => VerificationTypeInfo::Long,
            Representation::Boolean => VerificationTypeInfo::Integer,
            Representation::Array(inner) => VerificationTypeInfo::Object {
                constant_pool_index: pool.add_array_class(inner.into_field_descriptor()),
            },
            Representation::Object(object) => VerificationTypeInfo::Object {
                constant_pool_index: pool.add_class(object),
            },
        }
    }

    pub fn category(&self) -> TypeCategory {
        match self {
            Representation::Integer
            | Representation::Boolean
            | Representation::Array(_)
            | Representation::Object(_) => TypeCategory::Normal,
            Representation::Long => TypeCategory::Big,
        }
    }

    pub fn stack_size(&self) -> u16 {
        self.category().stack_size()
    }
}

#[derive(Debug, Clone)]
pub enum PrimitiveOrObject {
    Primitive(PrimitiveType),
    Object(SmallString),
}

/// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-FieldType>
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FieldDescriptor {
    Byte,
    Char,
    Double,
    Float,
    Integer,
    Long,
    Object(SmallString),
    Short,
    Boolean,
    Array {
        dimension_count: u8,
        descriptor: Box<FieldDescriptor>,
    },
}

impl FieldDescriptor {
    pub fn string() -> Self {
        FieldDescriptor::Object("java/lang/String".into())
    }

    pub fn print_stream() -> Self {
        FieldDescriptor::Object("java/io/PrintStream".into())
    }

    pub fn into_primitive(self) -> Representation {
        match self {
            FieldDescriptor::Byte
            | FieldDescriptor::Char
            | FieldDescriptor::Integer
            | FieldDescriptor::Short
            | FieldDescriptor::Boolean => Representation::Integer,
            FieldDescriptor::Long => Representation::Long,
            FieldDescriptor::Object(name) => Representation::Object(name),
            FieldDescriptor::Array {
                dimension_count,
                descriptor,
            } => {
                assert_eq!(dimension_count, 1, "Higher dimensions not implemented yet");
                Representation::Array(Box::new(descriptor.into_primitive()))
            }
            other => todo!("No primitive for {other} yet"),
        }
    }
}

impl Display for FieldDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FieldDescriptor::Byte => write!(f, "B"),
            FieldDescriptor::Char => write!(f, "C"),
            FieldDescriptor::Double => write!(f, "D"),
            FieldDescriptor::Float => write!(f, "F"),
            FieldDescriptor::Integer => write!(f, "I"),
            FieldDescriptor::Long => write!(f, "J"),
            FieldDescriptor::Object(class_name) => write!(f, "L{class_name};"),
            FieldDescriptor::Short => write!(f, "S"),
            FieldDescriptor::Boolean => write!(f, "Z"),
            FieldDescriptor::Array {
                dimension_count,
                descriptor,
            } => {
                for _ in 0..*dimension_count {
                    write!(f, "[")?;
                }
                descriptor.fmt(f)?;
                Ok(())
            }
        }
    }
}

/// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-ReturnDescriptor>
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReturnDescriptor {
    Value(FieldDescriptor),
    Void,
}

impl ReturnDescriptor {
    pub fn into_primitive(self) -> Option<Representation> {
        match self {
            ReturnDescriptor::Value(value) => Some(value.into_primitive()),
            ReturnDescriptor::Void => None,
        }
    }
}

impl Display for ReturnDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReturnDescriptor::Value(field_descriptor) => field_descriptor.fmt(f),
            ReturnDescriptor::Void => f.write_str("V"),
        }
    }
}

impl From<FieldDescriptor> for ReturnDescriptor {
    fn from(value: FieldDescriptor) -> Self {
        ReturnDescriptor::Value(value)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ImplicitArgs {
    None,
    This,
    UninitializedThis,
}

impl ImplicitArgs {
    pub fn as_verification_type(
        &self,
        this_index: ConstantPoolIndex,
    ) -> Option<VerificationTypeInfo> {
        match self {
            ImplicitArgs::None => None,
            ImplicitArgs::This => Some(VerificationTypeInfo::Object {
                constant_pool_index: this_index,
            }),
            ImplicitArgs::UninitializedThis => Some(VerificationTypeInfo::UninitializedThis),
        }
    }

    pub fn stack_size(self) -> u16 {
        match self {
            ImplicitArgs::None => 0,
            ImplicitArgs::This | ImplicitArgs::UninitializedThis => 1,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodDescriptor {
    pub parameters: Vec<FieldDescriptor>,
    pub return_type: ReturnDescriptor,
}

impl MethodDescriptor {
    /// Returns the index of the first valid local variable of this method
    pub fn first_variable_index(&self, implicit_args: ImplicitArgs) -> u16 {
        implicit_args.stack_size()
            + self
                .parameters
                .iter()
                .map(|param| param.clone().into_primitive().stack_size())
                .sum::<u16>()
    }
}

impl Display for MethodDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for arg in &self.parameters {
            arg.fmt(f)?;
        }
        write!(f, ")")?;

        self.return_type.fmt(f)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct StructuralRepr {
    pub fields: Vec<(SmallString, Representation)>,
    pub class_name: SmallString,
}

impl StructuralRepr {
    pub fn is_zero_sized(&self) -> bool {
        self.fields.is_empty()
    }

    fn to_representation(&self) -> Option<Representation> {
        if self.is_zero_sized() {
            None
        } else {
            Some(Representation::Object(self.class_name.clone()))
        }
    }

    pub fn init_descriptor(&self) -> MethodDescriptor {
        MethodDescriptor {
            parameters: self
                .fields
                .iter()
                .map(|(_, repr)| repr.clone().into_field_descriptor())
                .collect(),
            return_type: ReturnDescriptor::Void,
        }
    }
}

#[derive(Debug, Default)]
pub struct RepresentationCtx {
    pub(super) structural_types: FastMap<TypeId<StructuralType>, StructuralRepr>,
}

impl RepresentationCtx {
    /// Returns the representation of an auryn type for the jvm.
    /// Returns [`None`] if the type is not represented at runtime (because it is a compile time construct or zero-sized)
    pub fn get_representation(&mut self, air_type: TypeView) -> Option<Representation> {
        use TypeView::*;
        match air_type {
            I32 => Some(Representation::Integer),
            I64 => Some(Representation::Long),
            NumberLiteral(_) => None,
            Bool => Some(Representation::Boolean),
            String => Some(Representation::string()),
            Array(content_type) => {
                let content_repr = self.get_representation(content_type.element());
                match content_repr {
                    Some(repr) => Some(Representation::Array(Box::new(repr))),
                    None => todo!("Add representation for array of zero-sized types"),
                }
            }
            Extern(extern_type) => Some(Representation::Object(extern_type.extern_name.clone())),
            Structural(structural_type) => self
                .get_structural_repr(structural_type)
                .to_representation(),
            FunctionItem(_) | Intrinsic(_) | Meta(_) | Module(_) => None,
            Error => unreachable!("Called with error type"),
        }
    }

    /// Returns the representation of `air_type` as a jvm return [`ReturnDescriptor`]
    pub fn get_return_type_representation(&mut self, air_type: TypeView) -> ReturnDescriptor {
        self.get_representation(air_type)
            .map(|primitive| primitive.into_field_descriptor().into())
            .unwrap_or(ReturnDescriptor::Void)
    }

    /// Returns the representation of an auryn type for the jvm
    pub fn get_function_representation(
        &mut self,
        ty: TypeViewKind<FunctionItemType>,
    ) -> MethodDescriptor {
        let parameters = ty
            .constrained_parameters()
            .iter()
            .flat_map(|it| {
                self.get_representation(it.as_view(ty.ctx))
                    .map(|it| it.into_field_descriptor())
            })
            .collect();
        let return_type = self
            .get_representation(ty.r#return())
            .map_or(ReturnDescriptor::Void, |it| {
                it.into_field_descriptor().into()
            });
        MethodDescriptor {
            parameters,
            return_type,
        }
    }

    pub fn get_structural_repr(&mut self, ty: TypeViewKind<'_, StructuralType>) -> &StructuralRepr {
        if !self.structural_types.contains_key(&ty.id) {
            let mut name = format!("Structural{}", ty.value.fields.len());
            for (_, ty) in ty.fields() {
                name.push('$');
                write!(name, "{ty}").unwrap();
            }

            let fields = ty
                .fields()
                .flat_map(|(ident, ty)| {
                    self.get_representation(ty)
                        .map(|repr| (ident.clone(), repr))
                })
                .collect();

            let repr = StructuralRepr {
                fields,
                class_name: name.into(),
            };

            self.structural_types.insert(ty.id, repr);
        }
        self.structural_types.get(&ty.id).unwrap()
    }
}
