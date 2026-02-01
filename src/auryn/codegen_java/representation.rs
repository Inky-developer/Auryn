use std::fmt::{Display, Write};

use stdx::{FastMap, SmallString};

use crate::{
    auryn::air::typecheck::{
        type_context::{TypeContext, TypeId},
        types::{FunctionItemType, StructType, StructuralType, TypeView, TypeViewKind},
    },
    java::{
        class::{ConstantPoolIndex, PrimitiveType, TypeCategory, VerificationTypeInfo},
        constant_pool_builder::ConstantPoolBuilder,
    },
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

    pub fn into_primitive_type_or_object(self) -> PrimitiveOrObject {
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

    pub fn is_printable(&self) -> bool {
        matches!(
            self,
            Representation::Integer | Representation::Boolean | Representation::Long
        )
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

    pub fn object() -> Self {
        FieldDescriptor::Object("java/lang/Object".into())
    }

    pub fn print_stream() -> Self {
        FieldDescriptor::Object("java/io/PrintStream".into())
    }

    pub fn into_base_object_or_primitive(self) -> Self {
        match self {
            FieldDescriptor::Object(_) => FieldDescriptor::object(),
            other => other,
        }
    }

    pub fn into_primitive(self) -> Representation {
        match self {
            FieldDescriptor::Integer => Representation::Integer,
            FieldDescriptor::Long => Representation::Long,
            FieldDescriptor::Boolean => Representation::Boolean,
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

    /// Returns a valid jvm class name, so that unique names for structural types can be generated
    pub fn mangled_name(&self) -> impl Display {
        fn mangle_class_name(name: &str) -> impl Display {
            std::fmt::from_fn(|f| {
                for char in name.chars() {
                    match char {
                        '/' => f.write_char('$')?,
                        _ => f.write_char(char)?,
                    }
                }

                Ok(())
            })
        }
        std::fmt::from_fn(move |f| match self {
            FieldDescriptor::Byte => write!(f, "B"),
            FieldDescriptor::Char => write!(f, "C"),
            FieldDescriptor::Double => write!(f, "D"),
            FieldDescriptor::Float => write!(f, "F"),
            FieldDescriptor::Integer => write!(f, "I"),
            FieldDescriptor::Long => write!(f, "J"),
            FieldDescriptor::Object(class_name) => write!(f, "L{}", mangle_class_name(class_name)),
            FieldDescriptor::Short => write!(f, "S"),
            FieldDescriptor::Boolean => write!(f, "Z"),
            FieldDescriptor::Array {
                dimension_count,
                descriptor,
            } => {
                for _ in 0..*dimension_count {
                    write!(f, "A")?;
                }
                write!(f, "{}", descriptor.mangled_name())?;
                Ok(())
            }
        })
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
    pub const VOID: Self = MethodDescriptor {
        parameters: Vec::new(),
        return_type: ReturnDescriptor::Void,
    };

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
    pub is_named: bool,
    pub is_zero_sized: bool,
}

impl StructuralRepr {
    pub fn to_representation(&self) -> Option<Representation> {
        if self.is_zero_sized {
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
    pub(super) struct_types: FastMap<TypeId<StructType>, StructuralRepr>,
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
            Struct(struct_type) => self.get_struct_repr(struct_type).to_representation(),
            FunctionItem(_) | Intrinsic(_) | Meta(_) | Module(_) => None,
            Error => unreachable!("Called with error type"),
        }
    }

    /// Returns the representation of an auryn type for the jvm
    pub fn get_function_representation(
        &mut self,
        ty: TypeViewKind<FunctionItemType>,
    ) -> MethodDescriptor {
        let parameters = ty
            .parameters()
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
            let repr = self.compute_structural_repr(ty.ctx, ty.value);
            self.structural_types.insert(ty.id, repr);
        }
        self.structural_types.get(&ty.id).unwrap()
    }

    pub fn get_struct_repr(&mut self, ty: TypeViewKind<'_, StructType>) -> &StructuralRepr {
        if !self.struct_types.contains_key(&ty.id) {
            let class_name = ty.ident.clone();
            // We need to already insert something to prevent infinite recursion
            // It is fine that the fields are empty, since they are not needed to compute the repr
            self.struct_types.insert(
                ty.id,
                StructuralRepr {
                    fields: Vec::new(),
                    class_name: class_name.clone(),
                    is_named: true,
                    is_zero_sized: false,
                },
            );
            let fields = ty
                .fields()
                .flat_map(|(ident, ty)| {
                    self.get_representation(ty)
                        .map(|repr| (ident.clone(), repr))
                })
                .collect::<Vec<_>>();
            let repr = StructuralRepr {
                is_zero_sized: fields.is_empty(),
                fields,
                is_named: true,
                class_name,
            };
            self.struct_types.insert(ty.id, repr);
        }
        self.struct_types.get(&ty.id).unwrap()
    }

    fn compute_structural_repr(
        &mut self,
        ty_ctx: &TypeContext,
        ty: &StructuralType,
    ) -> StructuralRepr {
        let mut name = format!("Structural${}", ty.fields.len());
        for (_, ty) in &ty.fields {
            let ty = ty.as_view(ty_ctx);
            let ty_name = self
                .get_representation(ty)
                .map(|repr| repr.into_field_descriptor());
            if let Some(ty_name) = ty_name.as_ref() {
                write!(name, "{}", ty_name.mangled_name()).unwrap();
            } else {
                write!(name, "0").unwrap();
            }
        }

        let fields = ty
            .fields
            .iter()
            .flat_map(|(ident, ty)| {
                self.get_representation(ty.as_view(ty_ctx))
                    .map(|repr| (ident.clone(), repr))
            })
            .collect::<Vec<_>>();

        StructuralRepr {
            is_zero_sized: fields.is_empty(),
            fields,
            is_named: false,
            class_name: name.into(),
        }
    }
}
