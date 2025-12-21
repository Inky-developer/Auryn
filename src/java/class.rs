use std::{
    io::{self, Write},
    num::NonZeroU16,
};

use crate::java::constant_pool_builder::ConstantPoolBuilder;

#[derive(Debug, Clone, Copy)]
pub struct JVMVersion {
    pub major: u16,
    pub minor: u16,
}

/// Constant pool indexes are 1-indexed, ie. the first element in the constant pool has index 1.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstantPoolIndex(pub NonZeroU16);

impl ConstantPoolIndex {
    pub fn new(value: u16) -> Self {
        Self(NonZeroU16::new(value).expect("Value should not be 0"))
    }

    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        buf.write_all(&self.0.get().to_be_bytes())
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ConstantPoolEntry {
    Class {
        name_index: ConstantPoolIndex,
    },
    FieldRef {
        class_index: ConstantPoolIndex,
        name_and_type_index: ConstantPoolIndex,
    },
    MethodRef {
        /// Index to Class
        class_index: ConstantPoolIndex,
        /// Index to NameAndType
        name_and_type_index: ConstantPoolIndex,
    },
    NameAndType {
        /// Index to Utf8
        name_index: ConstantPoolIndex,
        /// Index to Utf8, must be a valid descriptor
        type_index: ConstantPoolIndex,
    },
    Utf8(String),
    String {
        string_index: ConstantPoolIndex,
    },
    Integer {
        integer: i32,
    },
}

impl ConstantPoolEntry {
    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        buf.write_all(&[self.tag()])?;
        match self {
            ConstantPoolEntry::Class { name_index } => {
                name_index.serialize(buf)?;
            }
            ConstantPoolEntry::MethodRef {
                class_index,
                name_and_type_index,
            }
            | ConstantPoolEntry::FieldRef {
                class_index,
                name_and_type_index,
            } => {
                class_index.serialize(buf)?;
                name_and_type_index.serialize(buf)?;
            }
            ConstantPoolEntry::NameAndType {
                name_index,
                type_index,
            } => {
                name_index.serialize(buf)?;
                type_index.serialize(buf)?;
            }
            ConstantPoolEntry::Utf8(text) => {
                buf.write_all(&(text.len() as u16).to_be_bytes())?;
                buf.write_all(text.as_bytes())?;
            }
            ConstantPoolEntry::String { string_index } => {
                string_index.serialize(buf)?;
            }
            ConstantPoolEntry::Integer { integer } => {
                buf.write_all(&integer.to_be_bytes())?;
            }
        }

        Ok(())
    }

    pub fn tag(&self) -> u8 {
        match self {
            ConstantPoolEntry::Class { .. } => 7,
            ConstantPoolEntry::FieldRef { .. } => 9,
            ConstantPoolEntry::MethodRef { .. } => 10,
            ConstantPoolEntry::NameAndType { .. } => 12,
            ConstantPoolEntry::Utf8 { .. } => 1,
            ConstantPoolEntry::String { .. } => 8,
            ConstantPoolEntry::Integer { .. } => 3,
        }
    }
}

/// Marks the byte of an instruction that should be jumped to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JumpPoint(pub u16);

/// Must be ordered as in https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.if_cond
#[derive(Debug, Clone, Copy)]
pub enum Comparison {
    Equal,
    NotEqual,
    Less,
    GreaterOrEqual,
    Greater,
    LessOrEqual,
}

impl Comparison {
    pub fn invert(self) -> Self {
        match self {
            Comparison::Equal => Self::NotEqual,
            Comparison::NotEqual => Self::Equal,
            Comparison::Greater => Self::LessOrEqual,
            Comparison::GreaterOrEqual => Self::Less,
            Comparison::Less => Self::GreaterOrEqual,
            Comparison::LessOrEqual => Self::Greater,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    /// Static must be a FieldRef
    /// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.iconst_i
    GetStatic(ConstantPoolIndex),
    /// Must be -1 <= value <= 8
    /// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.iconst_i
    Iconst(i8),
    /// Static must be a MethodRef
    /// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.invokestatic
    InvokeStatic(ConstantPoolIndex),
    /// Index must be a MethodRef
    InvokeVirtual(ConstantPoolIndex),
    Ireturn,
    /// Loads a constant
    Ldc(u8),
    /// Returns void
    Return,
    IAdd,
    IMul,
    IStore(u16),
    ILoad(u16),
    /// Compares the two topmost stack entries according to the given `comparison`
    /// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.if_icmp_cond
    IfICmp {
        comparison: Comparison,
        jump_point: JumpPoint,
    },
    /// Compares the topmost stack entry with 0 according to the given `comparison`
    /// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.if_cond
    IfI {
        comparison: Comparison,
        jump_point: JumpPoint,
    },
    Goto(JumpPoint),
    Nop,
    /// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.pop
    Pop,
    /// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.pop2
    Pop2,
}

impl Instruction {
    pub fn serialize(&self, buf: &mut impl Write, current_index: u16) -> io::Result<()> {
        match *self {
            Instruction::GetStatic(reference) => {
                buf.write_all(&[0xb2])?;
                reference.serialize(buf)?;
            }
            Instruction::Iconst(value) => {
                assert!((-1..=8).contains(&value), "Invalid value for iconst");
                buf.write_all(&(3 + value).to_be_bytes())?;
            }
            Instruction::InvokeStatic(reference) => {
                buf.write_all(&[0xb8])?;
                reference.serialize(buf)?;
            }
            Instruction::InvokeVirtual(reference) => {
                buf.write_all(&[0xb6])?;
                reference.serialize(buf)?;
            }
            Instruction::Ireturn => {
                buf.write_all(&[0xac])?;
            }
            Instruction::Ldc(index) => {
                buf.write_all(&[0x12, index])?;
            }
            Instruction::Return => {
                buf.write_all(&[0xb1])?;
            }
            Instruction::IAdd => {
                buf.write_all(&[0x60])?;
            }
            Instruction::IMul => {
                buf.write_all(&[0x68])?;
            }
            Instruction::IStore(index) => {
                // TODO: implement istore_<n> as space optimization
                let index: u8 = index.try_into().expect("TODO: Implement wide store");
                buf.write_all(&[0x36, index])?;
            }
            Instruction::ILoad(index) => {
                // TODO: implement iload_<n> as space optimization
                let index: u8 = index
                    .try_into()
                    .expect("TODO: Implement support for wide loads");
                buf.write_all(&[0x15, index])?;
            }
            Instruction::Goto(jump_point) => {
                let offset: i16 = jump_point.0 as i16 - current_index as i16;
                buf.write_all(&[0xa7])?;
                buf.write_all(&offset.to_be_bytes())?;
            }
            Instruction::Nop => {
                buf.write_all(&[0x0])?;
            }
            Instruction::IfICmp {
                comparison,
                jump_point,
            } => {
                buf.write_all(&[0x9f + (comparison as u8)])?;
                let offset: i16 = jump_point.0 as i16 - current_index as i16;
                buf.write_all(&offset.to_be_bytes())?;
            }
            Instruction::IfI {
                comparison,
                jump_point,
            } => {
                buf.write_all(&[0x99 + (comparison as u8)])?;
                let offset: i16 = jump_point.0 as i16 - current_index as i16;
                buf.write_all(&offset.to_be_bytes())?;
            }
            Instruction::Pop => {
                buf.write_all(&[0x57])?;
            }
            Instruction::Pop2 => {
                buf.write_all(&[0x58])?;
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct CodeAttribute {
    pub max_locals: u16,
    pub code: Vec<Instruction>,
    pub attributes: Vec<AttributeInfo>,
}

impl CodeAttribute {
    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        // Max stack size
        // Todo: calculate that
        buf.write_all(&[0, 8])?;

        buf.write_all(&self.max_locals.to_be_bytes())?;

        let mut code_buf = Vec::new();
        for instruction in &self.code {
            let current_index = code_buf
                .len()
                .try_into()
                .expect("Should not be too many instructions");
            instruction.serialize(&mut code_buf, current_index)?;
        }
        assert!(code_buf.len() < 65536, "Code buffer is too large");
        buf.write_all(&(code_buf.len() as u32).to_be_bytes())?;
        buf.write_all(&code_buf)?;

        // Exception table length & exception table
        buf.write_all(&[0, 0])?;

        // Attributes count & attributes
        let attribute_count: u16 = self
            .attributes
            .len()
            .try_into()
            .expect("Should not have too many attributes");
        buf.write_all(&attribute_count.to_be_bytes())?;
        for attribute in &self.attributes {
            attribute.serialize(buf)?;
        }

        Ok(())
    }
}

/// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-2.html#jvms-2.11.1-320
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TypeCategory {
    Normal,
    Big,
}

impl TypeCategory {
    pub fn stack_size(&self) -> u16 {
        match self {
            Self::Normal => 1,
            Self::Big => 2,
        }
    }
}

/// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.7.4
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum VerificationTypeInfo {
    // Supertype of everything
    Top,
    Integer,
    Float,
    Null,
    UninitializedThis,
    // An object of a type indicated by the class_info at the corresponding constant pool index
    Object {
        constant_pool_index: ConstantPoolIndex,
    },
    // Not sure what the offset is for
    UninitializedVariable {
        offset: u16,
    },
    Long,
    Double,
}

impl VerificationTypeInfo {
    pub fn category(&self) -> TypeCategory {
        match self {
            Self::Top
            | Self::Integer
            | Self::Float
            | Self::Null
            | Self::UninitializedThis
            | Self::Object { .. }
            | Self::UninitializedVariable { .. } => TypeCategory::Normal,
            Self::Long | Self::Double => TypeCategory::Big,
        }
    }

    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        match self {
            VerificationTypeInfo::Top => buf.write_all(&[0x0]),
            VerificationTypeInfo::Integer => buf.write_all(&[0x1]),
            VerificationTypeInfo::Float => buf.write_all(&[0x2]),
            VerificationTypeInfo::Null => buf.write_all(&[0x5]),
            VerificationTypeInfo::UninitializedThis => buf.write_all(&[0x6]),
            VerificationTypeInfo::Object {
                constant_pool_index,
            } => {
                buf.write_all(&[0x7])?;
                constant_pool_index.serialize(buf)
            }
            VerificationTypeInfo::UninitializedVariable { offset } => {
                buf.write_all(&[0x8])?;
                buf.write_all(&offset.to_be_bytes())
            }
            VerificationTypeInfo::Long => buf.write_all(&[0x4]),
            VerificationTypeInfo::Double => buf.write_all(&[0x3]),
        }
    }
}

/// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.7.4
#[derive(Debug)]
pub enum StackMapFrame {
    Same {
        offset_delta: u8,
    },
    Full {
        offset_delta: u16,
        locals: Vec<VerificationTypeInfo>,
        stack: Vec<VerificationTypeInfo>,
    },
}

impl StackMapFrame {
    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        match self {
            StackMapFrame::Same { offset_delta } => {
                assert!(
                    *offset_delta <= 63,
                    "TODO: Add the extended frame to support bigger offsets"
                );
                buf.write_all(&(*offset_delta).to_be_bytes())?;
            }
            StackMapFrame::Full {
                offset_delta,
                locals,
                stack,
            } => {
                // Frame type
                buf.write_all(&[255])?;
                buf.write_all(&offset_delta.to_be_bytes())?;

                let num_locals: u16 = locals.len().try_into().expect("Should not be too long");
                buf.write_all(&num_locals.to_be_bytes())?;
                for local in locals {
                    local.serialize(buf)?;
                }

                let num_stack_entries: u16 =
                    stack.len().try_into().expect("Should not be too long");
                buf.write_all(&num_stack_entries.to_be_bytes())?;
                for stack_entry in stack {
                    stack_entry.serialize(buf)?;
                }
            }
        }

        Ok(())
    }
}

/// https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.7.4
#[derive(Debug)]
pub struct StackMapTableAttribute {
    pub entries: Vec<StackMapFrame>,
}

impl StackMapTableAttribute {
    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        let mut entry_buf = Vec::new();
        for entry in &self.entries {
            entry.serialize(&mut entry_buf)?;
        }

        let num_entries: u16 = self
            .entries
            .len()
            .try_into()
            .expect("Should not have too many entries");
        buf.write_all(&num_entries.to_be_bytes())?;
        buf.write_all(&entry_buf)?;

        Ok(())
    }
}

#[derive(Debug)]
pub enum Attribute {
    Code(CodeAttribute),
    StackMapTable(StackMapTableAttribute),
}

impl Attribute {
    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        match self {
            Attribute::Code(code) => code.serialize(buf),
            Attribute::StackMapTable(table) => table.serialize(buf),
        }
    }
}

#[derive(Debug)]
pub struct AttributeInfo {
    pub name_index: ConstantPoolIndex,
    pub attribute: Attribute,
}

impl AttributeInfo {
    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        self.name_index.serialize(buf)?;

        let mut attribute_buf = Vec::new();
        self.attribute.serialize(&mut attribute_buf)?;
        buf.write_all(&(attribute_buf.len() as u32).to_be_bytes())?;
        buf.write_all(&attribute_buf)?;

        Ok(())
    }
}

#[repr(u16)]
pub enum MethodAccessFlag {
    Public = 0x0001,
    Static = 0x0008,
    Final = 0x0010,
}

#[derive(Debug)]
pub struct Method {
    pub flags: u16,
    /// Index into the constant pool that must reference a Utf8 entry
    pub name_index: ConstantPoolIndex,
    /// Index into the constant pool that must reference a Utf8 entry
    pub descriptor_index: ConstantPoolIndex,
    pub attributes: Vec<AttributeInfo>,
}

impl Method {
    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        buf.write_all(&self.flags.to_be_bytes())?;
        self.name_index.serialize(buf)?;
        self.descriptor_index.serialize(buf)?;

        buf.write_all(&(self.attributes.len() as u16).to_be_bytes())?;
        for attribute in &self.attributes {
            attribute.serialize(buf)?;
        }

        Ok(())
    }
}

#[repr(u16)]
pub enum ClassAccessFlag {
    Public = 0x0001,
    Final = 0x0010,
}

#[derive(Debug)]
pub struct ClassData {
    pub constant_pool: Vec<ConstantPoolEntry>,
    // Index into the constant pool that must reference a class entry
    pub this_class: ConstantPoolIndex,
    // Index into the constant pool that must reference a class entry
    pub super_class: ConstantPoolIndex,
    pub methods: Vec<Method>,
}

pub const MAGIC: [u8; 4] = 0xCAFEBABE_u32.to_be_bytes();
pub const JVM_VERSION: JVMVersion = JVMVersion {
    major: 65,
    minor: 0,
};
pub const ACCESSS_FLAG: u16 = (ClassAccessFlag::Public as u16) | (ClassAccessFlag::Final as u16);

impl ClassData {
    pub fn new(class_name: String, mut builder: ConstantPoolBuilder) -> Self {
        let this_class = builder.add_class(class_name);
        let super_class = builder.add_class("java/lang/Object".to_string());
        let constant_pool = builder.build();
        Self {
            constant_pool,
            this_class,
            super_class,
            methods: Vec::new(),
        }
    }

    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        buf.write_all(&MAGIC)?;

        buf.write_all(&JVM_VERSION.minor.to_be_bytes())?;
        buf.write_all(&JVM_VERSION.major.to_be_bytes())?;

        buf.write_all(&(self.constant_pool.len() as u16 + 1).to_be_bytes())?;
        for entry in &self.constant_pool {
            entry.serialize(buf)?;
        }

        buf.write_all(&ACCESSS_FLAG.to_be_bytes())?;

        self.this_class.serialize(buf)?;
        self.super_class.serialize(buf)?;

        // Interfaces count & interfaces
        buf.write_all(&[0, 0])?;

        // Fields count & fields
        buf.write_all(&[0, 0])?;

        buf.write_all(&(self.methods.len() as u16).to_be_bytes())?;
        for method in &self.methods {
            method.serialize(buf)?;
        }

        // Attributes count & attributes
        buf.write_all(&[0, 0])?;

        Ok(())
    }
}
