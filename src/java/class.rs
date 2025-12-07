use std::{
    io::{self, Write},
    num::NonZeroU16,
};

#[derive(Debug, Clone, Copy)]
pub struct JVMVersion {
    pub major: u16,
    pub minor: u16,
}

/// Constant pool indexes are 1-indexed, ie. the first element in the constant pool has index 1.
#[derive(Debug, Clone, Copy)]
pub struct ConstantPoolIndex(pub NonZeroU16);

impl ConstantPoolIndex {
    pub fn new(value: u16) -> Self {
        Self(NonZeroU16::new(value).expect("Value should not be 0"))
    }

    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        buf.write_all(&self.0.get().to_be_bytes())
    }
}

#[derive(Debug)]
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

/// Marks the index of an index that should be jumped to.
/// During serialization, this is replaced with the actual byte index.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JumpPoint(pub u16);

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    /// Static must be a FieldRef
    GetStatic(ConstantPoolIndex),
    /// Must be -1 <= value <= 8
    Iconst(i8),
    /// Static must be a MethodRef
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
    Goto(JumpPoint),
    Nop,
}

impl Instruction {
    pub fn serialize(&self, buf: &mut impl Write, jump_points: &[u32]) -> io::Result<()> {
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
                let byte_index = jump_points[jump_point.0 as usize] as i64;
                let own_index = jump_points.last().copied().unwrap_or(0) as i64;
                let offset: i16 = (byte_index - own_index)
                    .try_into()
                    .expect("Jump should not be too long");
                buf.write_all(&[0xa7])?;
                buf.write_all(&offset.to_be_bytes())?;
            }
            Instruction::Nop => {
                buf.write_all(&[0x0])?;
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct CodeAttribute {
    pub max_locals: u16,
    pub code: Vec<Instruction>,
}

impl CodeAttribute {
    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        // Max stack size
        // Todo: calculate that
        buf.write_all(&[0, 8])?;

        buf.write_all(&self.max_locals.to_be_bytes())?;

        let mut code_buf = Vec::new();
        // Lookup from instruction index to byte index
        let mut jump_points = vec![0];
        for instruction in &self.code {
            instruction.serialize(&mut code_buf, &jump_points)?;
            jump_points.push(
                code_buf
                    .len()
                    .try_into()
                    .expect("Instruction buffer too long"),
            );
        }
        assert!(code_buf.len() < 65536, "Code buffer is too large");
        buf.write_all(&(code_buf.len() as u32).to_be_bytes())?;
        buf.write_all(&code_buf)?;

        // Exception table length & exception table
        buf.write_all(&[0, 0])?;
        // Attributes count & attributes
        buf.write_all(&[0, 0])?;

        Ok(())
    }
}

#[derive(Debug)]
pub enum Attribute {
    Code(CodeAttribute),
}

impl Attribute {
    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        match self {
            Attribute::Code(code) => code.serialize(buf),
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
    pub fn new(class_name: String) -> Self {
        let constant_pool = vec![
            ConstantPoolEntry::Utf8(class_name),
            ConstantPoolEntry::Class {
                name_index: ConstantPoolIndex::new(1),
            },
            ConstantPoolEntry::Utf8("java/lang/Object".to_string()),
            ConstantPoolEntry::Class {
                name_index: ConstantPoolIndex::new(3),
            },
        ];
        Self {
            constant_pool,
            this_class: ConstantPoolIndex::new(2),
            super_class: ConstantPoolIndex::new(4),
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
