use std::{
    fmt::{Debug, Display},
    io::{self, Write},
    num::NonZeroU16,
    ops::Index,
};

use crate::{
    bitflags,
    java::{
        constant_pool_builder::ConstantPoolBuilder,
        display::{
            ConstantPoolDisplay, ConstantPoolEntryDisplay, FieldDisplay, InstructionDisplay,
            MethodDisplay, VerificationTypeInfoDisplay,
        },
    },
    utils::small_string::SmallString,
};

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

/// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.4>
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
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
    Utf8(SmallString),
    String {
        string_index: ConstantPoolIndex,
    },
    Integer {
        integer: i32,
    },
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.4.5>
    Long {
        long: i64,
    },
}

impl ConstantPoolEntry {
    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        buf.write_all(&[self.tag()])?;
        match self {
            ConstantPoolEntry::Class { name_index } => name_index.serialize(buf)?,
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
            ConstantPoolEntry::String { string_index } => string_index.serialize(buf)?,
            ConstantPoolEntry::Integer { integer } => buf.write_all(&integer.to_be_bytes())?,
            ConstantPoolEntry::Long { long } => {
                let hi_bytes = (long >> 32) as i32;
                let lo_bytes = *long as i32;
                buf.write_all(&hi_bytes.to_be_bytes())?;
                buf.write_all(&lo_bytes.to_be_bytes())?;
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
            ConstantPoolEntry::Long { .. } => 5,
        }
    }

    pub fn size_in_constant_pool(&self) -> u16 {
        match self {
            Self::Long { .. } => 2,
            _ => 1,
        }
    }

    pub fn display<'a>(&'a self, pool: &'a ConstantPool) -> ConstantPoolEntryDisplay<'a> {
        ConstantPoolEntryDisplay { entry: self, pool }
    }
}

pub struct ConstantPool {
    pub(super) entries: Box<[Option<ConstantPoolEntry>]>,
}

impl ConstantPool {
    fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        let effective_size = self
            .entries
            .iter()
            .flatten()
            .map(ConstantPoolEntry::size_in_constant_pool)
            .sum::<u16>();
        buf.write_all(&(effective_size + 1).to_be_bytes())?;
        for entry in self.entries.iter().flatten() {
            entry.serialize(buf)?;
        }
        Ok(())
    }
}

impl Index<ConstantPoolIndex> for ConstantPool {
    type Output = ConstantPoolEntry;

    fn index(&self, index: ConstantPoolIndex) -> &Self::Output {
        self.entries[index.0.get() as usize].as_ref().unwrap()
    }
}

/// Marks the byte of an instruction that should be jumped to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JumpPoint(pub u16);

/// Must be ordered as in <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.if_cond>
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

    pub fn as_str(self) -> &'static str {
        match self {
            Comparison::Equal => "==",
            Comparison::NotEqual => "!=",
            Comparison::Less => "<",
            Comparison::GreaterOrEqual => ">=",
            Comparison::Greater => ">",
            Comparison::LessOrEqual => "<=",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    /// Creates a new object of the given class
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.new>
    New(ConstantPoolIndex),
    /// Static must be a FieldRef
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.getstatic>
    GetStatic(ConstantPoolIndex),
    /// Loads a field of an object
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.getfield>
    GetField(ConstantPoolIndex),
    /// Writes a value to a field of an object
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.putfield>
    PutField(ConstantPoolIndex),
    /// Static must be a MethodRef
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.invokestatic>
    InvokeStatic(ConstantPoolIndex),
    /// Index must be a MethodRef
    InvokeVirtual(ConstantPoolIndex),
    /// Similar to [`Instruction::InvokeVirtual`], but only used for `<init>` and `super` calls.
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.invokespecial>
    InvokeSpecial(ConstantPoolIndex),
    /// Loads a constant from the constant pool
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.ldc>
    Ldc(ConstantPoolIndex),
    /// Loads a constant of type Long or Double from the constant pool
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.ldc2_w>
    Ldc2W(ConstantPoolIndex),
    /// Returns void
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.return>
    Return,
    /// Returns a reference type
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.areturn>
    AReturn,
    /// Returns Int, short, char, byte or boolean
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.ireturn>
    IReturn,
    //// Returns Long
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.lreturn>
    LReturn,
    /// Create a new array of references, initialize to nulls
    /// Argument must be a reference to a class, array or function
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.anewarray>
    ANewArray(ConstantPoolIndex),
    /// Creates a new array of the given type, initialized to zeros of that type
    NewArray(PrimitiveType),
    /// Returns the length of an array
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.arraylength>
    ArrayLength,
    /// Returns an object from an array of references
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.aaload>
    AALoad,
    /// Stores an object into an array of references
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.aastore>
    AAStore,
    /// Loads an int from an int array
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.iaload>
    IALoad,
    /// Stores an int into an int array
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.iastore>
    IAStore,
    /// Loads a byte or boolean from a byte or boolean array
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.baload>
    BALoad,
    /// Stores a byte or boolean into a byte or boolean array
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.bastore>
    BAStore,
    /// Loads a long from a long array
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.laload>
    LALoad,
    /// Stores a long into a long array
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.lastore>
    LAStore,
    // Load a reference
    // https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.aload
    ALoad(u16),
    // Store a reference
    // https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.astore
    AStore(u16),
    /// Must be -1 <= value <= 8
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.iconst_i>
    IConst(i8),
    IAdd,
    /// Subtract two integers
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.isub>
    ISub,
    /// Multiplies two integers
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.imul>
    IMul,
    /// Divides two integers
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.idiv>
    IDiv,
    /// Calculates the reminder of the division of two integers
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.irem>
    IRem,
    /// Stores an integer into a local variable
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.istore>
    IStore(u16),
    /// Loads an integer from a local variable
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.iload>
    ILoad(u16),
    /// Converts from Int to Long
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.i2l>
    I2L,
    /// Compares the two topmost stack entries according to the given `comparison`
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.if_icmp_cond>
    IfICmp {
        comparison: Comparison,
        jump_point: JumpPoint,
    },
    /// Compares the topmost stack entry with 0 according to the given `comparison`
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.if_cond>
    IfI {
        comparison: Comparison,
        jump_point: JumpPoint,
    },
    /// Add two Longs
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.ladd>
    LAdd,
    /// Subtract two Longs
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.lsub>
    LSub,
    /// Multiply two Longs
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.lmul>
    LMul,
    /// Divide two Longs
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.ldiv>
    LDiv,
    /// Calculate the reminder of two Longs
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.lrem>
    LRem,
    /// Store a Long in a local variable
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.lstore>
    LStore(u16),
    /// Load a Long from a local variable
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.lload>
    LLoad(u16),
    /// Converts from a Long to an In
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.l2i>
    L2I,
    /// Compares two Long values and pushes the integer -1, 0, or 1 to the stack
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.lcmp>
    Lcmp,
    Goto(JumpPoint),
    Nop,
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.pop>
    Pop,
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.pop2>
    Pop2,
    /// Duplicates the top stack value (of category 1)
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.dup>
    Dup,
    /// Duplicates the top stack value of category 2, or the top 2 stack values of category 1
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.dup2>
    Dup2,
}

impl Instruction {
    pub fn serialize(&self, buf: &mut impl Write, current_index: u16) -> io::Result<()> {
        match *self {
            Instruction::New(reference) => {
                buf.write_all(&[0xbb])?;
                reference.serialize(buf)?;
            }
            Instruction::GetStatic(reference) => {
                buf.write_all(&[0xb2])?;
                reference.serialize(buf)?;
            }
            Instruction::GetField(reference) => {
                buf.write_all(&[0xb4])?;
                reference.serialize(buf)?;
            }
            Instruction::PutField(reference) => {
                buf.write_all(&[0xb5])?;
                reference.serialize(buf)?;
            }
            Instruction::IConst(value) => {
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
            Instruction::InvokeSpecial(method_reference) => {
                buf.write_all(&[0xb7])?;
                method_reference.serialize(buf)?;
            }
            Instruction::Return => {
                buf.write_all(&[0xb1])?;
            }
            Instruction::AReturn => {
                buf.write_all(&[0xb0])?;
            }
            Instruction::IReturn => {
                buf.write_all(&[0xac])?;
            }
            Instruction::LReturn => {
                buf.write_all(&[0xad])?;
            }
            Instruction::ANewArray(index) => {
                buf.write_all(&[0xbd])?;
                index.serialize(buf)?;
            }
            Instruction::NewArray(r#type) => {
                buf.write_all(&[0xbc, r#type as u8])?;
            }
            Instruction::ArrayLength => {
                buf.write_all(&[0xbe])?;
            }
            Instruction::AALoad => buf.write_all(&[0x32])?,
            Instruction::AAStore => buf.write_all(&[0x53])?,
            Instruction::IALoad => buf.write_all(&[0x2e])?,
            Instruction::IAStore => buf.write_all(&[0x4f])?,
            Instruction::BALoad => buf.write_all(&[0x33])?,
            Instruction::BAStore => buf.write_all(&[0x54])?,
            Instruction::LALoad => buf.write_all(&[0x2f])?,
            Instruction::LAStore => buf.write_all(&[0x50])?,
            Instruction::Ldc(index) => {
                let index: u8 = index.0.get().try_into().expect("TODO: implement wide load");
                buf.write_all(&[0x12, index])?;
            }
            Instruction::Ldc2W(index) => {
                buf.write_all(&[0x14])?;
                buf.write_all(&index.0.get().to_be_bytes())?;
            }
            Instruction::IAdd => buf.write_all(&[0x60])?,
            Instruction::ISub => buf.write_all(&[0x64])?,
            Instruction::IMul => buf.write_all(&[0x68])?,
            Instruction::IDiv => buf.write_all(&[0x6c])?,
            Instruction::IRem => buf.write_all(&[0x70])?,
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
            Instruction::I2L => buf.write_all(&[0x85])?,
            Instruction::LAdd => buf.write_all(&[0x61])?,
            Instruction::LSub => buf.write_all(&[0x65])?,
            Instruction::LMul => buf.write_all(&[0x69])?,
            Instruction::LDiv => buf.write_all(&[0x6d])?,
            Instruction::LRem => buf.write_all(&[0x71])?,
            Instruction::LStore(index) => {
                // TODO: implement istore_<n> as space optimization
                let index: u8 = index.try_into().expect("TODO: Implement wide store");
                buf.write_all(&[0x37, index])?;
            }
            Instruction::LLoad(index) => {
                // TODO: implement iload_<n> as space optimization
                let index: u8 = index
                    .try_into()
                    .expect("TODO: Implement support for wide loads");
                buf.write_all(&[0x16, index])?;
            }
            Instruction::L2I => buf.write_all(&[0x88])?,
            Instruction::Lcmp => buf.write_all(&[0x94])?,
            Instruction::ALoad(index) => {
                let index: u8 = index
                    .try_into()
                    .expect("TODO: Implement support for wide loads");
                buf.write_all(&[0x19, index])?;
            }
            Instruction::AStore(index) => {
                let index: u8 = index
                    .try_into()
                    .expect("TODO: Implement support for wide stores");
                buf.write_all(&[0x3a, index])?;
            }
            Instruction::Goto(jump_point) => {
                let offset: i16 = jump_point.0 as i16 - current_index as i16;
                buf.write_all(&[0xa7])?;
                buf.write_all(&offset.to_be_bytes())?;
            }
            Instruction::Nop => buf.write_all(&[0x0])?,
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
            Instruction::Pop => buf.write_all(&[0x57])?,
            Instruction::Pop2 => buf.write_all(&[0x58])?,
            Instruction::Dup => buf.write_all(&[0x59])?,
            Instruction::Dup2 => buf.write_all(&[0x5c])?,
        }

        Ok(())
    }

    /// Returns the number of bytes this instrutions takes if serialized
    pub fn len_bytes(&self) -> u16 {
        /// A writer that voids everything but keeps track of the total number of bytes
        #[derive(Default)]
        struct VoidWriter {
            pub len: usize,
        }

        impl Write for VoidWriter {
            fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
                self.len += buf.len();
                Ok(buf.len())
            }

            fn flush(&mut self) -> std::io::Result<()> {
                Ok(())
            }
        }

        let mut writer = VoidWriter::default();
        self.serialize(&mut writer, 0).unwrap();
        writer.len.try_into().unwrap()
    }

    pub fn display<'a>(&'a self, pool: &'a ConstantPool) -> impl Display {
        InstructionDisplay {
            instruction: self,
            pool,
        }
    }
}

#[derive(Debug)]
pub struct CodeAttribute {
    pub max_locals: u16,
    pub max_stack: u16,
    pub code: Vec<Instruction>,
    pub attributes: Vec<AttributeInfo>,
}

impl CodeAttribute {
    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        buf.write_all(&self.max_stack.to_be_bytes())?;
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

/// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-2.html#jvms-2.11.1-320>
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

/// Used as argument ot the [`Instruction::NewArray`]
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum PrimitiveType {
    Boolean = 4,
    Char,
    Float,
    Double,
    Byte,
    Short,
    Int,
    Long,
}

impl PrimitiveType {
    pub fn to_verification_type(self) -> VerificationTypeInfo {
        use PrimitiveType::*;
        match self {
            Boolean | Char | Short | Byte | Int => VerificationTypeInfo::Integer,
            Float => VerificationTypeInfo::Float,
            Double => VerificationTypeInfo::Double,
            Long => VerificationTypeInfo::Long,
        }
    }
}

/// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.7.4>
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

    pub fn display<'a>(&'a self, pool: &'a ConstantPool) -> impl Display + use<'a> {
        VerificationTypeInfoDisplay { info: self, pool }
    }
}

/// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.7.4>
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

/// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.7.4>
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

bitflags! {
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.5-200-A.1>
    pub struct FieldAccessFlags: u16 {
        pub const PUBLIC    = 0x0001;
        pub const PRIVATE   = 0x0002;
        pub const PROTECTED = 0x0004;
        pub const STATIC    = 0x0008;
        pub const FINAL     = 0x0010;
        pub const VOLATILE  = 0x0040;
        pub const SYNTHETIC = 0x1000;
        pub const ENUM      = 0x4000;
    }
}

/// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.5>
#[derive(Debug)]
pub struct Field {
    pub flags: FieldAccessFlags,
    /// points to utf8 of the class name
    pub name_index: ConstantPoolIndex,
    /// points to utf8 of the descriptor
    pub descriptor_index: ConstantPoolIndex,
    pub attributes: Vec<AttributeInfo>,
}

impl Field {
    pub fn display<'a>(&'a self, pool: &'a ConstantPool) -> FieldDisplay<'a> {
        FieldDisplay { field: self, pool }
    }

    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        buf.write_all(&self.flags.get().to_be_bytes())?;
        self.name_index.serialize(buf)?;
        self.descriptor_index.serialize(buf)?;
        let attribute_count: u16 = self.attributes.len().try_into().unwrap();
        buf.write_all(&attribute_count.to_be_bytes())?;
        for attribute in &self.attributes {
            attribute.serialize(buf)?;
        }
        Ok(())
    }
}

bitflags! {
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.6-200-A.1>
    pub struct MethodAccessFlags: u16 {
        pub const PUBLIC       = 0x0001;
        pub const PRIVATE      = 0x0002;
        pub const PROTECTED    = 0x0004;
        pub const STATIC       = 0x0008;
        pub const FINAL        = 0x0010;
        pub const SYNCHRONIZED = 0x0020;
        pub const BRIDGE       = 0x0040;
        pub const VARARGS      = 0x0080;
        pub const NATIVE       = 0x0100;
        pub const ABSTRACT     = 0x0400;
        pub const SYNTHETIC    = 0x1000;
    }
}

#[derive(Debug)]
pub struct Method {
    pub flags: MethodAccessFlags,
    /// Index into the constant pool that must reference a Utf8 entry
    pub name_index: ConstantPoolIndex,
    /// Index into the constant pool that must reference a Utf8 entry
    pub descriptor_index: ConstantPoolIndex,
    pub attributes: Vec<AttributeInfo>,
}

impl Method {
    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        buf.write_all(&self.flags.get().to_be_bytes())?;
        self.name_index.serialize(buf)?;
        self.descriptor_index.serialize(buf)?;

        buf.write_all(&(self.attributes.len() as u16).to_be_bytes())?;
        for attribute in &self.attributes {
            attribute.serialize(buf)?;
        }

        Ok(())
    }

    pub fn display<'a>(&'a self, pool: &'a ConstantPool) -> MethodDisplay<'a> {
        MethodDisplay { method: self, pool }
    }
}

#[repr(u16)]
pub enum ClassAccessFlag {
    Public = 0x0001,
    Final = 0x0010,
}

pub struct ClassData {
    pub constant_pool: ConstantPool,
    // Index into the constant pool that must reference a class entry
    pub this_class: ConstantPoolIndex,
    // Index into the constant pool that must reference a class entry
    pub super_class: ConstantPoolIndex,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}

pub const MAGIC: [u8; 4] = 0xCAFEBABE_u32.to_be_bytes();
pub const JVM_VERSION: JVMVersion = JVMVersion {
    major: 65,
    minor: 0,
};
pub const ACCESSS_FLAG: u16 = (ClassAccessFlag::Public as u16) | (ClassAccessFlag::Final as u16);

impl ClassData {
    pub fn new(
        class_name: SmallString,
        mut builder: ConstantPoolBuilder,
        fields: Vec<Field>,
        methods: Vec<Method>,
    ) -> Self {
        let this_class = builder.add_class(class_name);
        let super_class = builder.add_class("java/lang/Object".into());
        let constant_pool = builder.build();
        Self {
            constant_pool,
            this_class,
            super_class,
            fields,
            methods,
        }
    }

    pub fn serialize(&self, buf: &mut impl Write) -> io::Result<()> {
        buf.write_all(&MAGIC)?;

        buf.write_all(&JVM_VERSION.minor.to_be_bytes())?;
        buf.write_all(&JVM_VERSION.major.to_be_bytes())?;

        self.constant_pool.serialize(buf)?;

        buf.write_all(&ACCESSS_FLAG.to_be_bytes())?;

        self.this_class.serialize(buf)?;
        self.super_class.serialize(buf)?;

        // Interfaces count & interfaces
        buf.write_all(&[0, 0])?;

        let field_count: u16 = self.fields.len().try_into().unwrap();
        buf.write_all(&field_count.to_be_bytes())?;
        for field in &self.fields {
            field.serialize(buf)?;
        }

        buf.write_all(&(self.methods.len() as u16).to_be_bytes())?;
        for method in &self.methods {
            method.serialize(buf)?;
        }

        // Attributes count & attributes
        buf.write_all(&[0, 0])?;

        Ok(())
    }
}

impl Debug for ClassData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ClassData {
            constant_pool,
            this_class,
            super_class,
            fields,
            methods,
        } = self;

        writeln!(f, "ClassData")?;
        writeln!(
            f,
            "  this_class:\t\t{this_class}\t{}",
            constant_pool[*this_class].display(constant_pool)
        )?;
        writeln!(
            f,
            "  super_class:\t\t{super_class}\t{}",
            constant_pool[*super_class].display(constant_pool)
        )?;

        let pool_display = ConstantPoolDisplay {
            pool: constant_pool,
        };
        writeln!(f, "{pool_display}")?;

        writeln!(f, "{{")?;
        for field in fields {
            writeln!(f, "{}", field.display(constant_pool))?;
        }
        for method in methods {
            writeln!(f, "{}", method.display(constant_pool))?;
        }
        writeln!(f, "}}")?;

        Ok(())
    }
}
