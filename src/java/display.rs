use std::fmt::Display;

use crate::java::class::{
    Attribute, CodeAttribute, ConstantPool, ConstantPoolEntry, ConstantPoolIndex, Field,
    Instruction, Method, StackMapFrame, StackMapTableAttribute, VerificationTypeInfo,
};

impl Display for ConstantPoolIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0.get())
    }
}

pub struct ConstantPoolDisplay<'a> {
    pub(super) pool: &'a ConstantPool,
}

impl Display for ConstantPoolDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ConstantPoolEntry::*;

        writeln!(f, "Constant pool:")?;
        let padding = self.pool.entries.as_ref().len().ilog10() as usize + 1;
        for (index, entry) in self.pool.entries.iter().enumerate() {
            let Some(entry) = entry else {
                continue;
            };
            write!(f, "  #{index:0>padding$} = ")?;

            match entry {
                Class { name_index } => write!(f, "Class\t\t{name_index}"),
                FieldRef {
                    class_index,
                    name_and_type_index,
                } => write!(f, "FieldRef\t{class_index}.{name_and_type_index}"),
                MethodRef {
                    class_index,
                    name_and_type_index,
                } => write!(f, "MethodRef\t{class_index}.{name_and_type_index}"),
                NameAndType {
                    name_index,
                    type_index,
                } => write!(f, "NameAndType\t{name_index}:{type_index}"),
                Utf8(_) => write!(f, "Utf8\t\t"),
                String { string_index } => write!(f, "String\t\t{string_index}"),
                Integer { integer: _ } => write!(f, "Integer\t\t"),
                Long { long: _ } => write!(f, "Long\t\t"),
            }?;

            writeln!(f, "\t{}", entry.display(self.pool))?;
        }

        Ok(())
    }
}

pub struct ConstantPoolEntryDisplay<'a> {
    pub(super) entry: &'a ConstantPoolEntry,
    pub(super) pool: &'a ConstantPool,
}

impl Display for ConstantPoolEntryDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ConstantPoolEntry::*;

        let load = |index: ConstantPoolIndex| self.pool[index].display(self.pool);

        match self.entry {
            Class { name_index } => {
                write!(f, "{}", load(*name_index))
            }
            FieldRef {
                class_index,
                name_and_type_index,
            } => write!(f, "{}.{}", load(*class_index), load(*name_and_type_index)),
            MethodRef {
                class_index,
                name_and_type_index,
            } => write!(f, "{}:{}", load(*class_index), load(*name_and_type_index)),
            NameAndType {
                name_index,
                type_index,
            } => write!(f, "{}:{}", load(*name_index), load(*type_index)),
            Utf8(small_string) => write!(f, "{}", small_string),
            String { string_index } => write!(f, "{}", load(*string_index)),
            Integer { integer } => write!(f, "{integer}"),
            Long { long } => write!(f, "{long}"),
        }
    }
}

pub struct FieldDisplay<'a> {
    pub(super) field: &'a Field,
    pub(super) pool: &'a ConstantPool,
}

impl Display for FieldDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let load = |index: ConstantPoolIndex| self.pool[index].display(self.pool);

        let Field {
            flags,
            name_index,
            descriptor_index,
            attributes,
        } = &self.field;

        write!(f, "  ")?;
        for flag in flags.active_variants() {
            write!(f, "{} ", flag.name)?;
        }
        writeln!(f, "{}: {}", load(*name_index), load(*descriptor_index))?;

        for attribute in attributes {
            writeln!(
                f,
                "{}",
                AttributeDisplay {
                    attribute: &attribute.attribute,
                    pool: self.pool,
                    indent: 4
                }
            )?;
        }

        Ok(())
    }
}

pub struct MethodDisplay<'a> {
    pub(super) method: &'a Method,
    pub(super) pool: &'a ConstantPool,
}

impl Display for MethodDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let load = |index: ConstantPoolIndex| self.pool[index].display(self.pool);

        let Method {
            flags,
            name_index,
            descriptor_index,
            attributes,
        } = &self.method;
        writeln!(f, "  fn {} {{", load(*name_index))?;

        writeln!(f, "    descriptor: {}", load(*descriptor_index))?;
        writeln!(f, "    flags: {flags:?}")?;

        for attribute_info in attributes {
            let attribute = &attribute_info.attribute;
            AttributeDisplay {
                attribute,
                pool: self.pool,
                indent: 4,
            }
            .fmt(f)?;
        }

        writeln!(f, "  }}")?;

        Ok(())
    }
}

pub struct AttributeDisplay<'a> {
    attribute: &'a Attribute,
    pool: &'a ConstantPool,
    indent: usize,
}

impl Display for AttributeDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.attribute {
            Attribute::Code(code) => CodeAttributeDisplay {
                code,
                pool: self.pool,
                indent: self.indent,
            }
            .fmt(f),
            Attribute::StackMapTable(stack_map_table) => StackMapTableAttributeDisplay {
                stack_map_table,
                pool: self.pool,
                indent: self.indent,
            }
            .fmt(f),
        }
    }
}

pub struct CodeAttributeDisplay<'a> {
    code: &'a CodeAttribute,
    pool: &'a ConstantPool,
    indent: usize,
}

impl Display for CodeAttributeDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let CodeAttribute {
            max_stack,
            max_locals,
            code,
            attributes,
        } = &self.code;

        print_indent(f, self.indent)?;
        writeln!(f, "Code:")?;

        let indent = self.indent + 2;

        print_indent(f, indent)?;
        writeln!(f, "stack = {max_stack}, locals = {max_locals}")?;

        let max_indent = (code.iter().map(|it| it.len_bytes()).sum::<u16>().ilog10() + 1) as usize;
        let mut offset = 0;
        for instruction in code {
            print_indent(f, indent)?;
            writeln!(
                f,
                "{offset:>max_indent$}: {}",
                instruction.display(self.pool)
            )?;
            offset += instruction.len_bytes();
        }

        for attribute in attributes {
            write!(
                f,
                "{}",
                AttributeDisplay {
                    attribute: &attribute.attribute,
                    pool: self.pool,
                    indent
                }
            )?;
        }

        Ok(())
    }
}

pub struct InstructionDisplay<'a> {
    pub(super) instruction: &'a Instruction,
    pub(super) pool: &'a ConstantPool,
}

impl Display for InstructionDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;

        let load = |index: &ConstantPoolIndex| self.pool[*index].display(self.pool);

        match &self.instruction {
            GetStatic(constant_pool_index) => write!(
                f,
                "GetStatic\t\t{constant_pool_index}\t{}",
                load(constant_pool_index)
            ),
            InvokeStatic(constant_pool_index) => write!(
                f,
                "InvokeStatic\t\t{constant_pool_index}\t{}",
                load(constant_pool_index)
            ),
            InvokeVirtual(constant_pool_index) => write!(
                f,
                "InvokeVirtual\t\t{constant_pool_index}\t{}",
                load(constant_pool_index)
            ),
            Ldc(constant_pool_index) => write!(
                f,
                "LoadConstant\t\t{constant_pool_index}\t{}",
                load(constant_pool_index)
            ),
            Return => write!(f, "Return"),
            AReturn => write!(f, "AReturn"),
            IReturn => write!(f, "IReturn"),
            ANewArray(constant_pool_index) => write!(
                f,
                "ANewArray\t\t{constant_pool_index}\t{}",
                load(constant_pool_index)
            ),
            NewArray(primitive_type) => write!(f, "NewArray\t\t{primitive_type:?}"),
            ArrayLength => write!(f, "ArrayLength"),
            AALoad => write!(f, "AALoad"),
            AAStore => write!(f, "AAStore"),
            IALoad => write!(f, "IALoad"),
            IAStore => write!(f, "IAStore"),
            BALoad => write!(f, "BALoad"),
            BAStore => write!(f, "BAStore"),
            ALoad(index) => write!(f, "ALoad\t\t\t{index}"),
            AStore(index) => write!(f, "AStore\t\t\t{index}"),
            IConst(value) => write!(f, "IConst\t\t\t{value}"),
            IAdd => write!(f, "IAdd"),
            ISub => write!(f, "ISub"),
            IMul => write!(f, "IMul"),
            IDiv => write!(f, "IDiv"),
            IRem => write!(f, "IRem"),
            IStore(index) => write!(f, "IStore\t\t\t{index}"),
            ILoad(index) => write!(f, "ILoad\t\t\t{index}"),
            I2L => write!(f, "I2L"),
            IfICmp {
                comparison,
                jump_point,
            } => write!(f, "IfICmp {}\t\t{}", comparison.as_str(), jump_point.0),
            IfI {
                comparison,
                jump_point,
            } => write!(f, "IfI {}\t\t{}", comparison.as_str(), jump_point.0),
            Goto(jump_point) => write!(f, "Goto\t\t\t\t{}", jump_point.0),
            Nop => write!(f, "Nop"),
            Pop => write!(f, "Pop"),
            Pop2 => write!(f, "Pop2"),
            Dup => write!(f, "Dup"),
            Dup2 => write!(f, "Dup2"),
            Ldc2W(constant_pool_index) => write!(
                f,
                "Ldc2W\t\t\t{constant_pool_index}\t{}",
                load(constant_pool_index)
            ),
            LReturn => write!(f, "LReturn"),
            LALoad => write!(f, "LALoad"),
            LAStore => write!(f, "LAStore"),
            LAdd => write!(f, "LAdd"),
            LSub => write!(f, "LSub"),
            LMul => write!(f, "LMul"),
            LDiv => write!(f, "LDiv"),
            LRem => write!(f, "LRem"),
            LStore(index) => write!(f, "LStore\t\t{index}"),
            LLoad(index) => write!(f, "LLoad\t\t\t{index}"),
            Lcmp => write!(f, "Lcmp"),
            L2I => write!(f, "L2I"),
            New(constant_pool_index) => write!(
                f,
                "New\t\t\t{constant_pool_index}\t{}",
                load(constant_pool_index)
            ),
            InvokeSpecial(constant_pool_index) => write!(
                f,
                "InvokeSpecial\t\t{constant_pool_index}\t{}",
                load(constant_pool_index)
            ),
            GetField(constant_pool_index) => write!(
                f,
                "GetField\t\t{constant_pool_index}\t{}",
                load(constant_pool_index)
            ),
            PutField(constant_pool_index) => write!(
                f,
                "PutField\t\t{constant_pool_index}\t{}",
                load(constant_pool_index)
            ),
        }
    }
}

struct StackMapTableAttributeDisplay<'a> {
    stack_map_table: &'a StackMapTableAttribute,
    pool: &'a ConstantPool,
    indent: usize,
}

impl Display for StackMapTableAttributeDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        print_indent(f, self.indent)?;
        writeln!(f, "StackMapTable:")?;

        let indent = self.indent + 2;
        for frame in &self.stack_map_table.entries {
            let (frame_type, offset, locals, stack) = match frame {
                StackMapFrame::Same { offset_delta } => ("Same", *offset_delta as u16, None, None),
                StackMapFrame::Full {
                    offset_delta,
                    locals,
                    stack,
                } => ("Full", *offset_delta, Some(locals), Some(stack)),
            };

            print_indent(f, indent)?;
            writeln!(f, "{frame_type}")?;

            let indent = indent + 2;

            print_indent(f, indent)?;
            writeln!(f, "offset_delta = {offset}")?;

            if let Some(locals) = locals {
                print_indent(f, indent)?;
                write!(f, "locals = [ ")?;
                for (index, local) in locals.iter().enumerate() {
                    write!(f, "{}", local.display(self.pool))?;
                    if index + 1 < locals.len() {
                        write!(f, ",")?;
                    }
                }
                writeln!(f, " ]")?;
            }

            if let Some(stack) = stack {
                print_indent(f, indent)?;
                write!(f, "Stack = [ ")?;
                for (index, entry) in stack.iter().enumerate() {
                    write!(f, "{}", entry.display(self.pool))?;
                    if index + 1 < stack.len() {
                        write!(f, ",")?;
                    }
                }
                writeln!(f, " ]")?;
            }
        }

        Ok(())
    }
}

pub(crate) struct VerificationTypeInfoDisplay<'a> {
    pub info: &'a VerificationTypeInfo,
    pub pool: &'a ConstantPool,
}

impl Display for VerificationTypeInfoDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use VerificationTypeInfo::*;

        match &self.info {
            Top => write!(f, "Top"),
            Integer => write!(f, "Integer"),
            Float => write!(f, "Float"),
            Null => write!(f, "Null"),
            UninitializedThis => write!(f, "UninitializedThis"),
            Object {
                constant_pool_index,
            } => write!(
                f,
                "Object '{}'",
                self.pool[*constant_pool_index].display(self.pool)
            ),
            UninitializedVariable { offset } => write!(f, "UninitializedVariable {offset}"),
            Long => write!(f, "Long"),
            Double => write!(f, "Double"),
        }
    }
}

fn print_indent(f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
    for _ in 0..indent {
        write!(f, " ")?;
    }

    Ok(())
}
