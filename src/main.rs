use std::{
    fs::OpenOptions,
    io::{self, Write},
};

use auryn::java::class::{
    Attribute, AttributeInfo, ClassData, CodeAttribute, ConstantPoolEntry, ConstantPoolIndex,
    Instruction, Method, MethodAccessFlag,
};

fn main() -> io::Result<()> {
    let constant_pool = vec![
        ConstantPoolEntry::Utf8("Helloworld".to_string()),
        ConstantPoolEntry::Class {
            name_index: ConstantPoolIndex::new(1),
        },
        ConstantPoolEntry::Utf8("java/lang/Object".to_string()),
        ConstantPoolEntry::Class {
            name_index: ConstantPoolIndex::new(3),
        },
        ConstantPoolEntry::Utf8("main".to_string()),
        ConstantPoolEntry::Utf8("([Ljava/lang/String;)V".to_string()),
        ConstantPoolEntry::Utf8("Code".to_string()),
        ConstantPoolEntry::Utf8("Hello World!".to_string()),
        ConstantPoolEntry::String {
            string_index: ConstantPoolIndex::new(8),
        },
        ConstantPoolEntry::Utf8("java/lang/System".to_string()),
        ConstantPoolEntry::Class {
            name_index: ConstantPoolIndex::new(10),
        },
        ConstantPoolEntry::Utf8("out".to_string()),
        ConstantPoolEntry::Utf8("Ljava/io/PrintStream;".to_string()),
        ConstantPoolEntry::NameAndType {
            name_index: ConstantPoolIndex::new(12),
            type_index: ConstantPoolIndex::new(13),
        },
        ConstantPoolEntry::FieldRef {
            class_index: ConstantPoolIndex::new(11),
            name_and_type_index: ConstantPoolIndex::new(14),
        },
        ConstantPoolEntry::Utf8("java/io/PrintStream".to_string()),
        ConstantPoolEntry::Class {
            name_index: ConstantPoolIndex::new(16),
        },
        ConstantPoolEntry::Utf8("println".to_string()),
        ConstantPoolEntry::Utf8("(Ljava/lang/String;)V".to_string()),
        ConstantPoolEntry::NameAndType {
            name_index: ConstantPoolIndex::new(18),
            type_index: ConstantPoolIndex::new(19),
        },
        ConstantPoolEntry::MethodRef {
            class_index: ConstantPoolIndex::new(17),
            name_and_type_index: ConstantPoolIndex::new(20),
        },
    ];
    let methods = vec![Method {
        flags: (MethodAccessFlag::Public as u16) | (MethodAccessFlag::Static as u16),
        name_index: ConstantPoolIndex::new(5),
        descriptor_index: ConstantPoolIndex::new(6),
        attributes: vec![AttributeInfo {
            name_index: ConstantPoolIndex::new(7),
            attribute: Attribute::Code(CodeAttribute {
                max_locals: 1,
                code: vec![
                    Instruction::GetStatic(ConstantPoolIndex::new(15)),
                    Instruction::Ldc(9),
                    Instruction::InvokeVirtual(ConstantPoolIndex::new(21)),
                    Instruction::Return,
                ],
            }),
        }],
    }];
    let class_data = ClassData {
        constant_pool,
        this_class: ConstantPoolIndex::new(2),
        super_class: ConstantPoolIndex::new(4),
        methods,
    };

    let mut output = Vec::new();
    class_data.serialize(&mut output)?;
    println!("Result: {output:?}");

    let mut f = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open("Helloworld.class")?;
    f.write_all(&output)?;

    Ok(())
}
