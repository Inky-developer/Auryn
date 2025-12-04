use std::{
    fs::OpenOptions,
    io::{self, Write},
};

use auryn::java::assembler::{
    Assembler, ConstantValue, FieldDescriptor, Instruction, MethodDescriptor,
};

fn main() -> io::Result<()> {
    let mut assembler = Assembler::new("Helloworld".to_string());
    assembler.add_all([
        Instruction::GetStatic {
            class_name: "java/lang/System".to_string(),
            name: "out".to_string(),
            field_type: FieldDescriptor("Ljava/io/PrintStream;".to_string()),
        },
        Instruction::LoadConstant {
            value: ConstantValue::String("Hello World!".to_string()),
        },
        Instruction::InvokeVirtual {
            class_name: "java/io/PrintStream".to_string(),
            name: "println".to_string(),
            method_type: MethodDescriptor("(Ljava/lang/String;)V".to_string()),
        },
        Instruction::ReturnNull,
    ]);

    let class_data = assembler.assemble();

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
