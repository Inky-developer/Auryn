use crate::{
    auryn::{
        ast::{AstError, Expression, NodeOrError, Root, Value},
        tokenizer::BinaryOperatorToken,
    },
    java::{
        assembler::{Assembler, ConstantValue, Instruction, MethodDescriptor},
        class::ClassData,
    },
};

#[derive(Debug)]
pub enum CodegenError {
    InvalidAst(AstError),
}

impl From<&AstError> for CodegenError {
    fn from(error: &AstError) -> Self {
        CodegenError::InvalidAst(*error)
    }
}

pub type CodegenResult<T = ()> = Result<T, CodegenError>;

pub fn query_class(class_name: String, ast: &NodeOrError<Root>) -> Result<ClassData, CodegenError> {
    let mut generator = Generator::new(class_name);
    generator.generate_from_ast(ast)?;

    Ok(generator.finish())
}

struct Generator {
    assembler: Assembler,
}

impl Generator {
    pub fn new(class_name: String) -> Self {
        Self {
            assembler: Assembler::new(class_name),
        }
    }

    pub fn finish(self) -> ClassData {
        self.assembler.assemble()
    }

    pub fn generate_from_ast(&mut self, root: &NodeOrError<Root>) -> CodegenResult {
        let root = root.as_ref()?;
        self.generate_root(&root.kind)?;
        self.assembler.add(Instruction::ReturnNull);
        Ok(())
    }
}

impl Generator {
    fn instrics_print_int(&mut self) {
        let result_id = self.assembler.alloc_variable();
        self.assembler.add_all([
            Instruction::IStore(result_id),
            Instruction::GetStatic {
                class_name: "java/lang/System".to_string(),
                name: "out".to_string(),
                field_type: crate::java::assembler::FieldDescriptor(
                    "Ljava/io/PrintStream;".to_string(),
                ),
            },
            Instruction::ILoad(result_id),
            Instruction::InvokeVirtual {
                class_name: "java/io/PrintStream".to_string(),
                name: "println".to_string(),
                method_type: MethodDescriptor("(I)V".to_string()),
            },
        ]);
    }
}

impl Generator {
    fn generate_root(&mut self, root: &Root) -> CodegenResult {
        let expression = root.expression.as_ref().as_ref()?;
        self.generate_expression(&expression.kind)?;
        Ok(())
    }

    fn generate_expression(&mut self, expression: &Expression) -> CodegenResult {
        match expression {
            Expression::Value(value) => {
                let value = value.as_ref().as_ref()?;
                self.generate_value(&value.kind)?;
            }
            Expression::BinaryOperation { lhs, operator, rhs } => {
                let lhs = lhs.as_ref().as_ref()?;
                let op = operator.as_ref().as_ref()?;
                let rhs = rhs.as_ref().as_ref()?;
                self.generate_binary_operation(&lhs.kind, op.kind, &rhs.kind)?;
            }
        }

        Ok(())
    }

    fn generate_value(&mut self, value: &Value) -> CodegenResult {
        match value {
            Value::Int(number) => {
                self.assembler.add(Instruction::LoadConstant {
                    value: ConstantValue::Integer(*number),
                });
            }
            Value::Expression(expression) => {
                let expression = expression.as_ref().as_ref()?;
                self.generate_expression(&expression.kind)?;
            }
            Value::FunctionCall(call) => {
                let call = call.as_ref().as_ref()?;
                assert_eq!(call.kind.ident, "print", "Unexpected function");
                let [expression] = call.kind.arguments.as_slice() else {
                    panic!("print only supports one parameter");
                };
                let expression = expression.as_ref()?;
                self.generate_expression(&expression.kind)?;
                self.instrics_print_int();
            }
        }

        Ok(())
    }

    fn generate_binary_operation(
        &mut self,
        lhs: &Expression,
        op: BinaryOperatorToken,
        rhs: &Expression,
    ) -> CodegenResult {
        self.generate_expression(lhs)?;
        self.generate_expression(rhs)?;

        match op {
            BinaryOperatorToken::Plus => self.assembler.add(Instruction::IAdd),
            BinaryOperatorToken::Times => self.assembler.add(Instruction::IMul),
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        auryn::{ast::query_ast, codegen_java::query_class, parser::Parser},
        java::class::ClassData,
    };

    fn generate_class(input: &str) -> ClassData {
        let result = Parser::new(input).parse();
        let ast = query_ast(result.syntax_tree.as_ref().unwrap());
        let class = query_class("Helloworld".to_string(), &ast).unwrap();
        class
    }

    #[test]
    fn test_simple() {
        insta::assert_debug_snapshot!(generate_class("1 + 2 * 3"));
    }

    #[test]
    fn test_print() {
        insta::assert_debug_snapshot!(generate_class("print(2 * 3)"));
    }
}
