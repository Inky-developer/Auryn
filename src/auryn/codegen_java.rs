use crate::{
    auryn::{
        ast::ast_parser::{
            Assignment, AstError, BinaryOperation, Block, Expression, NodeOrError, Root, Statement,
            Value, VariableUpdate,
        },
        tokenizer::BinaryOperatorToken,
    },
    java::{
        assembler::{
            Assembler, ConstantValue, FieldDescriptor, Instruction, InstructionId,
            MethodDescriptor, VariableId, primitive,
        },
        class::ClassData,
    },
    utils::fast_map::FastMap,
};

#[derive(Debug)]
pub enum CodegenError {
    InvalidAst(AstError),
}

impl From<AstError> for CodegenError {
    fn from(error: AstError) -> Self {
        CodegenError::InvalidAst(error)
    }
}

impl From<&AstError> for CodegenError {
    fn from(value: &AstError) -> Self {
        CodegenError::InvalidAst(value.clone())
    }
}

pub type CodegenResult<T = ()> = Result<T, CodegenError>;

pub fn query_class(class_name: String, ast: &NodeOrError<Root>) -> Result<ClassData, CodegenError> {
    let mut generator = Generator::new();
    generator.generate_from_ast(ast)?;

    Ok(generator.finish(class_name))
}

struct Generator {
    assembler: Assembler,
    variable_map: FastMap<String, VariableId<primitive::Integer>>,
}

impl Generator {
    pub fn new() -> Self {
        Self {
            assembler: Assembler::new(),
            variable_map: FastMap::default(),
        }
    }

    pub fn finish(self, class_name: String) -> ClassData {
        self.assembler.assemble(class_name)
    }

    pub fn generate_from_ast(&mut self, root: &NodeOrError<Root>) -> CodegenResult {
        let root = root.as_ref().map_err(Clone::clone)?;
        self.generate_root(&root.kind)?;
        self.assembler.add(Instruction::Goto(InstructionId(0)));
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
                field_type: FieldDescriptor::Object("java/io/PrintStream".to_string()),
            },
            Instruction::ILoad(result_id),
            Instruction::InvokeVirtual {
                class_name: "java/io/PrintStream".to_string(),
                name: "println".to_string(),
                // method_type: MethodDescriptor("(I)V".to_string()),
                method_type: MethodDescriptor {
                    arguments: vec![FieldDescriptor::Integer],
                    return_type: FieldDescriptor::Void,
                },
            },
        ]);
    }
}

impl Generator {
    fn generate_root(&mut self, root: &Root) -> CodegenResult {
        self.generate_block(root.block()?)?;
        Ok(())
    }

    fn generate_block(&mut self, block: &Block) -> CodegenResult {
        for statement in &block.statements {
            let statement = statement.as_ref()?;
            self.generate_statement(&statement.kind)?;
        }

        Ok(())
    }

    fn generate_statement(&mut self, statement: &Statement) -> CodegenResult {
        match statement {
            Statement::Assignement(assignment) => {
                let assignment = assignment.as_ref().as_ref()?;
                self.generate_assignment(&assignment.kind)
            }
            Statement::VariableUpdate(update) => {
                let update = update.as_ref().as_ref()?;
                self.generate_update(&update.kind)
            }
            Statement::Expression(expression) => {
                let expression = expression.as_ref().as_ref()?;
                self.generate_expression(&expression.kind)
            }
        }
    }

    fn generate_assignment(&mut self, assignment: &Assignment) -> CodegenResult {
        self.generate_expression(assignment.expression()?)?;

        assert!(
            !self.variable_map.contains_key(&assignment.ident),
            "Trying to redefine a variable"
        );
        let variable_id = self.assembler.alloc_variable();
        self.variable_map
            .insert(assignment.ident.clone(), variable_id);

        self.assembler.add(Instruction::IStore(variable_id));

        Ok(())
    }

    fn generate_update(&mut self, update: &VariableUpdate) -> CodegenResult {
        self.generate_expression(update.expression()?)?;

        let variable_id = self.variable_map[&update.ident];
        self.assembler.add(Instruction::IStore(variable_id));

        Ok(())
    }

    fn generate_expression(&mut self, expression: &Expression) -> CodegenResult {
        match expression {
            Expression::Value(value) => {
                let value = value.as_ref().as_ref()?;
                self.generate_value(&value.kind)?;
            }
            Expression::BinaryOperation(operation) => {
                let operation = operation.as_ref().as_ref()?;
                self.generate_binary_operation(&operation.kind)?;
            }
        }

        Ok(())
    }

    fn generate_binary_operation(&mut self, operation: &BinaryOperation) -> CodegenResult {
        self.generate_expression(operation.lhs()?)?;
        self.generate_expression(operation.rhs()?)?;

        match operation.operator()? {
            BinaryOperatorToken::Plus => self.assembler.add(Instruction::IAdd),
            BinaryOperatorToken::Times => self.assembler.add(Instruction::IMul),
        };

        Ok(())
    }

    fn generate_value(&mut self, value: &Value) -> CodegenResult {
        match value {
            Value::Number(number) => {
                let number = number.as_ref().as_ref()?;
                self.assembler.add(Instruction::LoadConstant {
                    value: ConstantValue::Integer(number.kind.value),
                });
            }
            Value::Ident(ident) => {
                let ident = ident.as_ref().as_ref()?;
                let local_variable_id = *self
                    .variable_map
                    .get(&ident.kind.ident)
                    .expect("Trying to access variable which was not defined");
                self.assembler.add(Instruction::ILoad(local_variable_id));
            }
            Value::Parenthesis(parenthesis) => {
                let parenthesis = parenthesis.as_ref().as_ref()?;
                let expression = parenthesis.kind.expression.as_ref().as_ref()?;
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

    #[test]
    fn test_assignment() {
        insta::assert_debug_snapshot!(generate_class("let a = 1"));
        insta::assert_debug_snapshot!(generate_class("let a = 1\nprint(a)"));
        insta::assert_debug_snapshot!(generate_class("let a = 7\na = a * a\nprint(a)"));
    }

    #[test]
    #[should_panic]
    fn test_reject_invalid_variable() {
        insta::assert_debug_snapshot!(generate_class("let a = a"));
    }
}
