use crate::{
    auryn::{
        air::{
            data::{Air, AirFunction, AirFunctionId},
            types::{FunctionType, Type},
        },
        codegen_java::{function_generator::generate_function, utils::translate_type},
    },
    java::{
        class::{self},
        constant_pool_builder::ConstantPoolBuilder,
        function_assembler::{FieldDescriptor, FunctionAssembler, Instruction, MethodDescriptor},
    },
    utils::{fast_map::FastMap, small_string::SmallString},
};

pub fn generate_class(air: &Air) -> class::ClassData {
    let mut generator = ClassGenerator::new("Main".into());
    generator.generate_from_air(air);
    generator.build()
}

#[derive(Debug, Clone)]
pub struct GeneratedMethodData {
    pub generated_name: SmallString,
    pub method_descriptor: MethodDescriptor,
}

pub struct ClassGenerator {
    class_name: SmallString,
    methods: Vec<class::Method>,
    constant_pool: ConstantPoolBuilder,
    generated_methods: FastMap<AirFunctionId, GeneratedMethodData>,
}

impl ClassGenerator {
    fn new(class_name: SmallString) -> Self {
        Self {
            class_name,
            methods: Vec::new(),
            constant_pool: ConstantPoolBuilder::default(),
            generated_methods: FastMap::default(),
        }
    }

    fn build(self) -> class::ClassData {
        class::ClassData::new(self.class_name, self.constant_pool, self.methods)
    }
}

impl ClassGenerator {
    fn generate_from_air(&mut self, air: &Air) {
        for (function_id, function) in &air.functions {
            let Type::Function(function_type) = function.r#type.computed() else {
                unreachable!("Function should have a function type");
            };
            let method_descriptor = translate_function_type(&mut self.constant_pool, function_type);

            let mangled_name = function.ident.clone();

            self.generated_methods.insert(
                *function_id,
                GeneratedMethodData {
                    generated_name: mangled_name,
                    method_descriptor,
                },
            );
        }

        for (function_id, function) in &air.functions {
            self.generate_function(*function_id, function);
        }

        let main_function_id = air.main_function().0;
        self.generate_main_function(main_function_id);
    }

    fn generate_function(&mut self, id: AirFunctionId, function: &AirFunction) {
        let data = &self.generated_methods[&id];
        let method = generate_function(
            &mut self.constant_pool,
            &self.generated_methods,
            &self.class_name,
            function,
            data.generated_name.clone(),
            data.method_descriptor.clone(),
        );
        self.methods.push(method);
    }

    fn generate_main_function(&mut self, id: AirFunctionId) {
        let main_descriptor = MethodDescriptor {
            parameters: vec![FieldDescriptor::Array {
                dimension_count: 1,
                descriptor: Box::new(FieldDescriptor::string()),
            }],
            return_type: FieldDescriptor::Void,
        };
        let mut assembler =
            FunctionAssembler::new("main".into(), main_descriptor, &mut self.constant_pool);

        let GeneratedMethodData {
            generated_name: name,
            method_descriptor,
        } = self.generated_methods[&id].clone();

        assembler.add(Instruction::InvokeStatic {
            class_name: self.class_name.clone(),
            name,
            method_descriptor,
        });

        let method = assembler.assemble();
        self.methods.push(method);
    }
}

fn translate_function_type(pool: &mut ConstantPoolBuilder, ty: &FunctionType) -> MethodDescriptor {
    let parameters = ty
        .parameters
        .iter()
        .flat_map(|it| translate_type(pool, it).map(|it| it.to_field_descriptor(pool)))
        .collect();
    let return_type = match &ty.return_type {
        Type::Null => FieldDescriptor::Void,
        other => translate_type(pool, other)
            .map_or(FieldDescriptor::Void, |it| it.to_field_descriptor(pool)),
    };
    MethodDescriptor {
        parameters,
        return_type,
    }
}
#[cfg(test)]
mod tests {
    use crate::{
        auryn::{air::query_air, ast::query_ast2, file_id::FileId, parser::Parser},
        java::class::ClassData,
    };

    fn generate_class_wrapped(input: &str) -> ClassData {
        let wrapped_input = format!("fn main() {{ {input} }}");
        generate_class(&wrapped_input)
    }

    fn generate_class(input: &str) -> ClassData {
        let result = Parser::new(FileId::MAIN_FILE, input).parse();
        let ast = query_ast2(result.syntax_tree.as_ref().unwrap());
        let air = query_air(ast.unwrap());
        assert!(air.diagnostics.is_empty());

        super::generate_class(&air.air)
    }

    #[test]
    fn test_simple() {
        insta::assert_debug_snapshot!(generate_class_wrapped("1 + 2 * 3"));
    }

    #[test]
    fn test_print() {
        insta::assert_debug_snapshot!(generate_class_wrapped("print(2 * 3)"));
        insta::assert_debug_snapshot!(generate_class_wrapped("print(print(\"Hallo, Welt!\"))"));
    }

    #[test]
    fn test_assignment() {
        insta::assert_debug_snapshot!(generate_class_wrapped("let a = 1"));
        insta::assert_debug_snapshot!(generate_class_wrapped("let a = 1\nprint(a)"));
        insta::assert_debug_snapshot!(generate_class_wrapped("let a = 7\na = a * a\nprint(a)"));
    }

    #[test]
    fn test_weird() {
        insta::assert_debug_snapshot!(generate_class_wrapped("loop { loop {} }"));
    }

    #[test]
    fn test_stack_map_table_generation() {
        insta::assert_debug_snapshot!(generate_class_wrapped(
            "loop {\nif 1 {\nprint(42)\n}\nprint(100)\n}"
        ));
    }

    #[test]
    fn test_function() {
        insta::assert_debug_snapshot!(generate_class("fn main() { bar() }\nfn bar() { print(1) }"));
    }

    #[test]
    #[should_panic]
    fn test_reject_invalid_variable() {
        insta::assert_debug_snapshot!(generate_class_wrapped("let a = a"));
    }
}
