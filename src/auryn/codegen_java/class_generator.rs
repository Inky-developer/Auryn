use crate::{
    auryn::{
        air::{
            data::{Air, AirFunction, AirFunctionId},
            typecheck::types::TypeView,
        },
        codegen_java::{
            function_generator::generate_function,
            representation::{
                FieldDescriptor, MethodDescriptor, ReturnDescriptor, get_function_representation,
            },
        },
    },
    java::{
        class::{self},
        constant_pool_builder::ConstantPoolBuilder,
        function_assembler::{FunctionAssembler, Instruction},
    },
    utils::{fast_map::FastMap, small_string::SmallString},
};

pub fn generate_class(air: &Air) -> class::ClassData {
    let generator = ClassGenerator::new("Main".into(), air);
    generator.generate_from_air()
}

#[derive(Debug, Clone)]
pub struct GeneratedMethodData {
    pub generated_name: SmallString,
    pub method_descriptor: MethodDescriptor,
}

pub struct ClassGenerator<'a> {
    air: &'a Air,
    class_name: SmallString,
    methods: Vec<class::Method>,
    constant_pool: ConstantPoolBuilder,
    generated_methods: FastMap<AirFunctionId, GeneratedMethodData>,
}

impl<'a> ClassGenerator<'a> {
    fn new(class_name: SmallString, air: &'a Air) -> Self {
        Self {
            air,
            class_name,
            methods: Vec::new(),
            constant_pool: ConstantPoolBuilder::default(),
            generated_methods: FastMap::default(),
        }
    }
}

impl ClassGenerator<'_> {
    fn generate_from_air(mut self) -> class::ClassData {
        for (function_id, function) in &self.air.functions {
            let TypeView::FunctionItem(function_type) =
                function.r#type.computed().as_view(&self.air.ty_ctx)
            else {
                unreachable!("Function should have a function type");
            };
            let method_descriptor = get_function_representation(function_type);

            let mangled_name = function.ident.clone();

            self.generated_methods.insert(
                *function_id,
                GeneratedMethodData {
                    generated_name: mangled_name,
                    method_descriptor,
                },
            );
        }

        for (function_id, function) in &self.air.functions {
            self.generate_function(*function_id, function);
        }

        let main_function_id = self.air.main_function().0;
        self.generate_main_function(main_function_id);

        class::ClassData::new(self.class_name, self.constant_pool, self.methods)
    }

    fn generate_function(&mut self, id: AirFunctionId, function: &AirFunction) {
        let data = &self.generated_methods[&id];
        let method = generate_function(
            &mut self.constant_pool,
            &self.air.ty_ctx,
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
            return_type: ReturnDescriptor::Void,
        };

        let GeneratedMethodData {
            generated_name: name,
            method_descriptor,
        } = self.generated_methods[&id].clone();

        // if the auryn main function is already defined like the java main function, we don't need generate a wrapper
        if method_descriptor == main_descriptor {
            return;
        }

        assert!(
            method_descriptor.parameters.is_empty(),
            "Main function should not take arguments"
        );

        let mut assembler =
            FunctionAssembler::new("main".into(), main_descriptor, &mut self.constant_pool, 1);

        assembler.add(Instruction::InvokeStatic {
            class_name: self.class_name.clone(),
            name,
            method_descriptor,
        });

        let method = assembler.assemble();
        self.methods.push(method);
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        auryn::{air::query_air, ast::query_ast, file_id::FileId, parser::Parser},
        java::class::ClassData,
    };

    fn generate_class_wrapped(input: &str) -> ClassData {
        let wrapped_input = format!("fn main() {{ {input} }}");
        generate_class(&wrapped_input)
    }

    fn generate_class(input: &str) -> ClassData {
        let result = Parser::new(FileId::MAIN_FILE, input).parse();
        let ast = query_ast(result.syntax_tree.as_ref().unwrap());
        let air = query_air(ast.unwrap());
        let mut diagnostics = Vec::new();
        diagnostics.extend(result.syntax_tree.unwrap().collect_diagnostics());
        diagnostics.extend(air.diagnostics.take());
        assert!(diagnostics.is_empty(), "Got diagnostics: {diagnostics:?}");

        super::generate_class(&air.air)
    }

    #[test]
    fn test_simple() {
        insta::assert_debug_snapshot!(generate_class_wrapped("1 + 2 * 3"));
        insta::assert_debug_snapshot!(generate_class_wrapped("1 / 2 % 3"));
    }

    #[test]
    fn test_print() {
        insta::assert_debug_snapshot!(generate_class_wrapped("print(2 * 3)"));
        insta::assert_debug_snapshot!(generate_class_wrapped("print(print(\"Hallo, Welt!\"))"));
    }

    #[test]
    fn test_assignment() {
        insta::assert_debug_snapshot!(generate_class_wrapped("let a: I32 = 1"));
        insta::assert_debug_snapshot!(generate_class_wrapped("let a: I32 = 1\nprint(a)"));
        insta::assert_debug_snapshot!(generate_class_wrapped(
            "let a: I32 = 7\na = a * a\nprint(a)"
        ));
    }

    #[test]
    fn test_logic() {
        insta::assert_debug_snapshot!(generate_class_wrapped("false and true"));
        insta::assert_debug_snapshot!(generate_class_wrapped("false or true"));
    }

    #[test]
    fn test_weird() {
        insta::assert_debug_snapshot!(generate_class_wrapped("loop { loop {} }"));
    }

    #[test]
    fn test_stack_map_table_generation() {
        insta::assert_debug_snapshot!(generate_class_wrapped(
            "loop {\nif true {\nprint(42)\n}\nprint(100)\n}"
        ));
    }

    #[test]
    fn test_function() {
        insta::assert_debug_snapshot!(generate_class("fn main() { bar() }\nfn bar() { print(1) }"));
    }

    #[test]
    fn test_i64() {
        insta::assert_debug_snapshot!(generate_class(
            r#"
            fn main() {
                let a: I64 = 35
                let b: I64 = 65
                let sum = a + b
                if sum >= 100 {
                    print("Greater or equal than 100")
                }
                printArray(arrayOf(a, b, sum))
            }

            fn printArray(values: []I64) {
                let i: I32 = 0
                loop {
                    if i >= arrayLen(values) {
                        break
                    }

                    print(arrayGet(values, i))

                    i += 1
                }
            }
            "#
        ));

        insta::assert_debug_snapshot!(generate_class(
            r#"
            fn main() {
                foo(2034)
            }

            fn foo(a: I64) {
                let b = a + 1            
                print(b % 2 == 1)
            }
            "#
        ));
    }

    #[test]
    fn test_externs() {
        insta::assert_debug_snapshot!(generate_class(
            r#"
            unsafe extern "java" {
                ["java/io/PrintStream"]
                type PrintStream {}

                ["java/lang/System"]
                type System {
                    ["out"]
                    static let out: PrintStream
                }
            }
            fn main() { System.out }
            "#
        ));

        insta::assert_debug_snapshot!(generate_class(
            r#"
            unsafe extern "java" {
            	["java/io/PrintStream"]
            	type PrintStream {
            		["println"]
            		fn printString(string: String)
            	}

            	["java/lang/System"]
            	type System {
            		["out"]
            		static let out: PrintStream
            	}
            }

            fn main() {
            	System.out.printString("Hello world!")
            }
            "#
        ))
    }

    #[test]
    #[should_panic]
    fn test_reject_invalid_variable() {
        insta::assert_debug_snapshot!(generate_class_wrapped("let a = a"));
    }
}
