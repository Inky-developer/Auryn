use stdx::{FastIndexSet, FastSet, SmallString, default};

use crate::{
    auryn::{
        air::{
            data::{Air, AirFunction, AirFunctionId},
            typecheck::{
                type_context::TypeContext,
                types::{Type, TypeView},
            },
        },
        codegen_java::{
            function_generator::FunctionGenerator,
            representation::{
                FieldDescriptor, ImplicitArgs, MethodDescriptor, MethodRepr, RepresentationCtx,
                ReturnDescriptor,
            },
        },
        input_files::InputFiles,
    },
    java::{
        class,
        constant_pool_builder::ConstantPoolBuilder,
        function_assembler::{FunctionAssembler, Instruction},
    },
};

pub(super) fn generate_main_class<'a>(
    air: &Air,
    repr_ctx: &'a mut RepresentationCtx,
    input_files: &'a InputFiles,
) -> class::ClassData {
    let generator = ClassGenerator::new("Main".into(), air, repr_ctx, input_files);
    generator.generate_from_air()
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(super) struct Monomorphization {
    function: AirFunctionId,
    generic_args: Vec<Type>,
}

#[derive(Debug, Default)]
pub(super) struct Monomorphizations {
    pending: FastIndexSet<Monomorphization>,
    seen: FastSet<Monomorphization>,
}

impl Monomorphizations {
    pub fn add(&mut self, id: AirFunctionId, generic_args: Vec<Type>) {
        let mono = Monomorphization {
            function: id,
            generic_args,
        };
        if !self.seen.contains(&mono) {
            self.seen.insert(mono.clone());
            self.pending.insert(mono);
        }
    }

    pub fn pop_pending(&mut self) -> Option<Monomorphization> {
        self.pending.pop()
    }
}

pub struct ClassGenerator<'a> {
    pub(super) input_files: &'a InputFiles,
    pub(super) air: &'a Air,
    pub(super) repr_ctx: &'a mut RepresentationCtx,
    pub(super) class_name: SmallString,
    methods: Vec<class::Method>,
    monomorphizations: Monomorphizations,
    pub(super) constant_pool: ConstantPoolBuilder,
}

impl<'a> ClassGenerator<'a> {
    fn new(
        class_name: SmallString,
        air: &'a Air,
        repr_ctx: &'a mut RepresentationCtx,
        input_files: &'a InputFiles,
    ) -> Self {
        Self {
            input_files,
            air,
            class_name,
            repr_ctx,
            methods: default(),
            constant_pool: default(),
            monomorphizations: default(),
        }
    }
}

impl ClassGenerator<'_> {
    pub fn ty_ctx(&self) -> &TypeContext {
        &self.air.ty_ctx
    }

    fn generate_from_air(mut self) -> class::ClassData {
        self.monomorphizations
            .add(self.air.main_function().0, vec![]);

        while let Some(mono) = self.monomorphizations.pop_pending() {
            let function = &self.air.globals.functions[&mono.function];
            self.generate_function(mono.function, function, mono.generic_args);
        }

        let main_function_id = self.air.main_function().0;
        self.generate_main_function(main_function_id);

        class::ClassData::new(
            self.class_name,
            self.constant_pool,
            Vec::new(),
            self.methods,
        )
    }

    fn get_method_repr(&mut self, id: AirFunctionId, monomorphization: Vec<Type>) -> MethodRepr {
        let function = &self.air.globals.functions[&id];
        let TypeView::FunctionItem(function_type) = function.r#type.as_view(&self.air.ty_ctx)
        else {
            panic!("Invalid function type")
        };
        let name = self.repr_ctx.get_method_name(
            self.input_files,
            function,
            id,
            monomorphization
                .iter()
                .map(|it| it.as_view(&self.air.ty_ctx)),
        );
        let method_descriptor = self
            .repr_ctx
            .get_method_descriptor(function_type, monomorphization);
        MethodRepr {
            generated_name: name.into(),
            method_descriptor,
        }
    }

    fn generate_function(
        &mut self,
        id: AirFunctionId,
        function: &AirFunction,
        monomorphization: Vec<Type>,
    ) {
        let method = {
            let MethodRepr {
                generated_name,
                method_descriptor,
            } = self.get_method_repr(id, monomorphization.clone());
            self.repr_ctx.monomorphization = monomorphization;
            let mut repr_ctx = std::mem::take(self.repr_ctx);
            let mut constant_pool = std::mem::take(&mut self.constant_pool);
            let mut monomorphizations = std::mem::take(&mut self.monomorphizations);

            let mut generator = FunctionGenerator::new(
                generated_name,
                function,
                method_descriptor,
                self,
                &mut repr_ctx,
                &mut monomorphizations,
                &mut constant_pool,
            );
            generator.generate_from_function(function);
            let result = generator.finish();

            self.monomorphizations = monomorphizations;
            self.constant_pool = constant_pool;
            *self.repr_ctx = repr_ctx;
            self.repr_ctx.monomorphization.clear();
            result
        };
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

        let MethodRepr {
            generated_name: name,
            method_descriptor,
        } = self.get_method_repr(id, Vec::new());

        // if the auryn main function is already defined like the java main function, we don't need generate a wrapper
        if method_descriptor == main_descriptor {
            return;
        }

        assert!(
            method_descriptor.parameters.is_empty(),
            "Main function should not take arguments"
        );

        let mut assembler = FunctionAssembler::new(
            self.constant_pool.add_class(self.class_name.clone()),
            "main".into(),
            main_descriptor,
            ImplicitArgs::None,
            &mut self.constant_pool,
        );

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
        auryn::{
            api::compile_in_memory, codegen_java::codegen::CodegenOutput, environment::ProjectTree,
        },
        java::class::ClassData,
    };

    fn generate_class_wrapped(input: &str) -> ClassData {
        let wrapped_input = format!("fn main() {{ {input} }}");
        generate_class(&wrapped_input)
    }

    fn generate_class(input: &str) -> ClassData {
        let mut output = generate_classes(&format!("// main\n{input}"));
        assert_eq!(
            output.files.len(),
            1,
            "Should use `generate_classes` for multi file test output"
        );
        output.files.remove("Main").unwrap()
    }

    fn generate_classes(input: &str) -> CodegenOutput {
        let files = input
            .split("// ")
            .filter(|file| !file.is_empty())
            .map(|file| {
                let (name, code) = file.split_once("\n").unwrap();
                (name.into(), code.into())
            })
            .collect();
        let project_tree = ProjectTree {
            source_files: files,
        };
        match compile_in_memory(project_tree) {
            Ok(output) => output,
            Err(err) => {
                panic!("Could not compile '{input}':\n{err}")
            }
        }
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
        insta::assert_debug_snapshot!(generate_class_wrapped("not false"));
        insta::assert_debug_snapshot!(generate_class_wrapped("not false and true"));
        insta::assert_debug_snapshot!(generate_class_wrapped("not true"));
    }

    #[test]
    fn test_loop() {
        insta::assert_debug_snapshot!(generate_class_wrapped("loop { loop {} }"));
        insta::assert_debug_snapshot!(generate_class_wrapped("loop { break }"));
        insta::assert_debug_snapshot!(generate_class_wrapped(
            r#"
            loop {
                if true {
                    continue
                }
                if false {
                    break
                }
            }
            "#
        ));
    }

    #[test]
    fn test_while_loop() {
        insta::assert_debug_snapshot!(generate_class_wrapped(
            r#"
            while true { }
            "#
        ));
        insta::assert_debug_snapshot!(generate_class_wrapped(
            r#"
            let i: I32 = 0
            while i < 5 {
                print(i)
                i += 1
            }
            print("Done")
            "#
        ));
    }

    #[test]
    fn test_stack_map_table_generation() {
        insta::assert_debug_snapshot!(generate_class_wrapped(
            "loop {\nif true {\nprint(42)\n}\nprint(100)\n}"
        ));

        // Tests that stack map frames are correctly deduplicated, see `convert_verification_frames` in `source_graph.rs`
        insta::assert_debug_snapshot!(generate_class_wrapped(
            r#"
        	loop {
        		loop {
        			if true {
        				let val: I32 = 0
        				if false {
        				}
        			}
        		}
        	}
            "#
        ));
    }

    #[test]
    fn test_function() {
        insta::assert_debug_snapshot!(generate_class("fn main() { bar() }\nfn bar() { print(1) }"));
    }

    #[test]
    fn test_arrays() {
        insta::assert_debug_snapshot!(generate_class_wrapped("let a: []I32 = arrayOfZeros(5)"));
        insta::assert_debug_snapshot!(generate_class_wrapped("let a: [][]I32 = arrayOfZeros(5)"));
        insta::assert_debug_snapshot!(generate_classes(
            r#"
            // main
            fn main() {
                let a: []{a: I32, b: I32} = arrayOf({a: 5, b: 6})
            }
            "#
        ));
    }

    #[test]
    fn test_conversions() {
        insta::assert_debug_snapshot!(generate_class_wrapped(
            "let a: I32 = 5\nlet b: I64 = cast(a)"
        ));
        insta::assert_debug_snapshot!(generate_class_wrapped(
            "let a: I64 = 5\nlet b: I32 = cast(a)"
        ));
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
    fn test_structural() {
        insta::assert_debug_snapshot!(generate_classes(
            r#"
            // main
            fn main() { print({a: true, b: 2, c: false}) }
            "#
        ));
        insta::assert_debug_snapshot!(generate_classes(
            r#"
            // main
            fn main() { print({a: 1}.a) }
            "#
        ));
        insta::assert_debug_snapshot!(generate_classes(
            r#"
            // main
            fn main() { print({a: "test"}.a) }
            "#
        ));
        insta::assert_debug_snapshot!(generate_classes(
            r#"
            // main
            fn main() {
                let val: {a: I32} = {a: 42}
                print(val.a)
            }
            "#
        ));
    }

    #[test]
    fn test_struct() {
        insta::assert_debug_snapshot!(generate_classes(
            r#"
            // main
            struct Foo {
                a: I32,
                b: []Foo
            }
            fn main() {
                let a: []Foo = arrayOf()
            }
            "#
        ));
    }

    #[test]
    fn test_module() {
        insta::assert_debug_snapshot!(generate_classes(
            r#"
            // main
            fn main() {
                foo.sayHi()
            }

            // foo
            fn sayHi() {
                print("hi")
            }
            "#
        ))
    }

    #[test]
    fn test_generic_function() {
        insta::assert_debug_snapshot!(generate_classes(
            r#"
            // main
            fn main() {
                let a: I32 = identity(0)
                let b: String = identity("hi")
            }

            fn identity[T](value: T) -> T {
                return value
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
