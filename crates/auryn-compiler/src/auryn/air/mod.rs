use crate::auryn::{
    air::{
        ast_transformer::query_globals,
        data::{Air, AirModuleId, AirType, Globals},
        namespace::UserDefinedTypeId,
        typecheck::{type_checker::typecheck_air, type_context::TypeId},
        unresolved_type::UnresolvedType,
    },
    ast::query_ast,
    diagnostics::diagnostic::Diagnostics,
    input_files::InputFile,
};

pub mod ast_transformer;
pub mod data;
pub mod namespace;
pub mod typecheck;
pub mod unresolved_type;

pub fn query_air<'a>(
    input_files: impl Iterator<Item = &'a InputFile> + Clone,
) -> (Air, Diagnostics) {
    let mut diagnostics = Vec::new();
    let mut globals = Globals::default();

    let included_modules = input_files
        .clone()
        .map(|file| (file.name.clone(), AirModuleId(file.file_id)));
    for input_file in input_files.clone() {
        let ast = query_ast(input_file.syntax_tree());
        let output = query_globals(ast, included_modules.clone());
        diagnostics.extend(output.diagnostics.take());
        globals.merge(output.globals);

        globals.types.insert(
            UserDefinedTypeId::Module(AirModuleId(input_file.file_id).into()),
            AirType::Unresolved(UnresolvedType::Module {
                name: input_file.name.clone(),
                id: TypeId::from(AirModuleId(input_file.file_id)).syntax_id(),
                namespace: output.namespace,
            }),
        );
    }

    typecheck_air(globals, diagnostics.into_iter().collect())
}

#[cfg(test)]
mod tests {
    use crate::auryn::{
        air::{data::Air, query_air},
        diagnostics::diagnostic::Diagnostics,
        file_id::FileId,
        input_files::InputFiles,
    };

    #[track_caller]
    fn compile_wrapped(input: &str) -> (Air, Diagnostics) {
        let wrapped_input = format!("fn main() {{ {input} }}");
        compile(&wrapped_input)
    }

    #[track_caller]
    fn compile(input: &str) -> (Air, Diagnostics) {
        let mut input_files = InputFiles::default();
        input_files.add("main".into(), input.into());
        let syntax_tree = input_files.get(FileId::MAIN_FILE).syntax_tree();
        let diagnostics = syntax_tree.collect_diagnostics();
        if !diagnostics.is_empty() {
            panic!("Could not parse input: {diagnostics:?}");
        }

        println!("{}", syntax_tree.display(input));
        query_air(input_files.iter().map(|(_, file)| file))
    }

    #[test]
    fn it_works() {
        insta::assert_debug_snapshot!(compile_wrapped("print(1)"));
        insta::assert_debug_snapshot!(compile_wrapped("if false { print(0) } else { print(1) }"));
    }

    #[test]
    fn test_function_call() {
        insta::assert_debug_snapshot!(compile(
            "fn main() { foo(1) }\nfn foo(bar: I32) -> I32 { print(bar) }"
        ));
    }

    #[test]
    fn test_array_type() {
        insta::assert_debug_snapshot!(compile(
            "fn main() {}\nfn foo(array: []I32) -> []I32 { return array }"
        ));
    }

    #[test]
    fn test_extern_items() {
        insta::assert_debug_snapshot!(compile(
            "unsafe extern \"java\" { [\"java/lang/Foo\"] type Foo { [\"foo\"] static let foo: I32 } }"
        ));
    }

    #[test]
    fn test_intrinsics() {
        insta::assert_debug_snapshot!(compile_wrapped("let a: () = print(1)"));
        insta::assert_debug_snapshot!(compile_wrapped("let a: []I64 = arrayOf(1, 2, 3)"));
        insta::assert_debug_snapshot!(compile_wrapped("let a: []I64 = arrayOfZeros(3)"));
        insta::assert_debug_snapshot!(compile_wrapped("let a: I64 = arrayGet(arrayOfZeros(3), 1)"));
    }

    #[test]
    fn invalid_assignment() {
        insta::assert_debug_snapshot!(compile_wrapped("let a: I32 = print(1)\nprint(a + 1)"));
    }

    #[test]
    fn test_update() {
        insta::assert_debug_snapshot!(compile_wrapped("let a: I32 = 1\na = 2"));
        insta::assert_debug_snapshot!(compile_wrapped("let a: I32 = 1\na += 2"));
    }

    #[test]
    fn test_reassignment() {
        insta::assert_debug_snapshot!(compile_wrapped(
            r#"
            let a: I32 = 0
            if true {
                let a: I32 = 1
            }
            a
            "#
        ))
    }

    #[test]
    fn test_explicit_unit() {
        insta::assert_debug_snapshot!(compile(
            r#"
                fn main() -> {} {
                    foo(50)
                }

                fn foo(val: I32) -> () {
                    if val > 50 {
                        return {}
                    }
                }
            "#
        ));
    }

    #[test]
    fn test_struct() {
        insta::assert_debug_snapshot!(compile(
            r#"
            struct Foo {
                a: I32,
                b: []Foo
            }

            fn main() {}
            "#
        ));
        insta::assert_debug_snapshot!(compile(
            r#"
            struct Foo[T] {
                a: T,
                b: []Foo[T]
            }

            fn main() {}
            "#
        ));
    }

    #[test]
    fn test_generic_function() {
        insta::assert_debug_snapshot!(compile(
            r#"
            fn identity[T](value: T) -> T {
                return value
            }

            fn main() {
                let a: I64 = identity(10)
            }
            "#
        ));
    }
}
