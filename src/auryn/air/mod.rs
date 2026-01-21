use crate::auryn::{
    air::{
        ast_transformer::{AirOutput, transform_ast},
        typecheck::type_checker::typecheck_air,
    },
    ast::ast_node,
};

pub mod ast_transformer;
pub mod data;
pub mod namespace;
pub mod typecheck;

pub fn query_air(ast: ast_node::Root) -> AirOutput {
    let mut output = transform_ast(ast);

    output.diagnostics = typecheck_air(&mut output.air, output.diagnostics);

    output
}

#[cfg(test)]
mod tests {
    use crate::auryn::{
        air::{ast_transformer::AirOutput, query_air},
        ast::query_ast,
        file_id::FileId,
        parser::Parser,
    };

    #[track_caller]
    fn compile_wrapped(input: &str) -> AirOutput {
        let wrapped_input = format!("fn main() {{ {input} }}");
        compile(&wrapped_input)
    }

    #[track_caller]
    fn compile(input: &str) -> AirOutput {
        let output = Parser::new(FileId::MAIN_FILE, input).parse();
        let diagnostics = output.syntax_tree.collect_diagnostics();
        if !diagnostics.is_empty() {
            panic!("Could not parse input: {diagnostics:?}");
        }

        let tree = output.syntax_tree;
        println!("{}", tree.display(input));
        let ast = query_ast(&tree).unwrap();
        query_air(ast)
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
}
