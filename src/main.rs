use std::{io::Write, path::Path};

use auryn::auryn::api::{compile_file, compile_str, run};
use yansi::Paint;

const BUILD_DIR: &str = "build";

fn main() -> std::io::Result<()> {
    let mut args = std::env::args();
    args.next().unwrap();
    if let Some(filename) = args.next() {
        match compile_file(Path::new(&filename)) {
            Ok(class) => {
                println!("Compilation completed {}!", "successfully".green());
                if args.next().as_deref() == Some("--print-class") {
                    println!("{class:?}");
                }
                print!("{}", run(class, BUILD_DIR));
            }
            Err(diagnostics) => diagnostics.to_display().eprint(),
        }
    } else {
        repl();
    }

    Ok(())
}

fn repl() {
    let mut input = String::new();
    loop {
        String::clear(&mut input);
        read_user_input(&mut input);
        let input = format!("fn main() {{ {input} }}");
        match compile_str(&input) {
            Ok(class) => {
                print!("{}", run(class, BUILD_DIR));
            }
            Err(diagnostics) => diagnostics.to_display().eprint(),
        }
    }
}

fn read_user_input(buf: &mut String) {
    print_prompt();
    for line in std::io::stdin().lines() {
        let line = line.unwrap();
        if line.is_empty() {
            break;
        }
        buf.push_str(&line);
        buf.push('\n');

        print_prompt();
    }
}

fn print_prompt() {
    print!("> ");
    std::io::stdout().flush().unwrap();
}
