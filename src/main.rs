use std::io::Write;

use auryn::auryn::api::{compile, run};

fn main() -> std::io::Result<()> {
    let mut args = std::env::args();
    args.next().unwrap();
    if let Some(filename) = args.next() {
        let input = std::fs::read_to_string(filename)?;
        match compile(&input) {
            Ok(class) => {
                if args.next().as_deref() == Some("--print-class") {
                    println!("{class:?}");
                }
                print!("{}", run(class, "."));
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
        input.clear();
        read_user_input(&mut input);
        let input = format!("fn main() {{ {input} }}");
        match compile(&input) {
            Ok(class) => {
                print!("{}", run(class, "."));
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
