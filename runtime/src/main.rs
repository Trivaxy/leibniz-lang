use std::fs;

use codegen::CodeGen;

mod codegen;
mod function;
mod instruction;
mod optimizations;
mod runtime;
mod value;

fn main() {
    let args = std::env::args().collect::<Vec<String>>();

    if args.len() == 1 {
        error("leibniz: no input file found");
    }

    let file = match fs::read_to_string(&args[1]) {
        Ok(f) => f,
        Err(e) => error(&format!("{}", e)),
    };

    let parsed_file = match parser::parse_leibniz_file(&file) {
        Ok(ast) => ast,
        Err(err) => error(&err),
    };

    let mut codegen = CodeGen::new("main".to_string());
    codegen.generate_from_node(parsed_file);

    let mut runtime = codegen.dissolve();

    for func in runtime.functions() {
        println!(
            "FUNC {}({}) [{}]:",
            func.1.name,
            func.1.parameters().join(", "),
            func.0
        );

        for instr in func.1.instructions() {
            println!("{}", instr);
        }

        println!("");
    }

    let result = runtime.run();

    match result {
        Ok(val) => println!("{}", val),
        Err(err) => println!("{}", err),
    }
}

fn error(message: &str) -> ! {
    println!("{}", message);
    std::process::exit(0);
}
