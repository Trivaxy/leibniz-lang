use std::fs;

mod parser;
mod runtime;

const PRELUDE: &'static str = include_str!("lbstandard.lbz");

fn main() {
    let args = std::env::args().collect::<Vec<String>>();

    if args.len() == 1 {
        error("leibniz: no input file found");
    }

    let filename = &args[1];
    let file = fs::read_to_string(filename).expect("something went wrong reading the file");

    let mut full_script = PRELUDE.to_owned();
    full_script.push_str("\n");
    full_script.push_str(&file);

    match parser::parse_leibniz_file(&full_script) {
        Ok(ast) => match runtime::execute(ast) {
            Ok(num) => println!("{}", num),
            Err(error) => println!("runtime error: {}", error)
        },
        Err(error) => println!("parsing error: {}", error)
    }
}

fn error(message: &str) {
    println!("{}", message);
    std::process::exit(0);
}