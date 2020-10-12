use std::fs;

mod parser;
mod runtime;

const PRELUDE: &'static str = include_str!("lbstandard.lbz");

fn main() {
    let args = std::env::args().collect::<Vec<String>>();

    if args.len() == 1 {
        error("leibniz: no input file found");
    }

    let mut parsed_total = match parser::parse_leibniz_file(PRELUDE) {
        Ok(ast) => ast,
        Err(err) => {
            error(&err);
            unreachable!()
        }
    };

    let filename = &args[1];
    let file = fs::read_to_string(filename).expect("something went wrong reading the file");

    let parsed_file = match parser::parse_leibniz_file(&file) {
        Ok(ast) => ast,
        Err(err) => {
            error(&err);
            unreachable!()
        }
    };

    parsed_total.append_tree(parsed_file);

    match runtime::execute(parsed_total) {
        Ok(value) => println!("{}", value),
        Err(err) => println!("{}", err)
    }
}

fn error(message: &str) {
    println!("{}", message);
    std::process::exit(0);
}