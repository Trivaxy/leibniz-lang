mod parser;

pub use parser::*;

fn parse(file: &str) -> ParserNode {
    match parser::parse_leibniz_file(file) {
        Ok(ast) => ast,
        Err(err) => {
            error(&err);
            unreachable!()
        }
    }
}

fn error(message: &str) {
    println!("{}", message);
    std::process::exit(0);
}
