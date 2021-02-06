use std::{fs, time};

use glium::{
    glutin::{self, event::WindowEvent},
    Display, Surface,
};
use glutin::{
    event::Event, event_loop::ControlFlow, event_loop::EventLoop, window::WindowBuilder,
    ContextBuilder,
};
use time::{Duration, Instant};

mod parser;
mod runtime;

const PRELUDE: &str = include_str!("lbstandard.lbz");

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
    let file = match fs::read_to_string(filename) {
        Ok(string) => string,
        Err(_) => {
            println!("something went wrong reading the file: {}", filename);
            return;
        }
    };

    let parsed_file = match parser::parse_leibniz_file(&file) {
        Ok(ast) => ast,
        Err(err) => {
            error(&err);
            unreachable!()
        }
    };

    parsed_total.append_tree(parsed_file);

    /*let event_loop = EventLoop::new();
    let window_builder = WindowBuilder::new();
    let context_builder = ContextBuilder::new();
    let display = Display::new(window_builder, context_builder, &event_loop).unwrap();

    event_loop.run(move |ev, _, control_flow| {
        let mut target = display.draw();
        target.clear_color(0.0, 0.0, 1.0, 1.0);
        target.finish().unwrap();

        let next_frame_time = Instant::now() + Duration::from_nanos(16_666_667);

        *control_flow = ControlFlow::WaitUntil(next_frame_time);

        match ev {
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::CloseRequested => {
                    *control_flow = ControlFlow::Exit;
                    return;
                }
                _ => return,
            },
            _ => (),
        }
    });*/

    match runtime::execute(parsed_total) {
        Ok(value) => println!("{}", value),
        Err(err) => println!("{}", err),
    }
}

fn error(message: &str) {
    println!("{}", message);
    std::process::exit(0);
}