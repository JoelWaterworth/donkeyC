#[macro_use]
extern crate nom;
extern crate llvm;
extern crate libc;

use llvm::*;
use std::fs::File;
use std::io::Read;
use std::str;
use std::str::FromStr;

mod ir;
mod parser;
mod lexer;

use parser::parser;

// todo implement arrays
// todo implement pattern matching
// todo make it possible to print numbers
// todo also need to implement scan which reads the terminal

fn main() {
    let ctx = Context::new();
    let module = Module::new("main", &ctx);
    let builder = Builder::new(&ctx);

    let mut file = File::open("exampleCode/debug.donk").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    lexer::lexer(&contents);

    //let func = debug( contents.as_bytes());
//    module.verify().unwrap();
//    let ee = JitEngine::new(&module, JitOptions {opt_level: 3}).unwrap();
//    ee.with_function(map.get_function(&String::from("bar")).func, |add:extern fn((f64)) -> f64| {
//        println!("{} = {}", 5., add((5.)));
//    });
}