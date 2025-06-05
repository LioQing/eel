use std::env;

use eel;

fn main() {
    let filename = env::args().nth(1).expect("Expected file argument");
    eel::run(filename);
}
