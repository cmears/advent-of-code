#[derive(Debug)]
enum State {
    Normal,    // no escaping happening
    Escaping,  // just read \
    Hex1,      // just read \x
    Hex2       // just read \x<digit> (e.g. "\x1")
}
use State::*;

fn main() {
    let bytes = std::fs::read("input.txt").unwrap();

    let mut state = Normal;
    let mut code_bytes = 0;
    let mut memory_bytes = 0;
    for b in &bytes {
        if b.is_ascii_whitespace() { continue };
        code_bytes += 1;
        match (&state, b) {
            (Normal, b'\\') => { state = Escaping },
            (Normal, b'"') => {},
            (Normal, _) => { memory_bytes += 1 },
            (Escaping, b'\\') => { state = Normal; memory_bytes += 1 },
            (Escaping, b'"') => { state = Normal; memory_bytes += 1 },
            (Escaping, b'x') => { state = Hex1 },
            (Hex1, _) if b.is_ascii_hexdigit() => { state = Hex2 },
            (Hex2, _) if b.is_ascii_hexdigit() => { state = Normal; memory_bytes += 1 },
            _ => panic!("bad parse; state={:?}; b={}", state, b)
        }
    }

    println!("Part 1: {} code bytes; {} memory bytes; difference = {}", code_bytes, memory_bytes, code_bytes - memory_bytes);

    let mut encoded_bytes = 0;
    for b in &bytes {
        encoded_bytes += 
            match b {
                b'\n' => 2,
                b'"' => 2,
                b'\\' => 2,
                _ => 1
            };
    }

    println!("Part 2: {} code bytes; {} encoded bytes; difference = {}", code_bytes, encoded_bytes, encoded_bytes - code_bytes);
    
}
