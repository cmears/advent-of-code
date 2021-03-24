use std::fs;
use std::collections::HashMap;
use std::iter::FromIterator;

#[derive(Debug)]
struct Connection {
    output: String,
    input: Input,
}

#[derive(Debug)]
enum Signal {
    Literal(u16),
    Wire(String),
}
use Signal::*;

#[derive(Debug)]
enum Input {
    Direct(Signal),
    And(Signal, Signal),
    Or(Signal, Signal),
    Not(Signal),
    LShift(Signal, Signal),
    RShift(Signal, Signal),
}

use Input::*;

fn parse_signal(s: &str) -> Signal {
    match s.parse() {
        Ok(x) => Literal(x),
        Err(_) => Wire(String::from(s))
    }
}

fn parse_connection(s: &str) -> Connection {
    match s.split_whitespace().collect::<Vec<&str>>()[..] {
        [x, "AND", y, "->", z] => Connection { output: String::from(z), input: And(parse_signal(x), parse_signal(y)) },
        [x, "OR", y, "->", z] => Connection { output: String::from(z), input: Or(parse_signal(x), parse_signal(y)) },
        [x, "LSHIFT", y, "->", z] => Connection { output: String::from(z), input: LShift(parse_signal(x), parse_signal(y)) },
        [x, "RSHIFT", y, "->", z] => Connection { output: String::from(z), input: RShift(parse_signal(x), parse_signal(y)) },
        [x, "->", z] => Connection { output: String::from(z), input: Direct(parse_signal(x)) },
        ["NOT", x, "->", z] => Connection { output: String::from(z), input: Not(parse_signal(x)) },
        _ => panic!("")
    }
}

// Compute a single input, possibly requiring (transitive) evaluation of signals.
fn compute(connections: &HashMap<String, Input>, m: &mut HashMap<String, u16>, input: &Input) -> u16 {
    match input {
        Direct(signal) => get(connections, m, signal),
        Not(signal) => !get(connections, m, signal),
        Or(signal1, signal2) => get(connections, m, signal1) | get(connections, m, signal2),
        And(signal1, signal2) => get(connections, m, signal1) & get(connections, m, signal2),
        LShift(signal1, signal2) => get(connections, m, signal1) << get(connections, m, signal2),
        RShift(signal1, signal2) => get(connections, m, signal1) >> get(connections, m, signal2),
    }
}

// Evaluate a single signal, possibly requiring (indirect) recursive evaluations.
// Store the value in the mutable map.
fn get(connections: &HashMap<String, Input>, m: &mut HashMap<String, u16>, signal: &Signal) -> u16 {
    match signal {
        Literal(x) => *x,
        Wire(s) => {
            match m.get(s) {
                Some(x) => *x,
                None => {
                    let value = compute(&connections, m, connections.get(s).unwrap());
                    m.insert(s.to_string(), value);
                    value
                }
            }
        }
    }
}

// Make sure every connection gets evaluated.
fn simulate(connections: &HashMap<String, Input>) -> HashMap<String, u16> {
    let mut m = HashMap::new();

    for (o,_i) in connections.iter() {
        get(&connections, &mut m, &Wire(o.clone()));
    }
    m
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("?");
    let connections = contents.lines().map(|l| parse_connection(l));
    let mut connections = HashMap::from_iter(connections.map(|c| (c.output, c.input)));

    let m = simulate(&connections);
    let part1_a = m.get(&("a".to_string())).unwrap();
    
    println!("Part 1: wire a has signal {}", part1_a);

    // Override wire b to that signal and simulate afresh.
    connections.insert("b".to_string(), Direct(Literal(*part1_a)));
    
    let m2 = simulate(&connections);
    let part2_a = m2.get(&("a".to_string())).unwrap();
    println!("Part 2: wire a has signal {}", part2_a);
}
