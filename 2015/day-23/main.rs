#[derive(Copy, Clone, Debug)]
enum Register { A, B }

type Offset = isize;

#[derive(Debug)]
enum Instruction {
    Half(Register),
    Increment(Register),
    Triple(Register),
    Jump(Offset),
    JumpIfEven(Register, Offset),
    JumpIfOne(Register, Offset)
}

fn parse_register(s: &str) -> Register {
    match &s[0..1] {
        "a" => Register::A,
        "b" => Register::B,
        _ => panic!("parse_register({})", s)
    }
}

fn parse_offset(s: &str) -> Offset {
    s.parse().unwrap()
}

fn parse_line(s: &str) -> Instruction {
    use Instruction::*;
    match s.split_whitespace().collect::<Vec<&str>>()[..] {
        ["hlf", r] => Half(parse_register(r)),
        ["tpl", r] => Triple(parse_register(r)),
        ["inc", r] => Increment(parse_register(r)),
        ["jmp", o] => Jump(parse_offset(o)),
        ["jie", r, o] => JumpIfEven(parse_register(r), parse_offset(o)),
        ["jio", r, o] => JumpIfOne(parse_register(r), parse_offset(o)),
        _ => panic!("parse_line")
    }
}

struct State<'a> {
    a: i64,
    b: i64,
    pc: usize,
    program: &'a Vec<Instruction>
}

impl<'a> std::fmt::Debug for State<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let i = if self.terminated() { "terminated".to_string() } else { format!("{:?}", self.program[self.pc]) };
        f.write_fmt(format_args!("State  A:{:<10}  B:{:<10}  PC:{:3}  ({})", self.a, self.b, self.pc, i))
    }
}

impl<'a> State<'a> {
    fn new(program: &'a Vec<Instruction>) -> State {
        State { a: 0, b: 0, pc: 0, program }
    }

    fn terminated(&self) -> bool {
        self.pc >= self.program.len()
    }

    fn set(&mut self, r:Register, x:i64) {
        match r {
            Register::A => self.a = x,
            Register::B => self.b = x
        }
    }

    fn get(&self, r:Register) -> i64 {
        match r {
            Register::A => self.a,
            Register::B => self.b
        }
    }

    fn jump(&mut self, offset:isize) {
        self.pc = (self.pc as isize + offset) as usize
    }

    fn step(&mut self) {
        use Instruction::*;
        match &self.program[self.pc] {
            Half(r) => { self.set(*r, self.get(*r)/2); self.jump(1) },
            Increment(r) => { self.set(*r, self.get(*r)+1); self.jump(1) },
            Triple(r) => { self.set(*r, self.get(*r)*3) ; self.jump(1) },
            Jump(o) => { self.jump(*o) },
            JumpIfEven(r,o) => { if self.get(*r) % 2 == 0 { self.jump(*o) } else { self.jump(1) } }
            JumpIfOne(r,o) => { if self.get(*r) == 1 { self.jump(*o) } else { self.jump(1) } }
        }
    }
}

fn execute(state: &mut State) {
    println!("Start state: {:?}", state);
    while !state.terminated() {
        state.step();
//        println!("{:?}", state);
    }
    println!("End state: {:?}", state);
}

fn main() {
    let input = std::fs::read_to_string("day-23/input.txt").unwrap();
    let program:Vec<Instruction> = input.lines().map(parse_line).collect();
    let mut state = State::new(&program);
    execute(&mut state);
    let mut state = State::new(&program);
    state.set(Register::A, 1);
    execute(&mut state);
}
