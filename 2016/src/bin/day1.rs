use std::collections::HashSet;

fn turn_left(dir: &mut (i32,i32)) { *dir = (-dir.1, dir.0) }

fn main() {
    let contents = std::fs::read_to_string("input/day1.txt").unwrap();
    let mut pos = (0,0);
    let mut dir = (0,1);
    let mut visited = HashSet::new();
    let mut twice = false;

    for s in contents.split(", ") {
        let (x,y) = s.trim().split_at(1);
        let n:i32 = y.parse().unwrap();
        match x {
            "L" => turn_left(&mut dir),
            "R" => { turn_left(&mut dir); turn_left(&mut dir); turn_left(&mut dir) }
            _ => panic!("!")
        }
        for _ in 0..n {
            pos.0 += dir.0;
            pos.1 += dir.1;
            if !twice {
                if visited.contains(&pos) {
                    println!("Visited twice: {:?} ({} blocks away)", pos, pos.0.abs() + pos.1.abs());
                    twice = true;
                }
                visited.insert(pos);
            }
        }
    }
    println!("Final location: {:?} ({} blocks away)", pos, pos.0.abs() + pos.1.abs());
}
