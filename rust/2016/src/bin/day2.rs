// Keypad shape:
//
// 123
// 456
// 789

fn part1(contents: &String) {
    let mut code:Vec<i32> = Vec::new();
    for l in contents.lines() {
        let mut x = 5;
        for c in l.chars() {
            match c {
                'L' if x % 3 != 1 => x -= 1,
                'R' if x % 3 != 0 => x += 1,
                'U' if x > 3 => x -= 3,
                'D' if x < 7 => x += 3,
                _ => ()
            }
        }
        code.push(x);
    }
    println!("{:?}", code);
}

//     1
//   2 3 4
// 5 6 7 8 9
//   A B C
//     D
fn part2(contents: &String) {
    let right = "234.56789.ABC".to_string();
    let left = right.chars().rev().collect();
    let down = "26A.137BD.48C".to_string();
    let up = down.chars().rev().collect();
    let mut code:Vec<char> = Vec::new();
    for l in contents.lines() {
        let mut x = '5';
        for c in l.chars() {
            let s = match c {
                'L' => &left,
                'R' => &right,
                'U' => &up,
                'D' => &down,
                _ => panic!("!")
            };
            // println!("{} {} {}", x, c, s);
            let mut i = s.chars().skip_while(|c| *c != x );
            // i either points to the x, or nothing.
            // Move to the point after the x
            i.next();
            // Now i points to element to move to, or a dot, or nothing.
            if let Some(y) = i.next() {
                if y != '.' {
                    x = y;
                }
            }
        }
        code.push(x);
    }
    println!("{:?}", code);
}

fn main() {
    let contents = std::fs::read_to_string("input/day2.txt").unwrap();
    part1(&contents);
    part2(&contents);
}
