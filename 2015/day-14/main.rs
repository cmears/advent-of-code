struct Reindeer {
    speed: i32,
    stamina: i32,
    rest: i32
}

enum Activity {
    Flying(i32),
    Resting(i32)
}

struct State {
    points: i32,
    distance: i32,
    activity: Activity
}

fn parse_line(re: &regex::Regex, s: &str) -> Reindeer {
    match re.captures(s) {
        Some(caps) => {
            let speed = caps.get(2).unwrap().as_str().parse().unwrap();
            let stamina = caps.get(3).unwrap().as_str().parse().unwrap();
            let rest = caps.get(4).unwrap().as_str().parse().unwrap();
            Reindeer { speed, stamina, rest }
        }
        None => panic!("fail")
    }
}

// Slow way.
fn distance(r: &Reindeer, time: i32) -> i32 {
    let mut t = time;
    let mut d = 0;
    while t > 0 {
        let burst_time = std::cmp::min(t, r.stamina);
        d += r.speed * burst_time;
        t -= r.stamina;
        t -= r.rest;
    }
    d
}

fn points(reindeer: &Vec<Reindeer>, time: i32) -> Vec<i32> {
    use Activity::*;
    let mut state: Vec<State> = reindeer.iter().map(|r| State { points: 0, distance: 0, activity: Flying(r.stamina) }).collect();
    let n = reindeer.len();
    // Where is the leader?
    let mut leader = 0;
    for _ in 0..time {
        // Update all the reindeer.
        for i in 0..n {
            let s = &mut state[i];
            match s.activity {
                Flying(ref mut t) => {
                    s.distance += reindeer[i].speed;
                    if s.distance > leader { leader = s.distance }
                    *t -= 1;
                    if *t == 0 { s.activity = Resting(reindeer[i].rest) }
                }
                Resting(ref mut t) => {
                    *t -= 1;
                    if *t == 0 { s.activity = Flying(reindeer[i].stamina) }
                }
            }
        }
        // Award points to the leaders.
        for i in 0..n {
            if state[i].distance == leader {
                state[i].points += 1
            }
        }
    }
    state.iter().map(|s| s.points).collect()
}

fn main() {
    let contents = std::fs::read_to_string("day-14/input.txt").unwrap();
    let re = regex::Regex::new(r"(.*) can fly (.*) km/s for (.*) seconds, but then must rest for (.*) seconds\.").unwrap();
    let reindeer:Vec<Reindeer> = contents.lines().map(|l| parse_line(&re, l)).collect();
    let t = 2503;
    println!("Longest distance: {}", reindeer.iter().map(|r| distance(r,t)).max().unwrap());
    println!("Most points: {}", points(&reindeer, t).iter().max().unwrap());
    
}
