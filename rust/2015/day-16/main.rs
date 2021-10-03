use std::collections::HashMap;

struct Sue {
    label: i32,
    properties: HashMap<String, i32>
}

fn parse_line(re: &regex::Regex, line: &str) -> Sue {
    match re.captures(line) {
        None => panic!("regex failed ({})", line),
        Some(caps) => {
            let label = caps.get(1).unwrap().as_str().parse().unwrap();
            let k1 = caps.get(2).unwrap().as_str().to_string();
            let v1 = caps.get(3).unwrap().as_str().parse().unwrap();
            let k2 = caps.get(4).unwrap().as_str().to_string();
            let v2 = caps.get(5).unwrap().as_str().parse().unwrap();
            let k3 = caps.get(6).unwrap().as_str().to_string();
            let v3 = caps.get(7).unwrap().as_str().parse().unwrap();
            let mut h = HashMap::new();
            h.insert(k1,v1);
            h.insert(k2,v2);
            h.insert(k3,v3);
            Sue { label, properties: h }
        }
    }
}

fn main() {
    let input = std::fs::read_to_string("day-16/input.txt").unwrap();
    let re = regex::Regex::new(r"Sue (.*): (.*): (.*), (.*): (.*), (.*): (.*)").unwrap();
    let sues: Vec<Sue> = input.lines().map(|l| parse_line(&re, l)).collect();

    let mut target = HashMap::new();
    target.insert("children".to_string(), 3);
    target.insert("cats".to_string(), 7);
    target.insert("samoyeds".to_string(), 2);
    target.insert("pomeranians".to_string(), 3);
    target.insert("akitas".to_string(), 0);
    target.insert("vizslas".to_string(), 0);
    target.insert("goldfish".to_string(), 5);
    target.insert("trees".to_string(), 3);
    target.insert("cars".to_string(), 2);
    target.insert("perfumes".to_string(), 1);

    let suspect = sues.iter().find(|s| s.properties.iter().all(|(k,v)| target.get(k).unwrap() == v)).unwrap();
    println!("{}", suspect.label);

    let suspect2 = sues.iter().find(|s| {
        s.properties.iter().all(|(k,v)| {
            match k.as_str() {
                "cats" | "trees" => v > target.get(k).unwrap(),
                "pomeranians" | "goldfish" => v < target.get(k).unwrap(),
                _ => v == target.get(k).unwrap()
            }
        })
    }).unwrap();
    println!("{}", suspect2.label);
}

