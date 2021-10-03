struct Ingredient {
    capacity: i32,
    durability: i32,
    flavor: i32,
    texture: i32,
    calories: i32
}

fn parse_line(re: &regex::Regex, line: &str) -> Ingredient {
    match re.captures(line) {
        None => panic!("regex failed"),
        Some(caps) => {
            let capacity = caps.get(1).unwrap().as_str().parse().unwrap();
            let durability = caps.get(2).unwrap().as_str().parse().unwrap();
            let flavor = caps.get(3).unwrap().as_str().parse().unwrap();
            let texture = caps.get(4).unwrap().as_str().parse().unwrap();
            let calories = caps.get(5).unwrap().as_str().parse().unwrap();
            Ingredient { capacity, durability, flavor, texture, calories }
        }
    }
}

fn main() {
    let input = std::fs::read_to_string("day-15/input.txt").unwrap();
    let re = regex::Regex::new(r".*: capacity (.*), durability (.*), flavor (.*), texture (.*), calories (.*)").unwrap();
    let ingredients: Vec<Ingredient> = input.lines().map(|l| parse_line(&re, l)).collect();

    assert!(ingredients.len() == 4);
    let mut best = 0;    // Best ignoring calories
    let mut best500 = 0; // Best with 500 calories
    for a in 0..100 {
        for b in 0..(100-a) {
            for c in 0..(100-a-b) {
                let d = 100-a-b-c;

                let coefficients = [a,b,c,d];
                let capacity: i32 = coefficients.iter().zip(ingredients.iter()).map(|(c,i)| c*i.capacity).sum();
                let durability: i32 = coefficients.iter().zip(ingredients.iter()).map(|(c,i)| c*i.durability).sum();
                let flavor: i32 = coefficients.iter().zip(ingredients.iter()).map(|(c,i)| c*i.flavor).sum();
                let texture: i32 = coefficients.iter().zip(ingredients.iter()).map(|(c,i)| c*i.texture).sum();
                let calories: i32 = coefficients.iter().zip(ingredients.iter()).map(|(c,i)| c*i.calories).sum();

                let score = if capacity < 0 || durability < 0 || flavor < 0 || texture < 0 {
                    0
                } else {
                    capacity as i64 * durability as i64 * flavor as i64 * texture as i64
                };
                
                if score > best {
                    best = score;
                }

                if score > best500 && calories == 500 {
                    best500 = score;
                }
            }
        }
    }
    println!("Best score (open category): {}", best);
    println!("Best score (500 calories): {}", best500);
}
