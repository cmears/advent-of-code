// Add all numbers in a JsonValue.
fn sum_numbers(v: &json::JsonValue, total: &mut i64) {
    match v {
        json::JsonValue::Number(_) => *total += v.as_i64().unwrap(),
        json::JsonValue::Object(_) => {
            for (_,v2) in v.entries() {
                sum_numbers(v2, total)
            }
        },
        json::JsonValue::Array(_) => {
            for v2 in v.members() {
                sum_numbers(v2, total)
            }
        },
        _ => {}
    }
}

// Add all numbers in a JsonValue, but not red objects.
fn sum_numbers_not_red(v: &json::JsonValue, total: &mut i64) {
    match v {
        json::JsonValue::Number(_) => *total += v.as_i64().unwrap(),
        json::JsonValue::Object(_) => {
            for (_,v2) in v.entries() {
                if v2.as_str() == Some("red") {
                    return
                }
            }
            for (_,v2) in v.entries() {
                sum_numbers_not_red(v2, total)
            }
        },
        json::JsonValue::Array(_) => {
            for v2 in v.members() {
                sum_numbers_not_red(v2, total)
            }
        },
        _ => {}
    }
}

fn main() {
    let contents = std::fs::read_to_string("input.txt").unwrap();
    let v = json::parse(&contents).unwrap();
    let mut x = 0;
    sum_numbers(&v, &mut x);
    println!("{}", x);

    let mut x = 0;
    sum_numbers_not_red(&v, &mut x);
    println!("{}", x);
}
