fn sum_numbers_except<T>(ignore: &T, v: &json::JsonValue, total: &mut i64)
    where T: Fn(&json::JsonValue) -> bool {
    if !ignore(v) {
        match v {
            json::JsonValue::Number(_) => *total += v.as_i64().unwrap(),
            json::JsonValue::Object(_) => {
                for (_,v2) in v.entries() {
                    sum_numbers_except(ignore, v2, total)
                }
            },
            json::JsonValue::Array(_) => {
                for v2 in v.members() {
                    sum_numbers_except(ignore, v2, total)
                }
            },
            _ => {}
        }
    }
}

fn is_red(v: &json::JsonValue) -> bool {
    if let json::JsonValue::Object(_) = v {
        for (_,v2) in v.entries() {
            if v2.as_str() == Some("red") {
                return true
            }
        }
    }
    false
}

fn main() {
    let contents = std::fs::read_to_string("input.txt").unwrap();
    let v = json::parse(&contents).unwrap();
    let mut x = 0;
    sum_numbers_except(&|_| false, &v, &mut x);
    println!("{}", x);

    let mut x = 0;
    sum_numbers_except(&is_red, &v, &mut x);
    println!("{}", x);
}
