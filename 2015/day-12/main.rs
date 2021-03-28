use json::JsonValue;
use std::convert::TryInto;

enum JsonCollectionIter<'a> {
    ArrayIter(core::slice::Iter<'a, JsonValue>),
    ObjectIter(json::object::Iter<'a>),
    ImmediateNumber(&'a json::number::Number)
}

// Iterator over the "number" elements of a JsonValue.
struct NumberIter<'a, T>
    where T: Fn(&JsonValue) -> bool
{
    value_filter: T,
    iter_stack: Vec<JsonCollectionIter<'a>>
}

impl<'a, T> NumberIter<'a, T>
    where T: Fn(&JsonValue) -> bool
{
    fn new(value_filter: T) -> NumberIter<'a, T> {
        NumberIter { iter_stack: Vec::new(), value_filter }
    }
    // Add the contents of a JsonValue to the "front" of the iterator.
    fn push_front(&mut self, v: &'a JsonValue) {
        use JsonCollectionIter::*;
        use JsonValue::*;
        match v {
            Array(vec) => self.iter_stack.push(ArrayIter(vec.iter())),
            Object(o) => self.iter_stack.push(ObjectIter(o.iter())),
            Number(x) => self.iter_stack.push(ImmediateNumber(x)),
            _ => {}
        }
    }
}

impl<'a, T> Iterator for NumberIter<'a, T>
    where T: Fn(&JsonValue) -> bool
{
    type Item = &'a json::number::Number;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.iter_stack.is_empty() {
                return None
            }

            let mut top = self.iter_stack.pop().unwrap();
            let opt_val = match top {
                JsonCollectionIter::ArrayIter(ref mut i) => i.next(),
                JsonCollectionIter::ObjectIter(ref mut i) => i.next().map(|(_,v)| v),
                JsonCollectionIter::ImmediateNumber(x) => return Some(x)
            };
            // If the iterator's not exhausted, push it back and also push
            // any new values we uncovered.
            if let Some(v) = opt_val {
                self.iter_stack.push(top);
                if (self.value_filter)(v) {
                    self.push_front(v);
                }
            }
        }
    }
}

fn number_iter_filter<'a, T>(v: &'a JsonValue, value_filter: T) -> NumberIter<'a, T>
    where T: Fn(&JsonValue) -> bool
{
    let mut number_iter = NumberIter::new(value_filter);
    number_iter.push_front(v);
    number_iter
}

fn number_iter<'a>(v: &'a JsonValue) -> NumberIter<'a, &dyn Fn(&JsonValue) -> bool>
{
    fn const_true(_: &JsonValue) -> bool { true }
    number_iter_filter(v, &const_true)
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_iter() {
        let v = json::parse(r#"[12, [13, 14], 15]"#).unwrap();
        let actual: Vec<i32> = number_iter(&v).map(|n| number_to_i32(n)).collect();
        assert_eq!(actual, vec![12,13,14,15]);
    }
    #[test]
    fn test_filter() {
        let v = json::parse(r#"[12, {"col":"red", "x":13}, 15]"#).unwrap();
        let actual: Vec<i32> = number_iter_filter(&v, |v| !is_red(v)).map(|n| number_to_i32(n)).collect();
        assert_eq!(actual, vec![12,15]);
    }
    #[test]
    fn test_filter2() {
        let v = json::parse(r#"[12, {"col":"blue", "x":13}, 15]"#).unwrap();
        let actual: Vec<i32> = number_iter_filter(&v, |v| !is_red(v)).map(|n| number_to_i32(n)).collect();
        assert_eq!(actual, vec![12,13,15]);
    }
}

fn is_red(v: &JsonValue) -> bool {
    if let JsonValue::Object(_) = v {
        for (_,v2) in v.entries() {
            if v2.as_str() == Some("red") {
                return true
            }
        }
    }
    false
}

fn number_to_i32(n: &json::number::Number) -> i32 {
    match n.clone().try_into() {
        Ok(i) => i,
        Err(_) => panic!("!")
    }
}

fn main() {
    let contents = std::fs::read_to_string("day-12/input.txt").unwrap();
    let v = json::parse(&contents).unwrap();

    println!("{}", number_iter(&v).map(number_to_i32).sum::<i32>());
    println!("{}", number_iter_filter(&v, |val| !is_red(val)).map(number_to_i32).sum::<i32>());
}
