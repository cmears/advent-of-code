fn next(a: &[u8]) -> Vec<u8> {
    let mut v = Vec::new();
    let mut i = 0;
    let mut t = Vec::new();
    while i < a.len() {
        let x = a[i];
        let mut count = 1;
        i += 1;
        while i < a.len() && a[i] == x {
            i += 1;
            count += 1;
        }
        // This ad hoc "integer -> digits" conversion is much faster
        // than actually making a String and adding the characters to
        // the vector.
        while count > 0 {
            t.push(count % 10);
            count /= 10;
        }
        while t.len() > 0 {
            v.push(t.pop().unwrap());
        }
        v.push(x);
    }
    v
}

fn main() {
    let mut v = vec![3,1,1,3,3,2,2,1,1,3];

    for _ in 0..40 {
        v = next(&v);
    }

    println!("{:?}", v.len());

    for _ in 0..10 {
        v = next(&v);
    }

    println!("{:?}", v.len());
}
