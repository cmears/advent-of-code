// The approach here is very naive (slow): check every permutation.

fn increment(a: &mut[u8]) {
    let mut i = a.len()-1;
    loop {
        a[i] += 1;
        if a[i] <= 26 { break };
        a[i] = 1;
        i -= 1;
    }
}

fn check(a: &[u8]) -> bool {
    let mut i = 0;
    loop {
        if a[i+2] == a[i+1]+1 && a[i+1] == a[i]+1 { break }
        i += 1;
        if i+2 == a.len() { return false }
    }

    for x in a {
        if *x == 9 || *x == 15 || *x == 12 { return false }
    }

    i = 1;
    loop {
        if a[i-1] == a[i] { break }
        i += 1;
        if i == a.len() { return false }
    }

    i += 2;
    if i >= a.len() { return false }

    loop {
        if a[i-1] == a[i] { break }
        i += 1;
        if i == a.len() { return false }
    }

    true
}

fn main() {
    let input = "hepxcrrq";
    let mut a = [0;8];
    a.clone_from_slice(input.as_bytes());
    // Now a holds the ASCII values of the input.
    for x in &mut a { *x -= 96 };
    // Now a holds the letter positions [8,5,15,etc.]

    while !check(&a) {
        increment(&mut a);
    }

    let mut s = String::new();
    for x in &a { s.push(char::from(96+x)) };
    println!("{}", s);

    increment(&mut a);
    
    while !check(&a) {
        increment(&mut a);
    }

    let mut s = String::new();
    for x in &a { s.push(char::from(96+x)) };
    println!("{}", s);
}
