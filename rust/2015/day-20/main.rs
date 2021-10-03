const INPUT:usize = 34000000;

fn main() {
    let n = INPUT/10;
    let mut x = vec![0; n as usize];
    for i in 1..n {
        let mut j = i;
        while j < n {
            x[j] += i*10;
            j += i;
        }
    }
    for i in 0..n {
        if x[i] >= INPUT {
            println!("{}", i);
            break;
        }
    }

    let n = INPUT/11;
    let mut x = vec![0; n as usize];
    for i in 1..n {
        let mut j = i;
        let mut c = 0;
        while j < n {
            x[j] += i*11;
            j += i;
            c += 1;
            if c == 50 { break }
        }
    }
    for i in 0..n {
        if x[i] >= INPUT {
            println!("{}", i);
            break;
        }
    }
}
