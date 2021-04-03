fn mod_exp(base: u64, exp: u64, modulus: u64) -> u64 {
    let mut result = 1;
    let mut exp = exp;
    let mut base = base;
    loop {
        if exp == 0 { return result }
        if exp % 2 == 1 {
            result = (result * base) % modulus;
        }
        exp = exp / 2;
        base = (base*base) % modulus;
    }
}

fn main() {
    // Puzzle input
    let row = 2978;
    let col = 3083;
    // Which diagonal is it on?
    let diag = row + col - 1;
    // This diagonal starts with the (n-1)th triangular number + 1
    let diag_minus_1 = diag-1;
    // triangle = 1 + 2 + 3 + ... + diag_minus_1
    let triangle = ((diag_minus_1+1) * diag_minus_1)/2;
    let triangle_plus_one = triangle+1;
    // The target number is col-1 spots along.
    let target = triangle_plus_one + col-1;

    println!("target: {}", target);

    // target-1 here because the first number has exp=0
    let code = (20151125 * mod_exp(252533, target-1, 33554393)) % 33554393;
    println!("code: {}", code);
}
