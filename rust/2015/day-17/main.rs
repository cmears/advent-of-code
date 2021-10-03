// How many ways are there to add up to "target", using at most
// "container_budget" containers?
fn combinations(target: i32, container_budget: i32, result: &mut i32, sizes: &[i32]) {
    if container_budget < 0 {
        return;
    } else if target == 0 {
        *result += 1;
    } else if sizes.len() > 0 {
        combinations(target, container_budget, result, &sizes[1..]);
        combinations(target-sizes[0], container_budget-1, result, &sizes[1..]);
    }
}

// What's the fewest number of containers we can use to get to "target"?
fn min_containers(target: i32, containers_used: i32, result: &mut i32, sizes: &[i32]) {
    if containers_used > *result {
        return;
    } else if target == 0 {
        *result = containers_used;
    } else if sizes.len() > 0 {
        min_containers(target, containers_used, result, &sizes[1..]);
        min_containers(target-sizes[0], containers_used+1, result, &sizes[1..]);
    }
}

fn main() {
    let sizes: Vec<i32> = std::fs::read_to_string("day-17/input.txt").unwrap().lines().map(|l| l.parse().unwrap()).collect();

    let mut result = 0;
    combinations(150, sizes.len() as i32, &mut result, &sizes[..]);
    println!("{}", result);

    let mut min_c = i32::MAX;
    min_containers(150, 0, &mut min_c, &sizes[..]);

    let mut result = 0;
    combinations(150, min_c, &mut result, &sizes[..]);
    println!("{}", result);
}
