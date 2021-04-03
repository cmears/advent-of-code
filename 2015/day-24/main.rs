//use advent_of_code_2015;

mod dfs {
    pub fn search<T,V>(start: T, mut visit: V)
    where
        V: FnMut(&T) -> Vec<T>
    {
        let mut stack = Vec::new();
        stack.push(start);

        while let Some(node) = stack.pop() {
            let neighbours = visit(&node);
            for n in neighbours {
                stack.push(n);
            }
        }
    }
}

#[derive(Clone, Debug)]
struct State {
    n_assigned: usize,
    n_passenger: usize,
    qe_passenger: usize,
    w_passenger: usize,
    w_container1: usize,
    w_container2: usize,
    w_container3: usize
}

fn main() {
    let packages:Vec<usize> = vec![1,2,3,5,7,13,17,19,23,29,31,37,41,43,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113].into_iter().rev().collect();
    let total_weight:usize = packages.iter().sum();
    let target_weight3 = total_weight / 3;
    let target_weight4 = total_weight / 4;
    println!("total weight = {}; target weight (3) = {}, target weight (4) = {}", total_weight, target_weight3, target_weight4);
    let mut best = (packages.len()+1, 0);
    let init = State {
        n_assigned: 0,
        n_passenger: 0,
        qe_passenger: 1,
        w_passenger: 0,
        w_container1: 0,
        w_container2: 0,
        w_container3: 0
    };
    let mut visit = |part: i32, s:&State| {
        let mut vec = Vec::new();
        let objective = (s.n_passenger, s.qe_passenger);
        let ok;
        let target_weight = if part == 1 { target_weight3 } else { target_weight4 };
        if part == 1 {
            ok = s.w_passenger <= target_weight && s.w_container1 <= target_weight && s.w_container2 <= target_weight && objective < best;
        } else {
            ok = s.w_passenger <= target_weight && s.w_container1 <= target_weight && s.w_container2 <= target_weight && s.w_container3 <= target_weight && objective < best;
        }
        if !ok { return vec }

        let goal = s.n_assigned == packages.len();
        if goal {
            println!("new best: {:?}", objective);
            best = objective;
            return vec
        }
        let x = packages[s.n_assigned];

        // Optimisation: stop when we can't reach the weight target in
        // the passenger compartment (because reaching the weight
        // target would require so many packages we would be worse
        // than the optimal solution).
        
        // How many more packages could we put in the passenger compartment?
        let passenger_remaining = best.0 - s.n_passenger;
        // What's the max weight of such packages?
        // Because we assign in order, it's the next n packages.
        let mut max_weight = 0;
        let mut i = s.n_assigned;
        while i < packages.len() && i < s.n_assigned + passenger_remaining {
            max_weight += packages[i];
            i += 1;
        }
        // If the max weight doesn't get us to the target, we're dead.
        if s.w_passenger + max_weight < target_weight {
            return vec
        }

        // Assign to container 1.
        let mut s2 = s.clone();
        s2.n_assigned += 1;
        s2.w_container1 += x;
        vec.push(s2);

        // Assign to container 2.
        let mut s2 = s.clone();
        s2.n_assigned += 1;
        s2.w_container2 += x;
        vec.push(s2);

        if part == 2 {
            // Assign to container 3.
            let mut s2 = s.clone();
            s2.n_assigned += 1;
            s2.w_container3 += x;
            vec.push(s2);
        }

        // Assign to passenger compartment.
        let mut s2 = s.clone();
        s2.n_assigned += 1;
        s2.n_passenger += 1;
        s2.qe_passenger *= x;
        s2.w_passenger += x;
        vec.push(s2);

        vec
    };

    println!("part 1");
    dfs::search(init.clone(), |s| { visit(1, s) });
    println!("part 2");
    dfs::search(init.clone(), |s| { visit(2, s) });
}
