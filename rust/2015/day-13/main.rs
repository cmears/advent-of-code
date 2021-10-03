use std::collections::HashMap;

// Graph represented by adjacency matrix.
#[derive(Debug)]
pub struct Graph {
    pub n: usize,
    adj: Vec<i32>
}

impl Graph {
    pub fn new(n: usize) -> Graph {
        Graph { n, adj: vec![0; n*n] }
    }
    pub fn insert(&mut self, u: usize, v: usize, w: i32) {
        self.adj[u*self.n+v] = w
    }
    pub fn get(&self, u: usize, v: usize) -> i32 {
        self.adj[u*self.n+v]
    }
    pub fn get_mut(&mut self, u: usize, v: usize) -> &mut i32 {
        &mut self.adj[u*self.n+v]
    }
}

fn heaviest_hamiltonian_cycle(g: &Graph) -> i32 {
    let mut best = i32::MIN;
    let mut stack = Vec::new();
    let mut visited = vec![false; g.n];

    fn f(g: &Graph, stack: &mut Vec<usize>, visited: &mut Vec<bool>, best: &mut i32, i: usize, cost: i32) {
        // Path is finished; check cost.
        if i == g.n {
            // Finish the cycle.
            let cost = cost + g.get(stack[i-1], stack[0]);
            if cost > *best {
                *best = cost;
            }
            return;
        }

        // Try every extension of the current partial path.
        for next in 0..g.n {
            if visited[next] { continue }
            stack.push(next);
            visited[next] = true;
            let c = if i == 0 { 0 } else { g.get(stack[i-1], stack[i]) };
            f(g, stack, visited, best, i+1, cost + c);
            visited[next] = false;
            stack.pop();
        }
    }
    
    f(g, &mut stack, &mut visited, &mut best, 0, 0);

    best
}

fn parse_line(re: &regex::Regex, s: &str) -> (String, String, i32) {
    match re.captures(s) {
        Some(caps) => {
            let x = caps.get(1).unwrap().as_str();
            let y = caps.get(4).unwrap().as_str();
            let w: i32 = caps.get(3).unwrap().as_str().parse().unwrap();
            let gain = caps.get(2).unwrap();
            let w = if gain.as_str() == "gain" { w } else { -w };
            (x.to_string(),y.to_string(),w)
        }
        None => panic!("fail")
    }
}

fn get_label(label: &mut HashMap<String, usize>, k: &str) -> usize {
    match label.get(k) {
        Some(x) => *x,
        None => {
            let n = label.len();
            label.insert(k.to_string(), n);
            n
        }
    }
}

fn main() {
    let contents = std::fs::read_to_string("day-13/input.txt").unwrap();

    let re = regex::Regex::new(r"^(.*) would (.*) (.*) happiness units by sitting next to (.*)\.").unwrap();

    let tuples: Vec<(String,String,i32)> = contents.lines().map(|l| parse_line(&re, &l)).collect();
    println!("{:?}", tuples);

    let mut label = HashMap::new();
    for (a,b,_w) in tuples.iter() {
        get_label(&mut label, &a);
        get_label(&mut label, &b);
    }
    let mut g = Graph::new(label.len());
    for (a,b,w) in tuples.iter() {
        let an = *label.get(a).unwrap();
        let bn = *label.get(b).unwrap();
        *g.get_mut(an, bn) += *w;
        *g.get_mut(bn, an) += *w;
    }
    println!("Part 1: {}", heaviest_hamiltonian_cycle(&g));

    // Now add a new person whose weights are all zero.
    // Just the +1 on the graph size is enough; the default is all zero.
    let mut g = Graph::new(label.len()+1);
    for (a,b,w) in tuples.iter() {
        let an = *label.get(a).unwrap();
        let bn = *label.get(b).unwrap();
        *g.get_mut(an, bn) += *w;
        *g.get_mut(bn, an) += *w;
    }
    println!("Part 2: {}", heaviest_hamiltonian_cycle(&g));
    
}
    
