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
}

// Find the cost of the shortest Hamiltonian path in the graph.
// Not very efficient.
fn shortest_hamiltonian_path(g: &Graph) -> i32 {
    let mut best = i32::MAX;
    let mut stack = Vec::new();
    let mut visited = vec![false; g.n];

    fn f(g: &Graph, stack: &mut Vec<usize>, visited: &mut Vec<bool>, best: &mut i32, i: usize, cost: i32) {
        // Path is finished; check cost.
        if i == g.n {
            if cost < *best {
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

fn parse_line(s: &str) -> (String, String, i32) {
    match s.split_whitespace().collect::<Vec<&str>>()[..] {
        [a, "to", b, "=", n] => (a.to_string(), b.to_string(), n.parse().unwrap()),
        _ => panic!("!")
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
    let contents = std::fs::read_to_string("day-9/input.txt").unwrap();
    let tuples: Vec<(String,String,i32)> = contents.lines().map(parse_line).collect();
    let mut label = HashMap::new();
    for (a,b,_w) in tuples.iter() {
        get_label(&mut label, &a);
        get_label(&mut label, &b);
    }
    let mut g = Graph::new(label.len());
    for (a,b,w) in tuples.iter() {
        let an = *label.get(a).unwrap();
        let bn = *label.get(b).unwrap();
        g.insert(an, bn, *w);
        g.insert(bn, an, *w);
    }
    println!("Part 1: {}", shortest_hamiltonian_path(&g));

    // Part 2 asks for the longest path instead of the shortest.
    // We negate all the weights in the graph and do it again.
    for i in 0..g.n {
        for j in 0..g.n {
            g.insert(i, j, -g.get(i,j));
        }
    }
    println!("Part 2: {}", -shortest_hamiltonian_path(&g));
    
}
    
