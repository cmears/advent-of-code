use std::collections::HashSet;

const INPUT: &str = r"Al => ThF
Al => ThRnFAr
B => BCa
B => TiB
B => TiRnFAr
Ca => CaCa
Ca => PB
Ca => PRnFAr
Ca => SiRnFYFAr
Ca => SiRnMgAr
Ca => SiTh
F => CaF
F => PMg
F => SiAl
H => CRnAlAr
H => CRnFYFYFAr
H => CRnFYMgAr
H => CRnMgYFAr
H => HCa
H => NRnFYFAr
H => NRnMgAr
H => NTh
H => OB
H => ORnFAr
Mg => BF
Mg => TiMg
N => CRnFAr
N => HSi
O => CRnFYFAr
O => CRnMgAr
O => HP
O => NRnFAr
O => OTi
P => CaP
P => PTi
P => SiRnFAr
Si => CaSi
Th => ThCa
Ti => BP
Ti => TiTi
e => HF
e => NAl
e => OMg
";

fn main () {
    let replacements:Vec<(&str,&str)> = INPUT.lines().map(|l| {
        let words: Vec<&str> = l.split_whitespace().collect();
        (words[0], words[2])
    }).collect();

    let base = "CRnCaSiRnBSiRnFArTiBPTiTiBFArPBCaSiThSiRnTiBPBPMgArCaSiRnTiMgArCaSiThCaSiRnFArRnSiRnFArTiTiBFArCaCaSiRnSiThCaCaSiRnMgArFYSiRnFYCaFArSiThCaSiThPBPTiMgArCaPRnSiAlArPBCaCaSiRnFYSiThCaRnFArArCaCaSiRnPBSiRnFArMgYCaCaCaCaSiThCaCaSiAlArCaCaSiRnPBSiAlArBCaCaCaCaSiThCaPBSiThPBPBCaSiRnFYFArSiThCaSiRnFArBCaCaSiRnFYFArSiThCaPBSiThCaSiRnPMgArRnFArPTiBCaPRnFArCaCaCaCaSiRnCaCaSiRnFYFArFArBCaSiThFArThSiThSiRnTiRnPMgArFArCaSiThCaPBCaSiRnBFArCaCaPRnCaCaPMgArSiRnFYFArCaSiThRnPBPMgAr";

    // Part 1
    let mut results = HashSet::new();

    for r in replacements.iter() {
        execute(*r, base, &mut results);
    }

    println!("Distinct results: {}", results.len());

    // Part 2
    let tokens = tokenize(base);
    let mut productions = make_productions(replacements);

    // Add a top-level production.
    productions.push(Production { source: "γ", tokens: vec!["e"] });

    // Earley parser.
    let n = tokens.len();
    let mut state_set = Vec::new();
    for _ in 0..n+1 {
        state_set.push(HashSet::new())
    }

    state_set[0].insert(State { production_index: productions.len()-1,
                                position: 0,
                                origin: 0,
                                evidence: Vec::new() });

    println!("There are {} tokens", n);
    for k in 0..n+1 {
        // We must process every state in state_set[k], *including the
        // ones we add as we go along.
        let mut queue = Vec::new();
        for s in state_set[k].iter() {
            queue.push(s.clone())
        }
        let mut to_be_added = HashSet::new();
        let mut next_set = Vec::new();

        while let Some(s) = queue.pop() {
            let p = &productions[s.production_index];
            // If finished...
            if s.position == p.tokens.len() {
                for s2 in state_set[s.origin].iter() {
                    // We're completing e.g. B -> xyz·, starting at some origin.
                    // i.e. the production covers the range [origin..k]
                    //
                    // So look for A -> α·Bβ back at the origin and move the dot along
                    // In other words, if the production A -> α·Bβ was at that position at the origin,
                    // this completed production can move it along to αB·β
                    let p2 = &productions[s2.production_index];
                    if s2.position < p2.tokens.len() && p2.tokens[s2.position] == p.source {
                        let mut new_evidence = s2.evidence.clone();
                        new_evidence.push(s.origin);
                        let new_s = State {
                            production_index: s2.production_index,
                            position: s2.position + 1,
                            origin: s2.origin,
                            evidence: new_evidence
                        };
                        if !to_be_added.contains(&new_s) {
                            queue.push(new_s.clone());
                            to_be_added.insert(new_s);
                        }
                    }
                }
            } else {
                let next_token = p.tokens[s.position];
                // Predictor
                for pi in 0..productions.len() {
                    let p2 = &productions[pi];
                    if p2.source == next_token {
                        let new_s = State {
                            production_index: pi,
                            position: 0,
                            origin: k,
                            evidence: Vec::new()
                        };
                        if !to_be_added.contains(&new_s) {
                            queue.push(new_s.clone());
                            to_be_added.insert(new_s);
                        }
                    }
                }
                // Scanner
                if next_token == tokens[k] {
                    let mut new_evidence = s.evidence.clone();
                    new_evidence.push(k);
                    next_set.push(State {
                        production_index: s.production_index,
                        position: s.position + 1,
                        origin: s.origin,
                        evidence: new_evidence
                    });
                }
            }
        }
        for s in to_be_added {
            state_set[k].insert(s);
        }
        for s in next_set {
            state_set[k+1].insert(s);
        }
    }

    let mut top_state = None;
    
    for s in state_set[n].iter() {
        if s.production_index == productions.len()-1 {
            println!("{}", disp_state(s, &productions[s.production_index]));
            top_state = Some(s);
        }
    }

    let mut production_count = 0;
    explain_state(n, top_state.unwrap(), &state_set, &productions, &tokens, 0, &mut production_count);

    // Subtract the artificial γ → e production
    production_count -= 1;
    
    println!("Productions: {}", production_count);
}

fn explain_state(k: usize, s: &State, ss: &Vec<HashSet<State>>, ps: &Vec<Production>, tokens: &Vec<&str>, depth: usize, production_count: &mut i32) {
    let p = &ps[s.production_index];
    let indent = " ".repeat(4*depth);
    println!("{}{}", indent, disp_state(s,p));
    let mut evidence = s.evidence.clone();
    evidence.push(k);
    *production_count += 1;
    for i in 0..p.tokens.len() {
        // Base case: if it's a literal token (and a 1-length match), search no further.
        if p.tokens[i] == tokens[evidence[i]] && evidence[i+1] == evidence[i]+1 {
            println!("{}found literal {}", indent, p.tokens[i]);
        } else {
            // Otherwise, recursively find the explanation of each token in the production.
            let s2 = find_state(ss, ps, p.tokens[i], evidence[i], evidence[i+1]);
            explain_state(evidence[i+1], s2, ss, ps, tokens, depth+1, production_count);
        }
    }
}

fn find_state<'a>(ss: &'a Vec<HashSet<State>>, ps: &'a Vec<Production>, tok: &'a str, start: usize, finish: usize) -> &'a State {
    for s in ss[finish].iter() {
        let p = &ps[s.production_index];
        if s.origin == start && s.position == p.tokens.len() && p.source == tok {
            return s
        }
    }
    panic!("no state found");
}

fn disp_state<'a>(s: &State, p: &Production<'a>) -> String {
    let mut out = String::new();
    out.push_str(&format!("{} -> ", p.source));
    for i in 0..p.tokens.len() {
        if s.position == i {
            out.push_str("·");
        }
        out.push_str(p.tokens[i]);
    }
    if s.position == p.tokens.len() {
        out.push_str("·");
    }
    out.push_str(&format!(" ({})", s.origin));
    out.push_str(&format!(" ({:?})", s.evidence));
    out
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
struct State {
    production_index: usize,
    position: usize,
    origin: usize,
    // evidence[i] is the start of the explanation of the ith token in the production.
    // So evidence[0] == origin
    // The first token in the production represents tokens [evidence[0]..evidence[1]]
    // The second token is [evidence[1]..evidence[2]]
    // and so on.
    // We can use this (inefficient) information to reconstruct the parse tree.
    evidence: Vec<usize>
}

fn make_productions<'a>(replacements: Vec<(&'a str,&'a str)>) -> Vec<Production<'a>> {
    let mut productions = Vec::new();
    for r in replacements.iter() {
        let (source, destination) = r;
        productions.push(Production { source, tokens: tokenize(destination) });
    }
    productions
}

#[derive(Debug)]
struct Production<'a> {
    source: &'a str,
    tokens: Vec<&'a str>
}

fn execute(replacement: (&str,&str), base: &str, results: &mut HashSet<String>) {
    let (pattern,to) = replacement;
    let mut i = 0;
    let n = pattern.len();
    loop {
        match base[i..].find(pattern) {
            None => break,
            // Remember the j is relative to the i offset.
            Some(j) => {
                let mut s = base.to_string();
                s.replace_range(i+j..i+j+n, to);
                results.insert(s);
                i = i+j+n;
            }
        }
    }
}

// E.g. TiCSi => "Ti", "C", "Si"
fn tokenize(input: &str) -> Vec<&str> {
    let mut i = 0;
    let mut tokens = Vec::new();
    let bytes = input.as_bytes();
    while i < bytes.len() {
        // Assume we're looking at an uppercase letter.
        // If the next letter is uppercase, token is length 1.
        // (Or if there's only one letter left.)
        // Else it's length 2.
        let len;
        if i+1 == bytes.len() || (bytes[i+1] as char).is_uppercase() {
            len = 1;
        } else {
            len = 2;
        }
        tokens.push(&input[i..i+len]);
        i += len;
    }
    tokens
}
