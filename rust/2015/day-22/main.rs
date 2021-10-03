use std::rc::Rc;

#[derive(Clone, PartialEq, Debug)]
enum Combatant { Player, Boss }
use Combatant::*;

// Game state (not search state)
#[derive(Clone, Debug)]
struct State {
    // Combatant stats
    player_health: i32,
    boss_health: i32,
    player_mana: i32,
    boss_damage: i32,
    // Whose turn it is
    turn: Combatant,
    // Timers
    poison: i32,
    shield: i32,
    recharge: i32,
    // Difficulty setting
    hard_mode: bool
}

impl State {
    fn new(boss_health: i32, boss_damage: i32, hard_mode: bool) -> State {
        State {
            player_health: 50,
            boss_health,
            player_mana: 500,
            boss_damage,
            turn: Player,
            poison: 0,
            shield: 0,
            recharge: 0,
            hard_mode
        }
    }
}

// Node in the search graph
struct Node {
    // Game state
    state: State,
    // Cost to reach this state
    cost: i32,
    // Edge label and previous node pointer (to reconstruct the path)
    label: &'static str,
    prev: Option<Rc<Node>>
}

fn is_final_state(state: &State) -> bool {
    state.boss_health <= 0
}

fn search(init_state: State) -> Rc<Node> {
    let mut queue = Vec::new();
    let mut best = i32::MAX;
    let mut best_node = None;
    let init_node = Node{state: init_state, cost: 0, label: "start", prev: None};
    queue.push(Rc::new(init_node));
    while let Some(node) = queue.pop() {
        if node.cost >= best { continue }
        if is_final_state(&node.state) {
            best = node.cost;
            best_node = Some(node);
            continue;
        }
        let ns = next_states(&node.state);
        for (next_state, edge_cost, label) in ns {
            queue.push(Rc::new(Node{state: next_state, cost: node.cost + edge_cost, label, prev: Some(node.clone())}));
        }
    }
    best_node.unwrap()
}

fn next_states(state: &State) -> Vec<(State, i32, &'static str)> {
    let mut vec = Vec::new();
    // Apply effects
    let mut s = state.clone();
    let mut player_armor = 0;
    if s.poison > 0 { s.boss_health -= 3; s.poison -= 1 }
    if s.shield > 0 { player_armor = 7; s.shield -= 1 }
    if s.recharge > 0 { s.player_mana += 101; s.recharge -= 1 }

    if s.hard_mode && s.turn == Player { s.player_health -= 1 }
    
    // If player is dead, we go nowhere from here.
    if s.player_health <= 0 { return vec; }
    // If the boss is dead (because of the effects), we have a "free
    // move" to a victory state.
    if s.boss_health <= 0 { vec.push((s,0,"done")); return vec; }

    match state.turn {
        Boss => {
            s.turn = Player;
            let dam = std::cmp::max(1, s.boss_damage - player_armor);
            s.player_health -= dam;
            vec.push((s,0,"boss"));
        }
        Player => {
            s.turn = Boss;
            // Magic missile
            if s.player_mana >= 53 {
                let mut s = s.clone();
                s.player_mana -= 53;
                s.boss_health -= 4;
                vec.push((s,53,"magic missile"));
            }
            // Drain
            if s.player_mana >= 73 {
                let mut s = s.clone();
                s.player_mana -= 73;
                s.player_health += 2;
                s.boss_health -= 2;
                vec.push((s,73,"drain"));
            }
            // Shield
            if s.player_mana >= 113 && s.shield == 0 {
                let mut s = s.clone();
                s.player_mana -= 113;
                s.shield = 6;
                vec.push((s,113,"shield"));
            }
            // Poison
            if s.player_mana >= 173 && s.poison == 0 {
                let mut s = s.clone();
                s.player_mana -= 173;
                s.poison = 6;
                vec.push((s,173,"poison"));
            }
            // Recharge
            if s.player_mana >= 229 && s.recharge == 0 {
                let mut s = s.clone();
                s.player_mana -= 229;
                s.recharge = 5;
                vec.push((s,229,"recharge"));
            }
        }
    }
    vec
}

fn explain(node: &Node) {
    let mut stack = Vec::new();
    let mut curr = node;
    stack.push(curr);
    while let Some(p) = &curr.prev {
        curr = p;
        stack.push(curr);
    }
    while let Some(node) = stack.pop() {
        if node.label != "boss" { println!("{}", node.label) }
        println!("    {}/{}  {}", node.state.player_health, node.state.player_mana, node.state.boss_health);
    }
    println!("{}", node.cost);
}

fn main() {
    explain(&search(State::new(71,10,false)));
    explain(&search(State::new(71,10,true)));
}
