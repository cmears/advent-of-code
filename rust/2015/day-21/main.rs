// (cost, damage, armor)
type Item = (i32,i32,i32);

fn choose(items: &Vec<Item>, min: usize, max: usize) -> Vec<Item> {
    let mut result = Vec::new();
    let mut stack = Vec::new();
    stack.push((0,0,(0,0,0)));
    while let Some((i, n, (c,d,a))) = stack.pop() {
        if i == items.len() {
            if n >= min {
                result.push((c,d,a));
            }
        } else {
            // Buy the ith item.
            if n < max {
                let (ic,id,ia) = &items[i];
                stack.push((i+1,n+1,(c+ic,d+id,a+ia)));
            }
            // Don't buy the item.
            stack.push((i+1,n,(c,d,a)));
        }
    }
    result
}

// (health, damage, armor)
type Player = (i32,i32,i32);

// Fight until one player has <= 0 health.
fn fight(p1: &mut Player, p2: &mut Player) {
    if p1.0 <= 0 || p2.0 <= 0 { return }

    let p1hit = std::cmp::max(1, p1.1 - p2.2);
    let p2hit = std::cmp::max(1, p2.1 - p1.2);
    
    loop {
        p2.0 -= p1hit;
        if p2.0 <= 0 { return };
        p1.0 -= p2hit;
        if p1.0 <= 0 { return };
    }
}

fn main() {
    let weapons:Vec<Item> = vec![(8,4,0), (10,5,0), (25,6,0), (40,7,0), (74,8,0)];
    let armor:Vec<Item> = vec![(13,0,1), (31,0,2), (53,0,3), (75,0,4), (102,0,5)];
    let rings:Vec<Item> = vec![(25,1,0), (50,2,0), (100,3,0), (20,0,1), (40,0,2), (80,0,3)];

    let boss = (104,8,1);

    // Least gold to win the fight.
    let mut best = i32::MAX;
    
    for w in choose(&weapons, 1, 1) {
        for a in choose(&armor, 0, 1) {
            for r in choose(&rings, 0, 2) {
                let cost = w.0+a.0+r.0;
                if cost >= best { continue }
                let mut player = (100, w.1+a.1+r.1, w.2+a.2+r.2);
                let mut boss = boss.clone();
                fight(&mut player, &mut boss);
                if boss.0 <= 0 {
                    best = cost;
                }
            }
        }
    }

    println!("{}", best);

    // Most gold to lose the fight.
    let mut best = 0;
    
    for w in choose(&weapons, 1, 1) {
        for a in choose(&armor, 0, 1) {
            for r in choose(&rings, 0, 2) {
                let cost = w.0+a.0+r.0;
                if cost <= best { continue }
                let mut player = (100, w.1+a.1+r.1, w.2+a.2+r.2);
                let mut boss = boss.clone();
                fight(&mut player, &mut boss);
                if player.0 <= 0 {
                    best = cost;
                }
            }
        }
    }

    println!("{}", best);


}



