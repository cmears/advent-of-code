fn main() {
    let input = std::fs::read_to_string("day-18/input.txt").unwrap();
    let lines:Vec<&str> = input.lines().collect();
    let rows = lines.len();
    let cols = lines[0].len();

    let mut grid:Vec<bool> = Vec::with_capacity(rows * cols);
    for r in 0..rows {
        for c in lines[r].as_bytes() {
            grid.push(c == &b'#');
        }
    }

    let grid2 = grid.clone();

    let f = |grid:&Vec<bool>,r,c| {
        if r < 0 || r >= (rows as i32) || c < 0 || c >= (cols as i32) {
            return 0
        }
        if grid[(r as usize)*cols+(c as usize)] { 1 } else { 0 }
    };

    for _ in 0..100 {
        let mut next:Vec<bool> = Vec::with_capacity(rows * cols);
        for r in 0..(rows as i32) {
            for c in 0..(cols as i32) {
                let mut x = 0;
                x += f(&grid,r-1,c-1);
                x += f(&grid,r-1,c);
                x += f(&grid,r-1,c+1);
                x += f(&grid,r,c-1);
                x += f(&grid,r,c+1);
                x += f(&grid,r+1,c-1);
                x += f(&grid,r+1,c);
                x += f(&grid,r+1,c+1);
                if x == 3 || (x == 2 && grid[(r as usize)*cols+(c as usize)]) {
                    next.push(true)
                } else {
                    next.push(false)
                }
            }
        }
        grid = next;
    }

    println!("{}", grid.iter().filter(|&b| *b).collect::<Vec<&bool>>().len());

    grid = grid2;

    for _ in 0..100 {
        let mut next:Vec<bool> = Vec::with_capacity(rows * cols);
        for r in 0..(rows as i32) {
            for c in 0..(cols as i32) {
                let mut x = 0;
                x += f(&grid,r-1,c-1);
                x += f(&grid,r-1,c);
                x += f(&grid,r-1,c+1);
                x += f(&grid,r,c-1);
                x += f(&grid,r,c+1);
                x += f(&grid,r+1,c-1);
                x += f(&grid,r+1,c);
                x += f(&grid,r+1,c+1);
                if x == 3 || (x == 2 && grid[(r as usize)*cols+(c as usize)]) {
                    next.push(true)
                } else {
                    next.push(false)
                }
            }
        }
        grid = next;
        grid[0*cols+0] = true;
        grid[(rows-1)*cols+0] = true;
        grid[0*cols+(cols-1)] = true;
        grid[(rows-1)*cols+(cols-1)] = true;
    }

    println!("{}", grid.iter().filter(|&b| *b).collect::<Vec<&bool>>().len());
}
