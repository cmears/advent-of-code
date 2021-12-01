package advent2017;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Day21 {
    static Map<String, String> mapping = new HashMap<>();
    static class Grid {
        boolean[][] grid;
        static Grid fromString(String s) {
            Grid g = new Grid();
            String[] rows = s.split("/");
            int size = rows.length;
            g.grid = new boolean[size][size];
            for (int i = 0 ; i < size ; i++) {
                for (int j = 0 ; j < size ; j++) {
                    g.grid[i][j] = rows[i].charAt(j) == '#';
                }
            }
            return g;
        }
        public String toString() {
            int size = grid.length;
            StringBuilder b = new StringBuilder();
            for (int i = 0 ; i < size ; i++) {
                for (int j = 0 ; j < size ; j++) {
                    b.append(grid[i][j] ? '#' : '.');
                }
                if (i < size-1)
                    b.append('/');
            }
            return b.toString();
        }
        public Grid transpose() {
            Grid g = new Grid();
            int size = grid.length;
            g.grid = new boolean[size][size];
            for (int i = 0 ; i < size ; i++)
                for (int j = 0 ; j < size ; j++)
                    g.grid[i][j] = grid[j][i];
            return g;
        }
        public Grid mirror() {
            Grid g = new Grid();
            int size = grid.length;
            g.grid = new boolean[size][size];
            for (int i = 0 ; i < size ; i++)
                for (int j = 0 ; j < size ; j++)
                    g.grid[i][j] = grid[i][size-1-j];
            return g;
        }
        public Grid subgrid(int row, int col, int rows, int cols) {
            Grid g = new Grid();
            g.grid = new boolean[rows][cols];
            for (int i = 0 ; i < rows ; i++)
                for (int j = 0 ; j < cols ; j++)
                    g.grid[i][j] = grid[row+i][col+j];
            return g;
        }
        public void transplant(int row, int col, Grid g) {
            int size = g.grid.length;
            for (int i = 0 ; i < size ; i++)
                for (int j = 0 ; j < size ; j++)
                    grid[row+i][col+j] = g.grid[i][j];
        }
        public Grid expand() {
            int size = grid.length;
            Grid g = new Grid();
            if (size % 2 == 0) {
                g.grid = new boolean[size/2*3][size/2*3];
                for (int i = 0 ; i < size/2 ; i++) {
                    for (int j = 0 ; j < size/2 ; j++) {
                        Grid s = subgrid(2*i, 2*j, 2, 2);
                        Grid b = Grid.fromString(mapping.get(s.toString()));
                        g.transplant(3*i,3*j,b);
                    }
                }
            } else {
                g.grid = new boolean[size/3*4][size/3*4];
                for (int i = 0 ; i < size/3 ; i++) {
                    for (int j = 0 ; j < size/3 ; j++) {
                        Grid s = subgrid(3*i, 3*j, 3, 3);
                        Grid b = Grid.fromString(mapping.get(s.toString()));
                        g.transplant(4*i,4*j,b);
                    }
                }
            }
            return g;
        }
        public int onCount() {
            int nOn = 0;
            for (int i = 0; i < grid.length; i++)
                for (int j = 0; j < grid.length; j++)
                    nOn += grid[i][j] ? 1 : 0;
            return nOn;
        }
    }
    public static void main(String[] args) throws Exception {
        List<String> lines = Files.lines(Paths.get("src/main/resources/2017/input-21.txt")).toList();
        for (String line: lines) {
            String[] parts = line.split(" => ");
            Grid a = Grid.fromString(parts[0]);
            String b = parts[1];
            mapping.put(a.toString(), b);
            mapping.put(a.transpose().toString(), b);
            mapping.put(a.transpose().mirror().toString(), b);
            mapping.put(a.transpose().mirror().transpose().toString(), b);
            mapping.put(a.mirror().toString(), b);
            mapping.put(a.mirror().transpose().toString(), b);
            mapping.put(a.mirror().transpose().mirror().toString(), b);
            mapping.put(a.mirror().transpose().mirror().transpose().toString(), b);
        }
        Grid grid = Grid.fromString(".#./..#/###");
        System.out.println(grid.toString());
        for (int i = 0 ; i < 18 ; i++) {
            grid = grid.expand();
            if (i == 4 || i == 17) {
                System.out.println(grid.onCount());
            }
        }
    }
}
