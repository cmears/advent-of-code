package advent2016;

import org.apache.commons.lang3.tuple.ImmutablePair;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Day24 {
    record Coord(int row, int col) {}

    public static void main(String[] args) throws Exception {
        List<String> lines = Files.readAllLines(Paths.get("src/main/resources/input-24.txt"));
        int nrows = lines.size();
        int ncols = lines.get(0).length();
        boolean[][] grid = new boolean[nrows][ncols];

        HashMap<Coord, Integer> markers = new HashMap<>();
        for (int row = 0 ; row < nrows ; row++) {
            for (int col = 0; col < ncols; col++) {
                char c = lines.get(row).charAt(col);
                grid[row][col] = (c == '#');
                if (Character.isDigit(c)) {
                    int x = Character.digit(c, 10);
                    markers.put(new Coord(row,col), x);
                }
            }
        }

        for (int row = 0 ; row < nrows ; row++) {
            loop: for (int col = 0 ; col < ncols ; col++) {
                int marker = markers.getOrDefault(new Coord(row,col), -1);
                if (marker != -1) {
                    System.out.print(marker);
                    continue loop;
                }
                System.out.print(grid[row][col] ? '#' : ' ');
            }
            System.out.println();
        }

        int nmarkers = markers.size();
        Coord[] marker = new Coord[nmarkers];
        for (Map.Entry<Coord, Integer> entry: markers.entrySet()) {
            marker[entry.getValue()] = entry.getKey();
        }
        int[][] distance = new int[nmarkers][nmarkers];

        for (int i = 0 ; i < nmarkers ; i++) {
            Queue<ImmutablePair<Coord, Integer>> queue = new ArrayDeque<>();
            Set<Coord> seen = new HashSet<>();
            queue.add(ImmutablePair.of(marker[i], 0));
            while (!queue.isEmpty()) {
                ImmutablePair<Coord, Integer> p = queue.remove();
                if (seen.contains(p.left))
                    continue;
                seen.add(p.left);
                if (markers.containsKey(p.left)) {
                    int m = markers.get(p.left);
                    distance[i][m] = p.right;
                }
                if (!grid[p.left.row][p.left.col-1])
                    queue.add(ImmutablePair.of(new Coord(p.left.row, p.left.col-1), p.right+1));
                if (!grid[p.left.row][p.left.col+1])
                    queue.add(ImmutablePair.of(new Coord(p.left.row, p.left.col+1), p.right+1));
                if (!grid[p.left.row-1][p.left.col])
                    queue.add(ImmutablePair.of(new Coord(p.left.row-1, p.left.col), p.right+1));
                if (!grid[p.left.row+1][p.left.col])
                    queue.add(ImmutablePair.of(new Coord(p.left.row+1, p.left.col), p.right+1));
            }
        }

        for (int i = 0 ; i < nmarkers ; i++) {
            for (int j = 0; j < nmarkers; j++) {
                System.out.format("%5d", distance[i][j]);
            }
            System.out.println();
        }

        Stack<ArrayList<Integer>> stack = new Stack<>();
        ArrayList<Integer> initial = new ArrayList<>();
        initial.add(0);
        stack.push(initial);
        int best = -1;

        while (!stack.empty()) {
            ArrayList<Integer> path = stack.pop();
            if (path.size() == nmarkers) {
                int cost = 0;
                for (int i = 0; i < nmarkers - 1; i++)
                    cost += distance[path.get(i)][path.get(i + 1)];
                cost += distance[path.get(nmarkers-1)][0];
                if (best == -1 || cost < best) {
                    System.out.format("%5d  %s\n", cost, path);
                    best = cost;
                }
            } else {
                for (int j = nmarkers - 1; j >= 0; j--) {
                    if (!path.contains(j)) {
                        ArrayList<Integer> path2 = (ArrayList<Integer>) path.clone();
                        path2.add(j);
                        stack.push(path2);
                    }
                }
            }
        }
    }
}
