package advent2017;

import org.apache.commons.lang3.tuple.Pair;

import java.util.Stack;

public class Day14 {
    static String key = "vbqugkhl";
    public static void main(String[] args) {
        int ones = 0;
        boolean[][] grid = new boolean[128][128];
        for (int i = 0 ; i < 128 ; i++) {
            int[] dense = Day10.KnotHash.hash(String.format("%s-%d", key, i));
            for (int j = 0 ; j < 16 ; j++) {
                for (int k = 0 ; k < 8 ; k++) {
                    int bit = (dense[j] >> k) & 0x1;
                    grid[i][8*j+7-k] = (bit == 1);
                    ones += bit;
                }
            }
        }
        System.out.println(ones);

//        for (int i = 0 ; i < 128 ; i++) {
//            for (int j = 0; j < 128; j++) {
//                System.out.print(grid[i][j] ? '#' : '.');
//            }
//            System.out.println();
//        }

        int regions = 0;
        for (int i = 0 ; i < 128 ; i++) {
            for (int j = 0 ; j < 128 ; j++) {
                if (grid[i][j] == false)
                    continue;
                regions++;
                Stack<Pair<Integer, Integer>> stack = new Stack<>();
                stack.push(Pair.of(i,j));
                while (!stack.empty()) {
                    Pair<Integer, Integer> p = stack.pop();
                    int x = p.getLeft();
                    int y = p.getRight();
                    if (grid[x][y] == false)
                        continue;
                    grid[x][y] = false;
                    if (x > 0) stack.push(Pair.of(x-1, y));
                    if (x < 127) stack.push(Pair.of(x+1,y));
                    if (y > 0) stack.push(Pair.of(x, y-1));
                    if (y < 127) stack.push(Pair.of(x,y+1));
                }
            }
        }
        System.out.println(regions);
    }
}
