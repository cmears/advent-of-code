package advent2017;

import org.apache.commons.lang3.tuple.Pair;

import java.util.HashMap;

import static java.lang.Math.min;

public class Day3 {
    static int input = 368078;

    public static void main(String[] args) {
        part1();
        part2();
    }

    public static void part1() {
        int n = input;
        int x = 1;
        while ((x + 2) * (x + 2) < n)
            x += 2;
        int ring = (x + 1) / 2;
        int sidelen = x + 1;
        int pos1 = (n - x * x) % sidelen;
        int pos2 = (pos1 + sidelen / 2) % sidelen;
        int pos3 = min(pos2, sidelen - pos2);
        int distance = ring + pos3;
        System.out.println(distance);
    }

    enum Dir { Up, Down, Left, Right };

    static Dir turnLeft(Dir dir) {
        return switch (dir) {
            case Up -> Dir.Left;
            case Left -> Dir.Down;
            case Down -> Dir.Right;
            case Right -> Dir.Up;
        };
    }

    static Pair<Integer, Integer> advance(Pair<Integer, Integer> p, Dir dir) {
        return switch (dir) {
            case Up -> Pair.of(p.getLeft(), p.getRight() + 1);
            case Down -> Pair.of(p.getLeft(), p.getRight() - 1);
            case Left -> Pair.of(p.getLeft() - 1, p.getRight());
            case Right -> Pair.of(p.getLeft() + 1, p.getRight());
        };
    }

    public static void part2() {
        HashMap<Pair<Integer, Integer>, Integer> grid = new HashMap<>();
        grid.put(Pair.of(0,0), 1);
        Pair<Integer, Integer> coord = Pair.of(1,0);
        Dir dir = Dir.Up;

        for(;;) {
            int x = coord.getLeft();
            int y = coord.getRight();
            int total =
                    grid.getOrDefault(Pair.of(x-1,y), 0) +
                    grid.getOrDefault(Pair.of(x+1,y), 0) +
                    grid.getOrDefault(Pair.of(x-1,y-1), 0) +
                    grid.getOrDefault(Pair.of(x-1,y+1), 0) +
                    grid.getOrDefault(Pair.of(x+1,y-1), 0) +
                    grid.getOrDefault(Pair.of(x+1,y+1), 0) +
                    grid.getOrDefault(Pair.of(x,y-1), 0) +
                    grid.getOrDefault(Pair.of(x,y+1), 0);
            grid.put(coord, total);
            if (total > input) {
                System.out.format("(%d,%d) -> %d\n", x, y, total);
                break;
            }

            Pair<Integer, Integer> leftHand = advance(coord, turnLeft(dir));
            if (!grid.containsKey(leftHand)) {
                dir = turnLeft(dir);
            }
            coord = advance(coord, dir);
        }
    }
}
