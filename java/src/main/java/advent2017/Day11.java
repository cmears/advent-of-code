package advent2017;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.stream.Stream;

import static java.lang.Math.abs;
import static java.lang.Math.max;

public class Day11 {
    static class Coord {
        int x = 0;
        int y = 0;
        int z = 0;
        void step(String direction) {
            switch (direction) {
                case "n": y++; z--; break;
                case "ne": x++; z--; break;
                case "se": x++; y--; break;
                case "s": y--; z++; break;
                case "sw": x--; z++; break;
                case "nw": x--; y++; break;
            }
        }
        int distance() {
            return (abs(x) + abs(y) + abs(z)) / 2;
        }

    }
    public static void main(String[] args) throws Exception {
        String input = Files.readString(Paths.get("src/main/resources/2017/input-11.txt"));
        Stream<String> steps = Arrays.stream(input.split(",")).map(String::trim);
        Coord c = new Coord();
        int furthest = 0;
        for (String step: steps.toList()) {
            c.step(step);
            furthest = max(furthest, c.distance());
        }
        System.out.println(c.distance());
        System.out.println(furthest);
    }
}
