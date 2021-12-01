package advent2017;

import org.apache.commons.lang3.tuple.Pair;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;

public class Day13 {
    static Optional<Integer> severity(int delay, List<Pair<Integer,Integer>> setup) {
        int severity = 0;
        boolean caught = false;
        for (Pair<Integer, Integer> p: setup) {
            int depth = p.getLeft();
            int range = p.getRight();
            if ((depth+delay) % (2*range - 2) == 0) {
                caught = true;
                severity += depth * range;
            }
        }
        if (caught)
            return Optional.of(severity);
        else
            return Optional.empty();
    }
    public static void main(String[] args) throws Exception {
        int severity = 0;
        List<Pair<Integer,Integer>> setup =
                Files.lines(Paths.get("src/main/resources/2017/input-13.txt"))
                    .map(line -> {
                        String[] parts = line.split(": ");
                        int depth = Integer.parseInt(parts[0]);
                        int range = Integer.parseInt(parts[1]);
                        return Pair.of(depth, range);
                    }).toList();
        System.out.println(severity(0, setup));
        for (int delay = 0 ; ; delay++) {
            Optional<Integer> s = severity(delay, setup);
            if (s.isEmpty()) {
                System.out.println(delay);
                break;
            }
        }
    }
}
