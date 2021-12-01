package advent2017;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.stream.Collectors;

import static java.lang.Math.max;
import static java.lang.Math.min;

public class Day2 {
    public static ImmutablePair<Integer, Integer> bounds(Integer[] a) {
        int min = a[0];
        int max = a[0];
        for (int x: a) {
            min = min(min, x);
            max = max(max, x);
        }
        return ImmutablePair.of(min, max);
    }
    public static int divis(Integer[] a) {
        int n = a.length;
        for (int i = 0 ; i < n ; i++)
            for (int j = 0 ; j < n ; j++)
                if (i != j && a[j] % a[i] == 0)
                    return a[j]/a[i];
        return -1;
    }
    public static void main(String[] args) throws Exception {
        Integer[][] rows = Files.lines(Paths.get("src/main/resources/2017/input-2.txt"))
                .map(s -> s.split("\\s+"))
                .map(a -> Arrays.stream(a).map(Integer::parseInt).toArray(Integer[]::new))
                .toArray(Integer[][]::new);
        int checksum1 = Arrays.stream(rows)
                .map(Day2::bounds)
                .mapToInt(p -> p.right - p.left)
                .sum();
        System.out.println(checksum1);
        int checksum2 = Arrays.stream(rows)
                .mapToInt(Day2::divis)
                .sum();
        System.out.println(checksum2);
    }
}
