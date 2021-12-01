package advent2017;

import java.nio.file.Files;
import java.nio.file.Paths;

public class Day5 {
    public static void main(String[] args) throws Exception {
        Integer[] a = Files.lines(Paths.get("src/main/resources/2017/input-5.txt"))
                .map(Integer::parseInt)
                .toArray(Integer[]::new);
        boolean part2 = true;
        int i = 0;
        int steps = 0;
        while (true) {
            if (i < 0 || i >= a.length)
                break;
            int j = i + a[i];
            if (part2 && a[i] >= 3)
                a[i]--;
            else
                a[i]++;
            i = j;
            steps++;
        }
        System.out.println(steps);
    }
}
