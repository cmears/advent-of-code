package advent2017;

import java.nio.file.Files;
import java.nio.file.Paths;

public class Day1 {
    static int[] toArray(String s) {
        int[] xs = s.chars()
                .filter(Character::isDigit)
                .map(c -> Character.digit(c, 10))
                .toArray();
        return xs;
    }

    static int f1(int[] xs) {
        int total = 0;
        for (int i = 0 ; i < xs.length ; i++)
            if (xs[i] == xs[(i+1)%xs.length])
                total += xs[i];
        return total;
    }

    static int f2(int[] xs) {
        int total = 0;
        int n = xs.length;
        for (int i = 0 ; i < n ; i++)
            if (xs[i] == xs[(i+(n/2))%n])
                total += xs[i];
        return total;
    }

    public static void main(String[] args) throws Exception {
        String s = Files.readString(Paths.get("src/main/resources/2017/input-1.txt"));
        int[] xs = toArray(s);
        System.out.println(f1(xs));
        System.out.println(f2(xs));
    }
}
