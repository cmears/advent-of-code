package advent2017;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public class Day4 {
    static boolean valid1(String line) {
        String[] words = line.split("\\s+");
        HashSet<String> set = new HashSet<>(Arrays.asList(words));
        return set.size() == words.length;
    }
    static String sortString(String s) {
        int[] a = s.chars().sorted().toArray();
        return new String(a, 0, a.length);
    }
    static boolean valid2(String line) {
        String[] words = line.split("\\s+");
        Set<String> set = Arrays.stream(words).map(Day4::sortString).collect(Collectors.toSet());
        return set.size() == words.length;
    }
    public static void main(String[] args) throws Exception {
        long n1 = Files.lines(Paths.get("src/main/resources/2017/input-4.txt"))
                .filter(Day4::valid1)
                .count();
        System.out.println(n1);
        long n2 = Files.lines(Paths.get("src/main/resources/2017/input-4.txt"))
                .filter(Day4::valid2)
                .count();
        System.out.println(n2);
    }
}
