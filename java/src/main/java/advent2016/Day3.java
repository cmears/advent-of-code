package advent2016;

import java.io.BufferedReader;
import java.io.FileReader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Day3 {
    static int[] readLine(String line) {
        String[] words = line.trim().split(" +");
        IntStream numbers = Arrays.stream(words).mapToInt(Integer::parseInt);
        return numbers.toArray();
    }

    public static void part1() throws Exception {
        Stream<String> s = Files.lines(Paths.get("input-3.txt"));
        Stream<Boolean> b = s.map(line -> {
            int[] numbers = readLine(line);
            Arrays.sort(numbers);
            return numbers[0] + numbers[1] > numbers[2];
        });
        long n = b.filter(i -> i).count();
        System.out.println(n);
    }

    public static void part2() throws Exception {
        BufferedReader r = new BufferedReader(new FileReader("input-3.txt"));
        int n = 0;
        while (true) {
            String as = r.readLine();
            if (as == null) break;
            int[] a = readLine(as);
            int[] b = readLine(r.readLine());
            int[] c = readLine(r.readLine());
            for (int i = 0 ; i < 3 ; i++) {
                int[] numbers = { a[i], b[i], c[i] };
                Arrays.sort(numbers);
                n += (numbers[0] + numbers[1] > numbers[2]) ? 1 : 0;
            }
        }
        System.out.println(n);
    }

    public static void main(String[] args) throws Exception {
        part1();
        part2();
    }
}