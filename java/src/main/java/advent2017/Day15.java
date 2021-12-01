package advent2017;

import java.util.Iterator;
import java.util.stream.Stream;

public class Day15 {
    static long startA = 618;
    static long startB = 814;
    public static void main(String[] args) {
        part1();
        part2();
    }

    static void part1() {
        Iterator<Long> a = Stream.iterate(startA, x -> (x*16807)%2147483647).iterator();
        Iterator<Long> b = Stream.iterate(startB, x -> (x*48271)%2147483647).iterator();
        int count = 0;
        for (int i = 0 ; i < 40000000 ; i++) {
            long x = a.next();
            long y = b.next();
            if ((x & 0xFFFF) == (y & 0xFFFF))
                count++;
        }
        System.out.println(count);
    }
    static void part2() {
        Iterator<Long> a = Stream.iterate(startA, x -> (x*16807)%2147483647)
                .filter(x -> x%4==0).iterator();
        Iterator<Long> b = Stream.iterate(startB, x -> (x*48271)%2147483647)
                .filter(x -> x%8==0).iterator();
        int count = 0;
        for (int i = 0 ; i < 5000000 ; i++) {
            long x = a.next();
            long y = b.next();
            if ((x & 0xFFFF) == (y & 0xFFFF))
                count++;
        }
        System.out.println(count);
    }
}
