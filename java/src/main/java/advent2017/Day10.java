package advent2017;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;

public class Day10 {
    static class KnotHash {
        static int[] hash(String input) {
            byte[] inputb = input.getBytes(StandardCharsets.UTF_8);
            ArrayList<Integer> input2 = new ArrayList<Integer>();
            for (byte b: inputb) { input2.add((int)b); }
            for (int i: new int[]{17, 31, 73, 47, 23}) { input2.add(i); }

            int n = 256;
            int[] x = new int[n];
            for (int i = 0; i < n; i++)
                x[i] = i;

            int current = 0;
            int skip = 0;
            for (int r = 0 ; r < 64 ; r++) {
                for (int l : input2) {
                    for (int i = 0; i < l / 2; i++) {
                        int t = x[(current + i) % n];
                        x[(current + i) % n] = x[(current + l - 1 - i) % n];
                        x[(current + l - 1 - i) % n] = t;
                    }
                    current = (current + l + skip) % n;
                    skip++;
                }
            }

            int[] dense = new int[16];
            for (int i = 0 ; i < 16 ; i++) {
                dense[i] = 0;
                for (int j = 0 ; j < 16 ; j++) {
                    dense[i] ^= x[16*i+j];
                }
            }
            return dense;
        }
    }

    public static void main(String[] args) {
        String input = "183,0,31,146,254,240,223,150,2,206,161,1,255,232,199,88";
        part1(input);
        part2(input);
    }

    static void part1(String input) {
        Integer[] input1 = Arrays.stream(input.split(",")).map(Integer::parseInt).toArray(Integer[]::new);
        int n = 256;
        int[] x = new int[n];
        for (int i = 0; i < n; i++)
            x[i] = i;

        int current = 0;
        int skip = 0;
        for (int l : input1) {
            for (int i = 0; i < l / 2; i++) {
                int t = x[(current + i) % n];
                x[(current + i) % n] = x[(current + l - 1 - i) % n];
                x[(current + l - 1 - i) % n] = t;
            }
            current = (current + l + skip) % n;
            skip++;
        }
        System.out.println(x[0] * x[1]);
    }

    static void part2(String input) {
        int[] dense = KnotHash.hash(input);
        for (int i = 0 ; i < 16 ; i++) {
            System.out.format("%02x", dense[i]);
        }
    }
}
