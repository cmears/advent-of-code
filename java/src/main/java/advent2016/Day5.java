package advent2016;

import java.math.BigInteger;
import java.security.MessageDigest;
import java.util.Iterator;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Day5 {
    public static String md5(String input) {
        try {
            byte[] digest = MessageDigest.getInstance("MD5").digest(input.getBytes());
            return String.format("%032x", new BigInteger(1, digest));
        } catch(Exception e) { return null; }
    }
    public static void main(String[] args) throws Exception {
        IntStream naturals = IntStream.iterate(0, i -> i + 1);
        String input = "abbhdwsy";
        Stream<String> hashInputs = naturals.mapToObj(i -> input + i);
        Stream<String> hashOutputs = hashInputs.map(Day5::md5);
        Stream<String> passwordHashes = hashOutputs.filter(s -> s.substring(0,5).equals("00000"));

        String part1 = "";
        char[] part2 = "________".toCharArray();
        for (Iterator<String> it = passwordHashes.iterator(); it.hasNext(); ) {
            String hash = it.next();
            if (part1.length() < 8) {
                part1 += hash.substring(5, 6);
                if (part1.length() == 8) {
                    System.out.println(part1);
                }
            }

            int pos = Integer.parseInt(hash.substring(5,6), 16);
            if (pos < 8 && part2[pos] == '_') {
                part2[pos] = hash.charAt(6);
                System.out.println(part2);
                if (!new String(part2).contains("_"))
                    break;
            }
        };
    }
}
