package advent2016;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Comparator;
import java.util.concurrent.atomic.AtomicInteger;

public class Day4 {
    public static void main(String[] args) throws Exception {
        AtomicInteger n = new AtomicInteger();
        Files.lines(Paths.get("input-4.txt")).forEach(line -> {
            String[] parts = line.split("\\[");
            String[] words = parts[0].split("-");
            Map<Character, Integer> m = new HashMap<Character, Integer>();
            for (int i = 0 ; i < words.length - 1 ; i++) {
                for (int j = 0 ; j < words[i].length() ; j++) {
                    int x = m.getOrDefault(words[i].charAt(j), 0);
                    m.put(words[i].charAt(j), x+1);
                }
            }
            ArrayList<Map.Entry<Character, Integer>> entries = new ArrayList(m.entrySet());
            Comparator<Map.Entry<Character, Integer>> co =
                    Comparator
                            .<Map.Entry<Character, Integer>>comparingInt(e -> e.getValue())
                            .reversed()
                            .thenComparing(e -> e.getKey());
            entries.sort(co);
            String claimedChecksum = parts[1].substring(0, 5);
            String checksum = "";
            for (int i = 0 ; i < 5 ; i++) {
                checksum += entries.get(i).getKey();
            }
            //System.out.println(line + " " + claimedChecksum + " " + checksum);

            int sector = Integer.parseInt(words[words.length - 1]);

            if (claimedChecksum.equals(checksum)) {
                n.addAndGet(sector);
            }

            String plain = "";
            for (int i = 0 ; i < words.length - 1 ; i++) {
                for (char c: words[i].toCharArray()) {
                    int d = Character.digit(c, 36) - 10;
                    d = (d + sector) % 26;
                    plain += Character.forDigit(d+10, 36);
                }
                plain += ' ';
            }
            if (plain.contains("north"))
                System.out.println(sector + " " + plain);

        });
        System.out.println(n.get());
    }
}
