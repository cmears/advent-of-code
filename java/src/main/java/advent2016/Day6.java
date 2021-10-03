package advent2016;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Day6 {
    public static void main(String[] args) throws Exception {
        ArrayList<Map<Character, Integer>> maps = new ArrayList<Map<Character, Integer>>();
        for (int i = 0 ; i < 8 ; i++) {
            maps.add(new HashMap<Character, Integer>());
        }
        Iterator<String> it = Files.lines(Paths.get("input-6.txt")).iterator();
        while (it.hasNext()) {
            String line = it.next();
            for (int i = 0 ; i < 8 ; i++) {
                char c = line.charAt(i);
                int n = maps.get(i).getOrDefault(c, 0);
                maps.get(i).put(c, n+1);
            }
        }
        String leastCommon = "";
        String mostCommon = "";
        for (int i = 0 ; i < 8 ; i++) {
            Set<Map.Entry<Character, Integer>> entrySet = maps.get(i).entrySet();
            ArrayList<Map.Entry<Character, Integer>> entries = new ArrayList<Map.Entry<Character, Integer>>(entrySet);
            Comparator<Map.Entry<Character, Integer>> co =
                    Comparator
                            .<Map.Entry<Character, Integer>>comparingInt(e -> e.getValue())
                            .reversed();
            entries.sort(co);
            mostCommon += entries.get(0).getKey();
            leastCommon += entries.get(entries.size()-1).getKey();
        }
        System.out.println(mostCommon);
        System.out.println(leastCommon);
    }
}
