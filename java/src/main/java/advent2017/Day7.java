package advent2017;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Day7 {
    static class Structure {
        Map<String, String> parent = new HashMap<>();
        Map<String, List<String>> children = new HashMap();
        Map<String, Integer> weight = new HashMap<>();
        Map<String, Integer> fullWeight = new HashMap<>();
        void update(String line) {
            String[] parts = line.split("\\s+");
            ArrayList<String> cs = new ArrayList<>();
            for (int i = 3 ; i < parts.length ; i++) {
                String child = parts[i].replaceAll(",", "");
                parent.put(child, parts[0]);
                cs.add(child);
            }
            children.put(parts[0], cs);
            weight.put(parts[0], Integer.parseInt(parts[1].replaceAll("[()]", "")));
        }
        Integer fullWeight(String s) {
            if (!fullWeight.containsKey(s)) {
                int total = weight.get(s);
                for (String c: children.get(s)) {
                    total += fullWeight(c);
                }
                fullWeight.put(s, total);
            }
            return fullWeight.get(s);
        }
    }
    public static void main(String[] args) throws Exception {
        Structure structure = new Structure();
        Files.lines(Paths.get("src/main/resources/2017/input-7.txt"))
                .forEach(structure::update);
        String s = structure.parent.keySet().stream().findAny().get();
        while (structure.parent.containsKey(s)) {
            s = structure.parent.get(s);
        }
        System.out.println(s);

        loop: for (String x: structure.weight.keySet()) {
            List<String> cs = structure.children.get(x);
            if (cs.size() > 0) {
                int w = structure.fullWeight(cs.get(0));
                for (int i = 1 ; i < cs.size() ; i++) {
                    if (structure.fullWeight(cs.get(i)) != w) {
                        System.out.format("unbalanced (%s): %d vs %d\n", x, w, structure.fullWeight(cs.get(i)));
                        for (int j = 0 ; j < cs.size() ; j++) {
                            System.out.format("%9s %6d %6d\n", cs.get(j), structure.weight.get(cs.get(j)), structure.fullWeight(cs.get(j)));
                        }
                        continue loop;
                    }
                }
            }
        }
    }
}
