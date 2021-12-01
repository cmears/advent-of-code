package advent2017;

import org.apache.commons.lang3.tuple.ImmutablePair;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Day24 {
    static int strongest = 0;
    static int longest = 0;
    static List<ImmutablePair<Integer,Integer>> components;
    static void f(int face, HashSet<Integer> used, int strength) {
        if (used.size() > longest || (used.size() == longest && strength > strongest)) {
            strongest = strength;
            longest = used.size();
        }
        for (int i = 0 ; i < components.size() ; i++) {
            if (!used.contains(i)) {
                ImmutablePair<Integer, Integer> p = components.get(i);
                if (p.left == face) {
                    HashSet<Integer> s = new HashSet<>(used);
                    s.add(i);
                    f(p.right, s, strength + p.left + p.right);
                }
                else if (p.right == face) {
                    HashSet<Integer> s = new HashSet<>(used);
                    s.add(i);
                    f(p.left, s, strength + p.left + p.right);
                }
            }
        }
    }
    public static void main(String[] args) throws Exception {
        List<String> lines = Files.lines(Paths.get("src/main/resources/2017/input-24.txt")).toList();
        components = new ArrayList<ImmutablePair<Integer,Integer>>();
        for (String line: lines) {
            String[] parts = line.split("/");
            components.add(ImmutablePair.of(Integer.parseInt(parts[0]), Integer.parseInt(parts[1])));
        }
        f(0, new HashSet<>(), 0);
        System.out.println(strongest);
    }
}
