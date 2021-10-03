package advent2016;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

record Dest(boolean output, int n) {}
record Pair(Dest low, Dest high) {}
record Assignment(int val, Dest dest) {}

public class Day10 {
    public static void main(String[] args) throws Exception {
        Pattern compare = Pattern.compile("bot ([0-9]+) gives low to (\\w+) ([0-9]+) and high to (\\w+) ([0-9]+)");
        Pattern assign = Pattern.compile("value ([0-9]+) goes to bot ([0-9]+)");

        Map<Integer, Pair> orders = new HashMap<>();
        Map<Integer, Integer> holding = new HashMap<>();
        Map<Integer, Integer> outputs = new HashMap<>();
        Queue<Assignment> queue = new ArrayDeque<>();

        for (String line: Files.readAllLines(Paths.get("input-10.txt"))) {
            Matcher c = compare.matcher(line);
            if (c.matches()) {
                int bot = Integer.parseInt(c.group(1));
                boolean lowOutput = c.group(2).equals("output");
                int low = Integer.parseInt(c.group(3));
                boolean highOutput = c.group(4).equals("output");
                int high = Integer.parseInt(c.group(5));
                orders.put(bot, new Pair(new Dest(lowOutput, low), new Dest(highOutput, high)));
            }

            Matcher a = assign.matcher(line);
            if (a.matches()) {
                int val = Integer.parseInt(a.group(1));
                int bot = Integer.parseInt(a.group(2));
                queue.add(new Assignment(val, new Dest(false, bot)));
            }
        }

        while (!queue.isEmpty()) {
            Assignment a = queue.remove();
            if (a.dest().output()) {
                outputs.put(a.dest().n(), a.val());
            } else {
                int bot = a.dest().n();
                if (holding.containsKey(bot)) {
                    int x = a.val();
                    int y = holding.get(bot);

                    int smaller = Math.min(x, y);
                    int larger = Math.max(x, y);

                    if (smaller == 17 && larger == 61) {
                        System.out.format("Bot %d is comparing 17 and 61\n", bot);
                    }

                    Pair p = orders.get(bot);
                    queue.add(new Assignment(smaller, p.low()));
                    queue.add(new Assignment(larger, p.high()));
                } else {
                    holding.put(bot, a.val());
                }
            }
        }

        int result = outputs.get(0) * outputs.get(1) * outputs.get(2);
        System.out.format("Product of outputs 0,1,2: %d\n", result);
    }
}
