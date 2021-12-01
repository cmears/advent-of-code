package advent2017;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Day12 {
    public static void main(String[] args) throws Exception {
        ArrayList<ArrayList<Integer>> connection = new ArrayList<>();
        Files.lines(Paths.get("src/main/resources/2017/input-12.txt"))
                .forEachOrdered(line -> {
                    String[] chunks = line.split(" <-> ");
                    ArrayList<Integer> neighbours = new ArrayList<>();
                    Arrays.stream(chunks[1].split(", ")).map(Integer::parseInt).forEach(neighbours::add);
                    connection.add(neighbours);
                });
        {
            Stack<Integer> stack = new Stack<>();
            Set<Integer> seen = new HashSet<>();
            stack.push(0);
            while (!stack.empty()) {
                Integer i = stack.pop();
                if (seen.contains(i))
                    continue;
                seen.add(i);
                connection.get(i).forEach(stack::push);
            }
            System.out.println(seen.size());
        }

        Set<Integer> todo = new HashSet<Integer>();
        for (int i = 0 ; i < connection.size() ; i++)
            todo.add(i);
        int groups = 0;
        while (!todo.isEmpty()) {
            groups++;
            Stack<Integer> stack = new Stack<>();
            Set<Integer> seen = new HashSet<>();
            stack.push(todo.iterator().next());
            while (!stack.empty()) {
                Integer i = stack.pop();
                if (seen.contains(i))
                    continue;
                seen.add(i);
                todo.remove(i);
                connection.get(i).forEach(stack::push);
            }
        }
        System.out.println(groups);
    }
}
