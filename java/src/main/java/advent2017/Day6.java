package advent2017;

import java.util.*;

public class Day6 {
    public static void main(String[] args) {
        Integer[] input = {4,1,15,12,0,9,9,5,5,8,7,3,14,5,12,3};

        ArrayList<Integer> memory = new ArrayList<Integer>(Arrays.asList(input));
        Map<ArrayList<Integer>, Integer> seen = new HashMap<>();
        seen.put(new ArrayList<>(memory), 0);

        int reconfigurations = 0;

        while (true) {
            int largest = 0;
            for (int i = 1 ; i < memory.size() ; i++)
                if (memory.get(i) > memory.get(largest))
                    largest = i;
            int pile = memory.get(largest);
            memory.set(largest, 0);
            int i = (largest + 1) % memory.size();
            while (pile > 0) {
                memory.set(i, memory.get(i) + 1);
                pile--;
                i = (i + 1) % memory.size();
            }

            reconfigurations++;

            if (seen.containsKey(memory))
                break;
            seen.put(new ArrayList<>(memory), reconfigurations);
        }

        System.out.println(reconfigurations);
        System.out.println(reconfigurations - seen.get(memory));

    }
}
