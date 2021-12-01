package advent2017;

import java.util.HashMap;

public class Day25 {
    public static void main(String[] args) {
        int target = 12794428;
        char state = 'A';
        int cursor = 0;
        HashMap<Integer, Integer> tape = new HashMap<>();

        for (int i = 0 ; i < target ; i++) {
            int r = tape.getOrDefault(cursor, 0);
            int w = -1;
            int d = 0;
            char s = '?';
            switch (state) {
                case 'A':
                    if (r == 0) { w=1; d=1; s='B'; }
                    if (r == 1) { w=0; d=-1; s='F'; }
                    break;
                case 'B':
                    if (r == 0) { w=0; d=1; s='C'; }
                    if (r == 1) { w=0; d=1; s='D'; }
                    break;
                case 'C':
                    if (r == 0) { w=1; d=-1; s='D'; }
                    if (r == 1) { w=1; d=1; s='E'; }
                    break;
                case 'D':
                    if (r == 0) { w=0; d=-1; s='E'; }
                    if (r == 1) { w=0; d=-1; s='D'; }
                    break;
                case 'E':
                    if (r == 0) { w=0; d=1; s='A'; }
                    if (r == 1) { w=1; d=1; s='C'; }
                    break;
                case 'F':
                    if (r == 0) { w=1; d=-1; s='A'; }
                    if (r == 1) { w=1; d=1; s='A'; }
                    break;
                default: System.out.println("!!!");
            }
            tape.put(cursor, w);
            cursor += d;
            state = s;
        }
        int ones = 0;
        for (int v: tape.values())
            if (v == 1)
                ones++;
        System.out.println(ones);

    }
}
