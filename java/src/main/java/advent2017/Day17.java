package advent2017;

import java.util.ArrayList;

public class Day17 {
    static class Circular {
        int value;
        Circular next;
    }
    public static void main(String[] args) {
        Circular zero = new Circular();
        zero.value = 0;
        zero.next = zero;

        Circular current = zero;
        for (int j = 1 ; j <= 2017 ; j++) {
            for (int i = 0 ; i < 382 ; i++)
                current = current.next;
            Circular c = new Circular();
            c.value = j;
            c.next = current.next;
            current.next = c;
            current = c;
        }
        System.out.println(current.next.value);

        int i = 0;
        int n = 1;
        int afterZero = 0;
        for (int j = 1 ; j <= 50000000 ; j++) {
            i += 382;
            i %= n;
            if (i == 0)
                afterZero = j;
            i++;
            n++;
        }
        System.out.println(afterZero);

    }
}
