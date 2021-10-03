package advent2016;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Day16 {
    static void extend(ArrayList<Integer> a) {
        int n = a.size();
        a.add(0);
        for (int i = n-1; i >= 0 ; i--) {
            a.add(1 - a.get(i));
        }
    }

    static List<Integer> checksum(List<Integer> l) {
        if (l.size() % 2 == 1)
            return l;
        ArrayList<Integer> checksum = new ArrayList<>();
        for (int i = 0 ; i < l.size() ; i += 2) {
            checksum.add(l.get(i) == l.get(i+1) ? 1 : 0);
        }
        return checksum(checksum);
    }

    public static void main(String[] args) {
        ArrayList<Integer> a = new ArrayList<>();
        String input = "01110110101001000";
        for (char c: input.toCharArray()) {
            a.add(c == '0' ? 0 : 1);
        }
//        int sz = 272;
        int sz = 35651584;
        while (a.size() < sz)
            extend(a);
        List l = a.subList(0, sz);
        System.out.println(checksum(l).stream().map(Object::toString).collect(Collectors.joining("")));
    }
}
