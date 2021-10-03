package advent2016;

public class Day15 {
    public static void main(String[] args) {
//        int[] sizes = {13,5,17,3,7,19};
//        int[] positions = {11,0,11,0,2,17};
        int[] sizes = {13,5,17,3,7,19,11};
        int[] positions = {11,0,11,0,2,17,0};

        int t = 0;
        int step = 1;

        for (int i = 0 ; i < sizes.length ; i++) {
            while ((t + positions[i] + i + 1) % sizes[i] != 0)
                t += step;

            // Really should be taking the gcd of the sizes,
            // but they're all prime anyway.
            step *= sizes[i];
        }
        System.out.println(t);
    }
}
