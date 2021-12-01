package advent2017;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

public class Day16 {
    public static void main(String[] args) throws Exception {
        String input = Files.readString(Paths.get("src/main/resources/2017/input-16.txt")).trim();
        String[] commands = input.split(",");
        char[] dancers = "abcdefghijklmnop".toCharArray();
        for (int round = 0 ; round < (1000000000 % 60) ; round++) {
            if (Arrays.equals(dancers, "abcdefghijklmnop".toCharArray()))
                System.out.format("*** %d ***\n", round);
            for (String command : commands) {
                char type = command.charAt(0);
                String rest = command.substring(1);
                if (type == 's') {
                    int n = Integer.parseInt(rest);
                    char[] a = new char[16];
                    for (int i = 0; i < 16; i++) {
                        a[(i + n) % 16] = dancers[i];
                    }
                    dancers = a;
                } else if (type == 'x') {
                    String[] parts = rest.split("/");
                    int i = Integer.parseInt(parts[0]);
                    int j = Integer.parseInt(parts[1]);
                    char t = dancers[i];
                    dancers[i] = dancers[j];
                    dancers[j] = t;
                } else if (type == 'p') {
                    char x = rest.charAt(0);
                    char y = rest.charAt(2);
                    int i = 0;
                    while (dancers[i] != x) i++;
                    int j = 0;
                    while (dancers[j] != y) j++;
                    char t = dancers[i];
                    dancers[i] = dancers[j];
                    dancers[j] = t;
                } else {
                    throw new Exception("uh oh");
                }
            }
            if (round == 0)
                System.out.println(dancers);
            if ((round+1) % 1000 == 0)
                System.out.println(round);
        }
        System.out.println(dancers);

//        char[] letters = "abcdefghijklmnop".toCharArray();
//        int[] offset = new int[16];
//        for (int i = 0 ; i < 16 ; i++) {
//            char c = letters[i];
//            int j = 0;
//            while (dancers[j] != c) j++;
//            offset[i] = (j-i+16)%16;
//            System.out.format("%3d", offset[i]);
//        }
//        System.out.println();
//
//        char[] ending = new char[16];
//        for (int i = 0 ; i < 16 ; i++) {
//            long j = offset[i];
//            j *= 1000000000;
//            j += i;
//            j %= 16;
//            ending[(int)j] = letters[i];
//        }
//        System.out.println(ending);
    }
}
