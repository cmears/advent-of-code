package advent2016;

public class Day18 {

    public static void main(String[] args) {
        String row = ".^^..^...^..^^.^^^.^^^.^^^^^^.^.^^^^.^^.^^^^^^.^...^......^...^^^..^^^.....^^^^^^^^^....^^...^^^^..^";
        String[] traps = { "^^.", ".^^", "^..", "..^"};
        int width = row.length();

        int rows = 0;
        int safe = 0;

        while (rows < 400000) {
            for (int i = 0; i < width; i++)
                if (row.charAt(i) == '.')
                    safe++;
            String next = "";
            row = "." + row + ".";
            for (int i = 0; i < width; i++) {
                String triple = row.substring(i, i + 3);
                Boolean trap = false;
                for (int j = 0 ; j < 4 ; j++)
                    if (traps[j].equals(triple))
                        trap = true;
                next = next + (trap ? "^" : ".");
            }
            row = next;
            rows++;
        }
        System.out.println(safe);
    }
}
