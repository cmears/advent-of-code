package advent2017;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class Day19 {
    static char get(List<String> lines, int row, int col) {
        return lines.get(row).charAt(col);
    }
    static boolean valid(List<String> lines, int row, int col) {
        return get(lines, row, col) != ' ';
    }
    public static void main(String[] args) throws Exception {
        List<String> lines = Files.lines(Paths.get("src/main/resources/2017/input-19.txt")).toList();
        int rowD = 1;
        int colD = 0;
        int row = 0;
        int col = 0;
        while (get(lines, row, col) != '|')
            col++;
        int steps = 1;

        while (true) {
            char c = get(lines, row, col);
            if (Character.isAlphabetic(c))
                System.out.print(c);
            if (valid(lines, row+rowD, col+colD)) {
                row += rowD;
                col += colD;
                steps++;
            } else {
                if (rowD != 0) {
                    rowD = 0;
                    if (valid(lines, row, col + 1))
                        colD = 1;
                    else if (valid(lines, row, col - 1))
                        colD = -1;
                } else {
                    colD = 0;
                    if (valid(lines, row+1, col))
                        rowD = 1;
                    else if (valid(lines, row-1, col))
                        rowD = -1;
                }
                if (rowD == 0 && colD == 0)
                    break;
            }
        }
        System.out.println();
        System.out.println(steps);
    }
}
