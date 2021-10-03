package advent2016;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day8 {
    static Pattern rect = Pattern.compile("rect ([0-9]+)x([0-9]+)");
    static Pattern row = Pattern.compile("rotate row y=([0-9]+) by ([0-9]+)");
    static Pattern column = Pattern.compile("rotate column x=([0-9]+) by ([0-9]+)");

    public static void main(String[] args) throws Exception {
        boolean[][] display = new boolean[50][6];
        for (Iterator<String> it = Files.lines(Paths.get("input-8.txt")).iterator() ; it.hasNext() ; ) {
            String line = it.next();
            Matcher rectMatcher = rect.matcher(line);
            if (rectMatcher.matches()) {
                int w = Integer.parseInt(rectMatcher.group(1));
                int h = Integer.parseInt(rectMatcher.group(2));
                for (int i = 0 ; i < w ; i++)
                    for (int j = 0 ; j < h ; j++)
                        display[i][j] = true;
            }
            Matcher rowMatcher = row.matcher(line);
            if (rowMatcher.matches()) {
                int row = Integer.parseInt(rowMatcher.group(1));
                int offset = Integer.parseInt(rowMatcher.group(2));
                boolean[] previous = new boolean[50];
                for (int i = 0; i < 50 ; i++) {
                    previous[i] = display[i][row];
                }
                for (int i = 0 ; i < 50 ; i++)
                    display[i][row] = previous[(50 + i - offset) % 50];
            }
            Matcher columnMatcher = column.matcher(line);
            if (columnMatcher.matches()) {
                int column = Integer.parseInt(columnMatcher.group(1));
                int offset = Integer.parseInt(columnMatcher.group(2));
                boolean[] previous = new boolean[6];
                for (int i = 0; i < 6 ; i++) {
                    previous[i] = display[column][i];
                }
                for (int i = 0 ; i < 6 ; i++)
                    display[column][i] = previous[(6 + i - offset) % 6];
            }
        }
        int n = 0;
        for (int i = 0 ; i < 50 ; i++)
            for (int j = 0 ; j < 6 ; j++)
                n += display[i][j] ? 1 : 0;
        System.out.println(n);
        for (int j = 0 ; j < 6 ; j++) {
            for (int i = 0; i < 50; i++) {
                System.out.print(display[i][j] ? "##" : "  ");
            }
            System.out.println();
        }
    }
}
