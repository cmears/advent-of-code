package advent2016;

import java.nio.file.Files;
import java.nio.file.Paths;

public class Day7 {
    public static boolean supportsTLS(String line) {
        boolean insideBrackets = false;
        boolean foundABBA = false;
        for (int i = 0 ; i < line.length() ; i++) {
            if (line.charAt(i) == '[')
                insideBrackets = true;
            if (line.charAt(i) == ']')
                insideBrackets = false;
            if (i >= 3 &&
                    line.charAt(i-3) == line.charAt(i) &&
                    line.charAt(i-2) == line.charAt(i-1) &&
                    line.charAt(i) != line.charAt(i-1)) {
                if (insideBrackets)
                    return false;
                else
                    foundABBA = true;
            }
        }
        return foundABBA;
    }
    public static boolean supportsSSL(String line) {
        // Slow, quadratic approach.
        // Find an ABA, then scan the whole string for a BAB.
        boolean insideBrackets = false;
        for (int i = 0 ; i < line.length() ; i++) {
            if (line.charAt(i) == '[') insideBrackets = true;
            if (line.charAt(i) == ']') insideBrackets = false;
            if (i >= 2 &&
                    !insideBrackets &&
                    line.charAt(i-2) == line.charAt(i) &&
                    line.charAt(i-1) != line.charAt(i)) {
                // Now that we have an ABA outside brackets, look for a BAB inside brackets.
                boolean jInsideBrackets = false;
                for (int j = 0 ; j < line.length() ; j++) {
                    if (line.charAt(j) == '[') jInsideBrackets = true;
                    if (line.charAt(j) == ']') jInsideBrackets = false;
                    if (j >= 2 && jInsideBrackets &&
                            line.charAt(j-2) == line.charAt(j) &&
                            line.charAt(j) == line.charAt(i-1) &&
                            line.charAt(j-1) == line.charAt(i)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }
    public static void main(String[] args) throws Exception {
        System.out.println(Files.lines(Paths.get("input-7.txt")).filter(Day7::supportsTLS).count());
        System.out.println(Files.lines(Paths.get("input-7.txt")).filter(Day7::supportsSSL).count());
    }
}
