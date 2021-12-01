package advent2017;

import java.nio.file.Files;
import java.nio.file.Paths;

public class Day9 {
    public static void main(String[] args) throws Exception {
        String input = Files.readString(Paths.get("src/main/resources/2017/input-9.txt"));
        int depth = 0;
        boolean garbage = false;
        boolean ignore = false;
        int score = 0;
        int removed = 0;
        for (char c: input.toCharArray()) {
            if (ignore) {
                ignore = false;
                continue;
            }

            if (garbage && c == '!') {
                ignore = true;
                continue;
            }

            if (garbage && c == '>') {
                garbage = false;
                continue;
            }

            if (!garbage && c == '<') {
                garbage = true;
                continue;
            }

            if (!garbage && c == '{') {
                depth++;
                continue;
            }

            if (!garbage && c == '}') {
                score += depth;
                depth--;
                continue;
            }

            if (garbage)
                removed++;
        }
        System.out.println(score);
        System.out.println(removed);
    }
}
