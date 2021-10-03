package advent2016;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day9 {
    static Pattern pattern = Pattern.compile("\\(([0-9]+)x([0-9]+)\\)");
    public static String decompress1(String input) {
        StringBuilder output = new StringBuilder();
        Matcher matcher = pattern.matcher(input);
        int i = 0;
        while (i < input.length()) {
            if (input.charAt(i) == '(') {
                matcher.region(i, input.length());
                matcher.lookingAt();
                int markerLength = matcher.group(0).length();
                int chunkLength = Integer.parseInt(matcher.group(1));
                int chunkRepeats = Integer.parseInt(matcher.group(2));
                i += markerLength;
                String chunk = input.substring(i, i+chunkLength);
                for (int j = 0 ; j < chunkRepeats ; j++) {
                    output.append(chunk);
                }
                i += chunkLength;
            } else {
                output.append(input.charAt(i));
                i++;
            }
        }
        return output.toString();
    }
    public static long decompress2(String input, int from, int to) {
        if (from == to) return 0;
        Matcher matcher = pattern.matcher(input);
        if (input.charAt(from) == '(') {
            matcher.region(from, to);
            matcher.lookingAt();
            int markerLength = matcher.group(0).length();
            int chunkLength = Integer.parseInt(matcher.group(1));
            int chunkRepeats = Integer.parseInt(matcher.group(2));
            long chunkResult = decompress2(input, from+markerLength, from+markerLength+chunkLength);
            return chunkResult * chunkRepeats + decompress2(input, from+markerLength+chunkLength, to);
        } else {
            return 1 + decompress2(input, from+1, to);
        }
    }
    public static void main(String[] args) throws Exception {
        String input = Files.readString(Paths.get("input-9.txt")).replaceAll("\\s", "");
        String decompressed = decompress1(input);
        System.out.println(decompressed.length());

        long length2 = decompress2(input, 0, input.length());
        System.out.println(length2);
    }
}
