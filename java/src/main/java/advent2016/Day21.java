package advent2016;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

public class Day21 {
    static Pattern swapPos = Pattern.compile("swap position (\\d+) with position (\\d+)");
    static Pattern swapLetter = Pattern.compile("swap letter (.) with letter (.)");
    static Pattern rotate = Pattern.compile("rotate ([^ ]+) (\\d+) steps?");
    static Pattern rotateRelative = Pattern.compile("rotate based on position of letter (.)");
    static Pattern reverse = Pattern.compile("reverse positions (\\d+) through (\\d+)");
    static Pattern move = Pattern.compile("move position (\\d+) to position (\\d+)");

    public static void main(String[] args) throws Exception {
        System.out.println(run("src/main/resources/input-21-example.txt", "abcde"));
        System.out.println(run("src/main/resources/input-21.txt", "abcdefgh"));
        for (Iterator<String> it = new Permutor("abcdefgh") ; it.hasNext() ; ) {
            String s = it.next();
            if (run("src/main/resources/input-21.txt", s).equals("fbgdceah"))
                System.out.println(s);
        }
    }

    public static class Permutor implements Iterator<String> {
        char[] a;
        int n;
        String can;
        Permutor(String s) {
            a = s.toCharArray();
            Arrays.sort(a);
            n = a.length;
            can = String.valueOf(a);
        }
        @Override
        public boolean hasNext() {
            return can != null;
        }
        @Override
        public String next() {
            String oldCan = can;
            can = null;
            int k = -1;
            for (int i = 0 ; i < n-1 ; i++)
                if (a[i] < a[i+1])
                    k = i;
            if (k > -1) {
                int l = k+1;
                for (int i = k+1 ; i < n ; i++)
                    if (a[k] < a[i])
                        l = i;
                char c = a[k];
                a[k] = a[l];
                a[l] = c;
                for (int i = k+1 ; i < n-i+k ; i++) {
                    char d = a[i];
                    a[i] = a[n-i+k];
                    a[n-i+k] = d;
                }
                can = String.valueOf(a);
            }
            return oldCan;
        }
    }

    public static String run(String path, String value) throws Exception {
        Stream<String> lines = Files.lines(Paths.get(path));

        char[] s = value.toCharArray();
        int n = s.length;

        for (Iterator<String> it = lines.iterator(); it.hasNext() ; ) {
            String line = it.next();
            Matcher m;

            m = swapPos.matcher(line);
            if (m.matches()) {
                int i = Integer.parseInt(m.group(1));
                int j = Integer.parseInt(m.group(2));
                char c = s[i];
                s[i] = s[j];
                s[j] = c;
            }

            m = swapLetter.matcher(line);
            if (m.matches()) {
                char a = m.group(1).charAt(0);
                char b = m.group(2).charAt(0);
                int i,j;
                for (i = 0 ; i < n ; i++) if (s[i] == a) break;
                for (j = 0 ; j < n ; j++) if (s[j] == b) break;
                char c = s[i];
                s[i] = s[j];
                s[j] = c;
            }

            m = rotate.matcher(line);
            if (m.matches()) {
                int steps = Integer.parseInt(m.group(2));
                steps %= n;
                if (m.group(1).equals("left")) steps *= -1;
                char[] s2 = new char[n];
                for (int i = 0 ; i < n ; i++) {
                    s2[(i+steps+n)%n] = s[i];
                }
                s = s2;
            }

            m = rotateRelative.matcher(line);
            if (m.matches()) {
                char a = m.group(1).charAt(0);
                int i;
                for (i = 0 ; i < n ; i++) if (s[i] == a) break;
                int steps = 1 + i + (i >= 4 ? 1 : 0);
                steps %= n;
                char[] s2 = new char[n];
                for (int j = 0 ; j < n ; j++) {
                    s2[(j+steps+n)%n] = s[j];
                }
                s = s2;
            }

            m = reverse.matcher(line);
            if (m.matches()) {
                int i = Integer.parseInt(m.group(1));
                int j = Integer.parseInt(m.group(2));
                char s2[] = new char[n];
                for (int k = 0 ; k < i ; k++) s2[k] = s[k];
                for (int k = 0 ; k < j-i+1 ; k++) s2[i+k] = s[j-k];
                for (int k = j+1 ; k < n ; k++) s2[k] = s[k];
                s = s2;
            }

            m = move.matcher(line);
            if (m.matches()) {
                int i = Integer.parseInt(m.group(1));
                int j = Integer.parseInt(m.group(2));
                char c = s[i];
                for (int k = i ; k < n-1 ; k++)
                    s[k] = s[k+1];
                for (int k = n-1 ; k > j ; k--)
                    s[k] = s[k-1];
                s[j] = c;
            }

        }

        return String.valueOf(s);
    }
}
