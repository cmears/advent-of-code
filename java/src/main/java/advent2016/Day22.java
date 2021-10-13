package advent2016;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day22 {
    static class Node {
        static Pattern pattern = Pattern.compile("/dev/grid/node-x(\\d+)-y(\\d+) +\\d+T +(\\d+)T +(\\d+)T +\\d+%");
        int used;
        int avail;
        int x;
        int y;
        static Node fromLine(String line) {
            Matcher m = pattern.matcher(line);
            if (m.matches()) {
                Node n = new Node();
                n.x = Integer.parseInt(m.group(1));
                n.y = Integer.parseInt(m.group(2));
                n.used = Integer.parseInt(m.group(3));
                n.avail = Integer.parseInt(m.group(4));
                return n;
            } else {
                return null;
            }
        }
    }
    public static void main(String[] args) throws Exception {
        ArrayList<Node> nodes = new ArrayList<>();
        Iterator<String> lines = Files.lines(Paths.get("src/main/resources/input-22.txt")).iterator();
        while (lines.hasNext()) {
            Node n = Node.fromLine(lines.next());
            if (n != null)
                nodes.add(n);
        }
        int viable = 0;
        for (Node a: nodes) {
            for (Node b: nodes) {
                if (a != b && a.used > 0 && a.used <= b.avail)
                    viable++;
            }
        }
        System.out.println(viable);

        for (Node n: nodes) {
            if (n.used > 100)
                System.out.format("%d,%d\n", n.x, n.y);
        }
    }
}
