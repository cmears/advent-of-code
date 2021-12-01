package advent2017;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Day22 {
    record Coord(int i, int j) {
        Coord turnLeft() {
            int i2=0; int j2=0;
            if (i == 1) { i2=0; j2=1; }
            if (i == -1) { i2=0; j2=-1; }
            if (j == 1) { i2=-1; j2=0; }
            if (j == -1) { i2=1; j2=0; }
            return new Coord(i2,j2);
        }
        Coord turnRight() { return turnLeft().turnLeft().turnLeft(); }
        Coord add(Coord d) {
            return new Coord(i+d.i, j+d.j);
        }
    }
    public static void main(String[] args) throws Exception {
        Set<Coord> infected = new HashSet<>();
        Set<Coord> weakened = new HashSet<>();
        Set<Coord> flagged = new HashSet<>();
        List<String> lines = Files.lines(Paths.get("src/main/resources/2017/input-22.txt")).toList();
        for (int i = 0 ; i < lines.size() ; i++)
            for (int j = 0 ; j < lines.get(i).length() ; j++)
                if (lines.get(i).charAt(j) == '#')
                    infected.add(new Coord(i,j));
        Coord c = new Coord(lines.size()/2, lines.get(0).length()/2);
        Coord d = new Coord(-1,0);
        int infections = 0;
        for (int r = 0 ; r < 10000000 ; r++) {
            if (infected.contains(c)) {
                d = d.turnRight();
                infected.remove(c);
                flagged.add(c);
            } else if (weakened.contains(c)) {
                weakened.remove(c);
                infected.add(c);
                infections++;
            } else if (flagged.contains(c)) {
                d = d.turnLeft().turnLeft();
                flagged.remove(c);
            } else {
                d = d.turnLeft();
                weakened.add(c);
            }
            c = c.add(d);
        }
        System.out.println(infections);
    }
}
