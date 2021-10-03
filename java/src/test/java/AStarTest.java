import static java.lang.Math.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import advent2016.AStar;
import advent2016.AStar.Node;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Iterator;

public class AStarTest {
    static class OpenFieldNode extends Node {
        record Coord(int x, int y) {}
        int x;
        int y;
        int cost;
        OpenFieldNode(int x, int y, int cost) { this.x = x; this.y = y; this.cost = cost; }
        public Iterator<Node> neighbours() {
            ArrayList<Node> nodes = new ArrayList<>();
            nodes.add(new OpenFieldNode(x+1,y, cost+2));
            nodes.add(new OpenFieldNode(x-1,y, cost+2));
            nodes.add(new OpenFieldNode(x,y+1, cost+2));
            nodes.add(new OpenFieldNode(x,y-1, cost+2));
            nodes.add(new OpenFieldNode(x+1,y+1, cost+3));
            nodes.add(new OpenFieldNode(x-1,y+1, cost+3));
            nodes.add(new OpenFieldNode(x+1,y-1, cost+3));
            nodes.add(new OpenFieldNode(x-1,y-1, cost+3));
            return nodes.iterator();
        }

        public Boolean goal() { return x == 50 && y == 50; }
        public int heuristic() {
            int dx = abs(x-50);
            int dy = abs(y-50);
            int mind = min(dx,dy);
            int maxd = max(dx,dy);
            return 3*mind + 2*(maxd-mind);
        }
        public Coord key() { return new Coord(x,y); }
        public int cost() { return this.cost; }

        public String toString() {
            return x + "," + y;
        }
    }

    @Test
    public void rightAnswer() {
        OpenFieldNode initial = new OpenFieldNode(0, 0, 0);
        AStar astar = new AStar(initial);
        Node best = astar.search();
        assertEquals(150, best.cost());
        assertTrue(astar.seenSize() < 300);
    }
}
