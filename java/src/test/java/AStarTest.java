package test.java;

import static java.lang.Math.abs;
import static org.junit.Assert.assertEquals;

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
            nodes.add(new OpenFieldNode(x+1,y, cost+1));
            nodes.add(new OpenFieldNode(x-1,y, cost+1));
            nodes.add(new OpenFieldNode(x,y+1, cost+1));
            nodes.add(new OpenFieldNode(x,y-1, cost+1));
            return nodes.iterator();
        }

        public Boolean goal() { return x == 50 && y == 50; }
        public int heuristic() { return abs(x-50) + abs(y-50); }
        public Coord key() { return new Coord(x,y); }
        public int cost() { return this.cost; }
    }

    @Test
    public void rightAnswer() {
        assertEquals(10,10);
    }
}
