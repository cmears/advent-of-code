package advent2016;

import java.util.*;

import static java.lang.Math.abs;

public class Day13b {
    public static class Node extends AStar.Node {
        record Pair(int a, int b) {}
        Node(int x, int y, int cost) {
            this.x = x;
            this.y = y;
            this.cost = cost;
        }

        static Boolean open(int x, int y) {
            if (x < 0 || y < 0) return false;
            int n = x*x + 3*x + 2*x*y + y + y*y;
            n += 1364;
            int bitcount = 0;
            while (n > 0) {
                bitcount += (n % 2);
                n /= 2;
            }
            return (bitcount % 2 == 0);
        }

        // What nodes can be reached from here?
        public Iterator<AStar.Node> neighbours() {
            ArrayList<AStar.Node> neighbours = new ArrayList<>();
            if (open(x-1, y)) neighbours.add(new Node(x-1, y,cost+1));
            if (open(x+1, y)) neighbours.add(new Node(x+1, y,cost+1));
            if (open(x, y-1)) neighbours.add(new Node(x, y-1,cost+1));
            if (open(x, y+1)) neighbours.add(new Node(x, y+1,cost+1));
            return neighbours.iterator();
        }

        public Boolean goal() {
            return x == 31 && y == 39;
        }

        public int heuristic() {
            return abs(31-x) + abs(39-y);
        }

        public Pair key() { return new Pair(x,y); }

        public int cost() { return cost; }

        int x;
        int y;
        int cost;
    }

    public static void main(String[] args) {
        Node initial = new Node(1,1,0);
        AStar a = new AStar(initial);
        AStar.Node best = a.search();
        System.out.println(best.cost());
    }
}
