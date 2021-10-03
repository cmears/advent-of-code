package advent2016;

import java.util.*;

import static java.lang.Math.abs;

public class Day13 {
    public static class AStar {
        record Pair(int a, int b) {}

        public static class Node {
            Node(int _x, int _y, Optional<Node> _previous, int _cost) {
                x = _x;
                y = _y;
                previous = _previous;
                cost = _cost;
            }

            Boolean open(int x, int y) {
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
            Iterator<Node> neighbours() {
                ArrayList<Node> neighbours = new ArrayList<>();
                if (open(x-1, y)) neighbours.add(new Node(x-1, y, Optional.of(this), cost+1));
                if (open(x+1, y)) neighbours.add(new Node(x+1, y, Optional.of(this), cost+1));
                if (open(x, y-1)) neighbours.add(new Node(x, y-1, Optional.of(this), cost+1));
                if (open(x, y+1)) neighbours.add(new Node(x, y+1, Optional.of(this), cost+1));
                return neighbours.iterator();
            }

            Boolean goal() {
                return x == 31 && y == 39;
            }

            int estimate() {
                return abs(31-x) + abs(39-y);
            }

            // Key for de-duplication purposes.
            Pair key() {
                return new Pair(x,y);
            }

            int x;
            int y;

            // The previous node in the path.
            Optional<Node> previous;
            // The cost already spent to reach this node.
            int cost;
        }

        PriorityQueue<Node> queue;
        Optional<Node> best;
        Map<Pair, Integer> seen;

        int under50;

        AStar() {
            Comparator c = Comparator.<Node>comparingInt(n -> n.cost + n.estimate());
            queue = new PriorityQueue<>(c);
            queue.add(new Node(1,1, Optional.empty(), 0));
            best = Optional.empty();
            seen = new HashMap<>();
            under50 = 0;
        }

        void step() {
            Node node = queue.remove();
            System.out.format("%2d,%2d\n", node.x, node.y);
            if (node.cost <= 50 && !seen.containsKey(node.key()))
                under50++;
            seen.put(node.key(), node.cost);
            if (node.goal()) {
                if (best.isEmpty() || best.get().cost > node.cost) {
                    best = Optional.of(node);
                }
            } else {
                for (Iterator<Node> it = node.neighbours(); it.hasNext(); ) {
                    Node n = it.next();
                    if (!(seen.containsKey(n.key()) && seen.get(n.key()) <= n.cost)) {
                        queue.add(n);
                    }
                }
            }
        }

        void search() {
            while (!queue.isEmpty()) {
                step();
            }
            System.out.println(best.get().cost);
            System.out.println(under50);
        }
    }

    public static void main(String[] args) {
        AStar a = new AStar();
        a.search();
    }
}
