package advent2016;

import java.util.*;

public class AStar {
    public static abstract class Node {
        // What nodes can be reached from here?
        public abstract Iterator<Node> neighbours();

        // Is this node satisfy the goal?
        public abstract Boolean goal();

        // What's an underestimate from here to the goal?
        public abstract int heuristic();

        // What's this node's "key" for caching?
        public abstract Object key();

        // What's the cost we've spent to get here?
        public abstract int cost();
    }

    PriorityQueue<Node> queue;
    Node best;

    // We've seen these nodes before, with the given costs.
    HashMap<Object, Integer> seen;

    public AStar(Node initial) {
        queue = new PriorityQueue<>(Comparator.<Node>comparingInt(n -> n.cost() + n.heuristic()));
        queue.add(initial);
        best = null;
        seen = new HashMap<>();
    }

    void step() {
        Node node = queue.remove();

        // If what we've seen before is better than this, stop.
        if (seen.containsKey(node.key()) && seen.get(node.key()) <= node.cost())
            return;
        seen.put(node.key(), node.cost());

        // If we have no chance of beating the optimal path, stop.
        if (best != null && node.cost() + node.heuristic() >= best.cost())
            return;

        if (node.goal()) {
            if (best == null || best.cost() > node.cost()) {
                best = node;
            }
        } else {
            for (Iterator<Node> it = node.neighbours(); it.hasNext(); ) {
                queue.add(it.next());
            }
        }
    }

    public Node search() {
        while (!queue.isEmpty())
            step();
        return best;
    }

    public int seenSize() { return seen.size(); }
}
