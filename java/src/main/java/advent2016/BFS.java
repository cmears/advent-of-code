package advent2016;

import java.util.*;

public class BFS {
    public static interface Node {
        // What nodes can be reached from here?
        public Iterator<Node> bfsneighbours();

        // Is this node satisfy the goal?
        public Boolean goal();
    }

    LinkedList<Node> queue;

    public BFS(Node initial) {
        queue = new LinkedList<>();
        queue.add(initial);
    }

    Node peek() {
        return queue.peek();
    }

    void step() {
        Node node = queue.remove();

        for (Iterator<Node> it = node.bfsneighbours(); it.hasNext(); ) {
            queue.add(it.next());
        }
    }

    public Node next() {
        while (!queue.isEmpty()) {
            if (queue.peek().goal())
                return queue.remove();
            else
                step();
        }
        return null;
    }
}

