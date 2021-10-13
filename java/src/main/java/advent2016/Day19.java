package advent2016;

public class Day19 {
    static int input = 3014603;

    static int f(int n) {
        if (n==1) return 0;
        if (n%2==0) return 2*f(n/2);
        return 2*f(n/2)+2;
    }

    static class Node {
        int n;
        Node next;
        Node(int n, Node next) { this.n = n; this.next = next; }
    }

    public static void main(String[] args) {
        System.out.println(f(input)+1);

        Node head = new Node(1, null);
        Node current = head;
        for (int i = 1 ; i < input ; i++) {
            current.next = new Node(current.n + 1, null);
            current = current.next;
        }
        current.next = head;

        Node prevhead = current;

        int n = input;
        Node c = head;
        Node prevc = prevhead;
        Node o = c;
        Node prevo = null;
        for (int i = 0 ; i < input/2 ; i++) {
            prevo = o;
            o = o.next;
        }

        while (n > 1) {
            // Delete o
            o = o.next;
            prevo.next = o;

            if (n%2==1) {
                o = o.next;
                prevo = prevo.next;
            }
            c = c.next;
            n--;
        }
        System.out.println(c.n);
    }
}
