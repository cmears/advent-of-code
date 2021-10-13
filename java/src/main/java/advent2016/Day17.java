package advent2016;

import java.math.BigInteger;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Iterator;

public class Day17 {

    public static void main(String[] args) {
        AStar astar = new AStar(Node.initial());
        Node best = (Node)astar.search();
        System.out.println(best.path);

        BFS bfs = new BFS(Node.initial());
        Node n;
        int biggest = 0;
        while ((n = (Node)bfs.next()) != null)
            biggest = n.path.length();
        System.out.println(biggest);
    }

    public static String md5(String input) {
        try {
            byte[] digest = MessageDigest.getInstance("MD5").digest(input.getBytes());
            return String.format("%032x", new BigInteger(1, digest));
        } catch (Exception e) { return null; }
    }

    static String passcode = "mmsxrhfx";

    static class Node extends AStar.Node implements BFS.Node {
        String path;
        int row;
        int col;

        Node(String path, int row, int col) {
            this.path = path;
            this.row = row;
            this.col = col;
        }

        static Node initial() {
            return new Node("", 1,1);
        }

        static Boolean open(char c) {
            return "bcdef".indexOf(c) != -1;
        }

        @Override
        public Iterator<AStar.Node> neighbours() {
            String hash = md5(passcode + path);
            ArrayList<AStar.Node> nodes = new ArrayList<>();
            if (row > 1 && open(hash.charAt(0))) nodes.add(new Node(path + "U", row-1, col));
            if (row < 4 && open(hash.charAt(1))) nodes.add(new Node(path + "D", row+1, col));
            if (col > 1 && open(hash.charAt(2))) nodes.add(new Node(path + "L", row, col-1));
            if (col < 4 && open(hash.charAt(3))) nodes.add(new Node(path + "R", row, col+1));
            return nodes.iterator();
        }
        @Override
        public Iterator<BFS.Node> bfsneighbours() {
            String hash = md5(passcode + path);
            ArrayList<BFS.Node> nodes = new ArrayList<>();
            if (row > 1 && open(hash.charAt(0))) nodes.add(new Node(path + "U", row-1, col));
            if (row < 4 && open(hash.charAt(1))) nodes.add(new Node(path + "D", row+1, col));
            if (col > 1 && open(hash.charAt(2))) nodes.add(new Node(path + "L", row, col-1));
            if (col < 4 && open(hash.charAt(3))) nodes.add(new Node(path + "R", row, col+1));
            return nodes.iterator();
        }

        @Override
        public Boolean goal() {
            return row == 4 && col == 4;
        }

        @Override
        public int heuristic() {
            return 0;
        }

        @Override
        public Object key() {
            return path;
        }

        @Override
        public int cost() {
            return path.length();
        }
    }
}
