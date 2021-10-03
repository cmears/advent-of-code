package advent2016;

import java.util.*;

public class Day11 {
    static boolean finished(ArrayList<Integer> state) {
        for (int i = 0 ; i < 11 ; i++) {
            if (state.get(i) != 4)
                return false;
        }
        return true;
    }
    static List<List<Integer>> allMoves(List<Integer> state) {
        ArrayList<List<Integer>> moves = new ArrayList<>();
        int e = state.get(0);
        for (int i = 1 ; i < 11 ; i++) {
            if (state.get(i) != e) continue;
            if (e > 1) moves.add(List.of(-1,i));
            if (e < 4) moves.add(List.of(1,i));
            for (int j = i+1 ; j < 11 ; j++) {
                if (state.get(j) != e) continue;
                if (e > 1) moves.add(List.of(-1,i,j));
                if (e < 4) moves.add(List.of(1,i,j));
            }
        }
        return moves;
    }
    static ArrayList<Integer> execute(List<Integer> move, ArrayList<Integer> state) {
        ArrayList<Integer> newState = (ArrayList<Integer>) state.clone();
        newState.set(0, state.get(0) + move.get(0));
        newState.set(11, state.get(11) + 1);
        for (int i = 1 ; i < move.size() ; i++) {
            int obj = move.get(i);
            newState.set(obj, state.get(obj) + move.get(0));
        }
        return newState;
    }
    static boolean legal(ArrayList<Integer> state) {
        ArrayList<Boolean> dangerous = new ArrayList<>();
        for (int i = 0 ; i < 5 ; i++) dangerous.add(false);
        for (int i = 6 ; i < 11 ; i++) dangerous.set(state.get(i), true);
        for (int i = 1 ; i < 6 ; i++) {
            if (dangerous.get(state.get(i)) && state.get(i) != state.get(i+5))
                return false;
        }
        return true;
    }
    public static void main(String[] args) {
        // Objects: elevator (0), chips (1-5), generators (6-10), moves taken (11)
        // Order: polonium, thulium, promethium, ruthenium, cobalt
        ArrayList<Integer> initial = new ArrayList<Integer>(List.of(1,2,1,2,1,1,1,1,1,1,1,0));

        Queue<ArrayList<Integer>> queue = new ArrayDeque<ArrayList<Integer>>();
        queue.add(initial);

        Set<ArrayList<Integer>> seen = new HashSet<>();

        int result = 0;
        while (!queue.isEmpty()) {
            ArrayList<Integer> state = queue.remove();
            ArrayList<Integer> key = (ArrayList<Integer>) state.clone();
            key.remove(11);
            if (seen.contains(key)) continue;
            seen.add(key);
            System.out.println(state);
            if (finished(state)) {
                result = state.get(11);
                break;
            }
            for (List<Integer> m: allMoves(state)) {
                ArrayList<Integer> newState = execute(m, state);
                if (legal(newState))
                    queue.add(newState);
            }
        }
        System.out.println(result);
    }
}
