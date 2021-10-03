package advent2016;

import java.util.*;

public class Day11b {
    static boolean finished(ArrayList<Integer> state) {
        for (int i = 0 ; i < 15 ; i++) {
            if (state.get(i) != 4)
                return false;
        }
        return true;
    }
    static List<List<Integer>> allMoves(List<Integer> state) {
        ArrayList<List<Integer>> moves = new ArrayList<>();
        int e = state.get(0);
        for (int i = 1 ; i < 15 ; i++) {
            if (state.get(i) != e) continue;
            if (e > 1) moves.add(List.of(-1,i));
            if (e < 4) moves.add(List.of(1,i));
            for (int j = i+1 ; j < 15 ; j++) {
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
        newState.set(15, state.get(15) + 1);
        for (int i = 1 ; i < move.size() ; i++) {
            int obj = move.get(i);
            newState.set(obj, state.get(obj) + move.get(0));
        }
        return newState;
    }
    static boolean legal(ArrayList<Integer> state) {
        ArrayList<Boolean> dangerous = new ArrayList<>();
        for (int i = 0 ; i < 7 ; i++) dangerous.add(false);
        for (int i = 8 ; i < 15 ; i++) dangerous.set(state.get(i), true);
        for (int i = 1 ; i < 8 ; i++) {
            if (dangerous.get(state.get(i)) && state.get(i) != state.get(i+5))
                return false;
        }
        return true;
    }
    public static void main(String[] args) {
        // Objects: elevator (0), chips (1-7), generators (8-14), moves taken (15)
        // Order: polonium, thulium, promethium, ruthenium, cobalt, elerium, dilithium
        ArrayList<Integer> initial = new ArrayList<>(List.of(1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0));

        Queue<ArrayList<Integer>> queue = new ArrayDeque<>();
        queue.add(initial);

        Set<ArrayList<Integer>> seen = new HashSet<>();

        int result = 0;
        while (!queue.isEmpty()) {
            ArrayList<Integer> state = queue.remove();
            ArrayList<Integer> key = (ArrayList<Integer>) state.clone();
            key.remove(15);
            if (seen.contains(key)) continue;
            seen.add(key);
            System.out.println(state);
            if (finished(state)) {
                result = state.get(15);
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
