package advent2017;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

public class Day8 {
    public static void main(String[] args) throws Exception{
        Map<String, Integer> register = new HashMap<>();
        Files.lines(Paths.get("src/main/resources/2017/input-8.txt"))
                .forEachOrdered(line -> {
                    String[] word = line.split("\\s+");
                    String condreg = word[4];
                    String operator = word[5];
                    Integer operand = Integer.parseInt(word[6]);
                    Integer c = register.getOrDefault(condreg, 0);
                    if ((operator.equals("==") && c.equals(operand)) ||
                            (operator.equals("<=") && c <= operand) ||
                            (operator.equals(">=") && c >= operand) ||
                            (operator.equals("!=") && !c.equals(operand)) ||
                            (operator.equals("<") && c < operand) ||
                            (operator.equals(">") && c > operand)) {
                        Integer amount = Integer.parseInt(word[2]);
                        if (word[1].equals("dec"))
                            amount *= -1;
                        int value = register.getOrDefault(word[0], 0) + amount;
                        register.put(word[0], value);
                    }
                });
        System.out.println(register.values().stream().max(Comparator.naturalOrder()));
    }
}
