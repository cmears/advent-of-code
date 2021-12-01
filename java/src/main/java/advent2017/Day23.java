package advent2017;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class Day23 {
    static class Machine {
        HashMap<String, Long> register = new HashMap<>();
        List<String> program = new ArrayList<>();
        int pc = 0;
        long freq = 0;
        int muls = 0;
        boolean running() { return 0 <= pc && pc < program.size(); }
        void execute() {
            String instruction = program.get(pc);
            String[] parts = instruction.split("\\s+");
            long arg1;
            if (Character.isAlphabetic(parts[1].charAt(0)))
                arg1 = register.getOrDefault(parts[1], 0L);
            else
                arg1 = Long.parseLong(parts[1]);
            long arg2 = 0;
            if (parts.length > 2)
                if (Character.isAlphabetic(parts[2].charAt(0)))
                    arg2 = register.getOrDefault(parts[2], 0L);
                else
                    arg2 = Long.parseLong(parts[2]);

            switch (parts[0]) {
                case "snd": freq = arg1; break;
                case "set": register.put(parts[1], arg2); break;
                case "add": register.put(parts[1], register.getOrDefault(parts[1], 0L) + arg2); break;
                case "sub": register.put(parts[1], register.getOrDefault(parts[1], 0L) - arg2); break;
                case "mul": register.put(parts[1], register.getOrDefault(parts[1], 0L) * arg2); muls++; break;
                case "mod": register.put(parts[1], register.getOrDefault(parts[1], 0L) % arg2); break;
                case "rcv": if (arg1 > 0) { System.out.format("recovered %d\n", freq); } break;
                case "jgz": if (arg1 > 0) { pc += arg2 - 1; } break;
                case "jnz": if (arg1 != 0) { pc += arg2 - 1; } break;
                default: System.out.println("???");
            }
            pc++;
        }
    }
    public static void main(String[] args) throws Exception {
        List<String> program = Files.lines(Paths.get("src/main/resources/2017/input-23.txt")).toList();
        Machine m = new Machine();
        m.program = program;
        while (m.running())
            m.execute();
        System.out.println(m.muls);

        int composites = 0;
        for (int i = 105700 ; i <= 122700 ; i += 17) {
            if (!prime(i))
                composites++;
        }
        System.out.println(composites);
    }
    static boolean prime(int n) {
        if (n % 2 == 0 && n > 2) return false;
        for (int i = 3 ; i*i <= n ; i += 2) {
            if (n % i == 0) return false;
        }
        return true;
    }
}
