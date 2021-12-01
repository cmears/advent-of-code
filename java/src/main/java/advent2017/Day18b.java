package advent2017;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Day18b {
    static class Machine {
        HashMap<String, Long> register = new HashMap<>();
        List<String> program = new ArrayList<>();
        int pc = 0;
        int sends = 0;
        Machine partner;
        boolean waiting = false;
        Queue<Long> queue = new ArrayDeque<>();
        boolean running() { return 0 <= pc && pc < program.size(); }
        void execute() {
            waiting = false;
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
                case "snd": partner.queue.add(arg1); sends++; break;
                case "set": register.put(parts[1], arg2); break;
                case "add": register.put(parts[1], register.getOrDefault(parts[1], 0L) + arg2); break;
                case "mul": register.put(parts[1], register.getOrDefault(parts[1], 0L) * arg2); break;
                case "mod": register.put(parts[1], register.getOrDefault(parts[1], 0L) % arg2); break;
                case "rcv":
                    if (queue.isEmpty()) { waiting = true; return; }
                    register.put(parts[1], queue.remove());
                    break;
                case "jgz": if (arg1 > 0) { pc += arg2 - 1; } break;
                default: System.out.println("???");
            }
            pc++;
        }
    }
    public static void main(String[] args) throws Exception {
        List<String> program = Files.lines(Paths.get("src/main/resources/2017/input-18.txt")).toList();
        Machine m0 = new Machine();
        m0.program = program;
        Machine m1 = new Machine();
        m1.program = program;
        m1.register.put("p", 1L);

        m0.partner = m1;
        m1.partner = m0;

        while (m0.running() || m1.running()) {
            if (m0.running()) m0.execute();
            if (m1.running()) m1.execute();
            if (m0.waiting && m1.waiting) break;
        }
        System.out.println(m1.sends);
    }
}
