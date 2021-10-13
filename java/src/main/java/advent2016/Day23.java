package advent2016;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Day23 {
    public static void main(String[] args) throws Exception {
        Stream<String> lines = Files.lines(Paths.get("src/main/resources/input-23.txt"));
        Machine m = new Machine();
        lines.forEach(m::add);
        m.registers[0] = 12;
        m.run();
        System.out.println(m.registers[0]);
    }
    static class Machine {
        int[] registers = new int[4];
        int pc = 0;
        ArrayList<Instruction> program = new ArrayList<>();

        void run() {
            while (pc < program.size()) {
                // Detect x += y
                if (program.get(pc) instanceof Increment &&
                        program.get(pc+1) instanceof Decrement &&
                        program.get(pc+2) instanceof JumpIfNonZero &&
                        program.get(pc+1).arguments[0].equals(program.get(pc+2).arguments[0]) &&
                        program.get(pc+2).arguments[1].getValue() == -2) {
                    //System.out.format("detected add at pc=%d\n", pc);
                    Register target = (Register)program.get(pc).arguments[0];
                    int amount = program.get(pc+1).arguments[0].getValue();
                    registers[target.index] += amount;
                    pc += 3;
                }

                program.get(pc).execute();
            }
        }

        static abstract class Argument {
            abstract int getValue();
        }
        static class Immediate extends Argument {
            int value;
            Immediate(int value) { this.value = value; }
            int getValue() { return value; }

            @Override
            public boolean equals(Object o) {
                if (this == o) return true;
                if (o == null || getClass() != o.getClass()) return false;
                Immediate immediate = (Immediate) o;
                return value == immediate.value;
            }
        }
        class Register extends Argument {
            int index;
            Register(int index) { this.index = index; }
            int getValue() { return registers[index]; }

            @Override
            public boolean equals(Object o) {
                if (this == o) return true;
                if (o == null || getClass() != o.getClass()) return false;
                Register register = (Register) o;
                return index == register.index;
            }
        }

        Argument parseArgument(String s) {
            if (s.equals("a")) return new Register(0);
            if (s.equals("b")) return new Register(1);
            if (s.equals("c")) return new Register(2);
            if (s.equals("d")) return new Register(3);
            return new Immediate(Integer.parseInt(s));
        }

        void add(String line) {
            String[] parts = line.split(" ");
            Argument[] arguments = Arrays.stream(parts).skip(1).map(this::parseArgument).toArray(Argument[]::new);
            if (parts[0].equals("cpy")) { program.add(new Copy(arguments)); }
            if (parts[0].equals("inc")) { program.add(new Increment(arguments)); }
            if (parts[0].equals("dec")) { program.add(new Decrement(arguments)); }
            if (parts[0].equals("jnz")) { program.add(new JumpIfNonZero(arguments)); }
            if (parts[0].equals("tgl")) { program.add(new Toggle(arguments)); }
        }

        static abstract class Instruction {
            Argument[] arguments;
            Instruction(Argument[] arguments) { this.arguments = arguments; }
            abstract void execute();
            abstract Instruction toggle();
        }

        class Copy extends Instruction {
            Copy(Argument[] arguments) { super(arguments); }
            void execute() {
                registers[((Register)arguments[1]).index] = arguments[0].getValue();
                pc++;
            }
            Instruction toggle() { return new JumpIfNonZero(arguments); }
        }

        class Increment extends Instruction {
            Increment(Argument[] arguments) { super(arguments); }
            void execute() {
                registers[((Register)arguments[0]).index]++;
                pc++;
            }
            Instruction toggle() { return new Decrement(arguments); }
        }

        class Decrement extends Instruction {
            Decrement(Argument[] arguments) { super(arguments); }
            void execute() {
                registers[((Register)arguments[0]).index]--;
                pc++;
            }
            Instruction toggle() { return new Increment(arguments); }
        }

        class JumpIfNonZero extends Instruction {
            JumpIfNonZero(Argument[] arguments) { super(arguments); }
            void execute() {
                if (arguments[0].getValue() != 0) {
                    pc += arguments[1].getValue();
                } else {
                    pc++;
                }
            }
            Instruction toggle() { return new Copy(arguments); }
        }

        class Toggle extends Instruction {
            Toggle(Argument[] arguments) { super(arguments); }
            void execute() {
                int index = pc + arguments[0].getValue();
                if (0 <= index && index < program.size()) {
                    Instruction i = program.get(index);
                    program.set(index, i.toggle());
                }
                pc++;
            }
            Instruction toggle() { return new Increment(arguments); }
        }

    }
}
