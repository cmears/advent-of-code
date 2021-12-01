package advent2017;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.lang.Math.abs;

public class Day20 {
    static Pattern p = Pattern.compile("p=<([0-9-]+),([0-9-]+),([0-9-]+)>, v=<([0-9-]+),([0-9-]+),([0-9-]+)>, a=<([0-9-]+),([0-9-]+),([0-9-]+)>");
    static class Particle {
        int x;
        int y;
        int z;
        int vx;
        int vy;
        int vz;
        int ax;
        int ay;
        int az;
        Particle(String line) {
            Matcher m = p.matcher(line);
            if (m.matches()) {
                x = Integer.parseInt(m.group(1));
                y = Integer.parseInt(m.group(2));
                z = Integer.parseInt(m.group(3));
                vx = Integer.parseInt(m.group(4));
                vy = Integer.parseInt(m.group(5));
                vz = Integer.parseInt(m.group(6));
                ax = Integer.parseInt(m.group(7));
                ay = Integer.parseInt(m.group(8));
                az = Integer.parseInt(m.group(9));
            } else {
                System.out.println("!!!!");
            }
        }
        void step() {
            vx = Math.addExact(vx, ax);
            vy = Math.addExact(vy, ay);
            vz = Math.addExact(vz, az);
            x = Math.addExact(x, vx);
            y = Math.addExact(y, vy);
            z = Math.addExact(z, vz);
        }
        int distance() {
            return Math.addExact(Math.addExact(abs(x), abs(y)), abs(z));
        }
        boolean colocated(Particle p) {
            return x == p.x && y == p.y && z == p.z;
        }
    }
    static void part2() {
        ArrayList<Particle> particles = null;
        try {
            particles = new ArrayList<>(Files.lines(Paths.get("src/main/resources/2017/input-20.txt"))
                    .map(Particle::new)
                    .toList());
            while (true) {
                particles.forEach(Particle::step);
                particles.sort(Comparator.<Particle>comparingInt(p -> p.x).thenComparingInt(p -> p.y).thenComparingInt(p -> p.z));
                Set<Integer> toDelete = new HashSet<>();
                for (int i = 0; i < particles.size() - 1; i++) {
                    if (particles.get(i).colocated(particles.get(i + 1))) {
                        toDelete.add(i);
                        toDelete.add(i + 1);
                    }
                }
                for (int i = particles.size() - 1; i >= 0; i--) {
                    if (toDelete.contains(i))
                        particles.remove(i);
                }
            }
        } catch (Exception e) {
            System.out.println(particles.size());
        }
    }
    public static void main(String[] args) throws Exception {
        part1();
        part2();
    }
    static void part1() {
        int closest = 0;
        try {
            List<Particle> particles = Files.lines(Paths.get("src/main/resources/2017/input-20.txt"))
                    .map(Particle::new)
                    .toList();
            while (true) {
                Integer dist = null;
                for (int i = 0; i < particles.size(); i++) {
                    particles.get(i).step();
                    int d = particles.get(i).distance();
                    if (dist == null || d < dist) {
                        dist = d;
                        closest = i;
                    }
                }
            }
        } catch (Exception e) {
            System.out.println(closest);
        }
    }
}
