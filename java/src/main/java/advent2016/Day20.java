package advent2016;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class Day20 {
    record Range(long l, long u) {}
    public static void main(String[] args) {
        try {
            List<String> lines = Files.readAllLines(Paths.get("2016/input-20.txt"));
            List<Range> ranges = lines.stream().map(s -> {
                String[] parts = s.split("-");
                return new Range(Long.parseLong(parts[0]), Long.parseLong(parts[1]));
            }).collect(Collectors.toList());
            System.out.println(ranges.size());
            ranges.sort(Comparator.comparingLong(r -> r.l));
            System.out.println(ranges.get(0));
            long candidate = 0;
            for (Range r: ranges) {
                if (r.l <= candidate && candidate <= r.u)
                    candidate = r.u + 1;
                else if (candidate < r.l)
                    break;
            }
            System.out.println(candidate);

            long lowest = 0;
            long npermitted = 0;
            for (Range r: ranges) {
                if (r.l <= lowest && lowest <= r.u) {
                    lowest = r.u + 1;
                } else if (lowest < r.l) {
                    npermitted += r.l - lowest;
                    lowest = r.u + 1;
                }
            }
            long toobig = 1l << 32;
            npermitted += toobig - lowest;
            System.out.println(npermitted);
        } catch (Exception e) {}
    }
}
