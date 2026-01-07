import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Gatherers;

record Range(long from, long to) implements Comparable<Range> {
    boolean contains(Ingredient x) {
        return from <= x.v && x.v <= to;
    }
    static Range of(long from, long to) {
        return new Range(from, to);
    }
    static Range parse(String s) {
        String[] parts = s.split("-");
        return new Range(Long.parseLong(parts[0]), Long.parseLong(parts[1]));
    }
    public int compareTo(Range o) {
        return Long.signum(this.from == o.from ? this.to - o.to : this.from - o.from);
    }
}

record Ingredient(long v) {
    static Ingredient parse(String s) {
        return new Ingredient(Long.parseLong(s));
    }
}

record Pair<A, B>(A a, B b) {}

static class Database {
    List<Range> ranges = List.of();
    void add(Range r) {
        List<Range> result = new ArrayList<>(ranges.size() + 1);
        for (Range x : ranges) {
            Range left = x.compareTo(r) <= 0 ? x : r;
            Range right = x.compareTo(r) > 0 ? x : r;
            if (left.to+1 < right.from) {
                result.add(x);
            } else {
                r = Range.of(left.from, Math.max(left.to, right.to));
            }
        }
        result.add(r);
        ranges = result;
    }
    void addFold(Range r) {
        Pair<List<Range>, Range> result = ranges.stream()
                .gather(Gatherers.fold(
                        () -> new Pair<List<Range>, Range>(new ArrayList<Range>(ranges.size()+1), r),
                        (p, x) -> {
                            Range left = x.compareTo(p.b) <= 0 ? x : p.b;
                            Range right = x.compareTo(p.b) > 0 ? x : p.b;
                            if (left.to+1 < right.from) {
                                p.a.add(x);
                                return p;
                            } else {
                                return new Pair<List<Range>, Range>(p.a, Range.of(left.from, Math.max(left.to, right.to)));
                            }
                        }
                ))
                .findFirst()
                .get();
        result.a.add(result.b);
        ranges = result.a;
    }
    boolean isFresh(Ingredient x) {
        return ranges.stream().anyMatch(r -> r.contains(x));
    }
    long totalFresh() {
        return ranges.stream()
                .mapToLong(r -> r.to - r.from + 1)
                .sum();
    }
}

void main() {
    Map<Boolean, List<String>> input = new Scanner(System.in)
            .useDelimiter("\n")
            .tokens()
            .filter(Predicate.not(String::isEmpty))
            .collect(Collectors.partitioningBy(
                    line -> line.contains("-")
            ));
    Database db = new Database();
    input.get(true).stream().map(Range::parse).forEach(db::addFold);
    long fresh = input.get(false).stream()
            .map(Ingredient::parse)
            .filter(db::isFresh)
            .count();
    IO.println(String.format("Identified %d fresh ingredients", fresh));
    IO.println(String.format("Total %d fresh ingredients", db.totalFresh()));
}
