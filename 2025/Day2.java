import java.util.List;
import java.util.Scanner;
import java.util.function.Predicate;
import java.util.stream.LongStream;
import java.util.stream.Stream;

record ID(long val) {
    boolean isInvalid() {
        return String.valueOf(val).matches("^(\\d+)\\1$");
    }
    boolean isVeryInvalid() {
        return String.valueOf(val).matches("^(\\d+)(\\1)+$");
    }
}
record Range(String first, String last) {
    static Range parse(String r) {
        String[] parts = r.split("-");
        return new Range(parts[0], parts[1]);
    }
    Stream<ID> elements() {
        return LongStream
                .range(Long.parseLong(first.strip()), Long.parseLong(last.strip())+1)
                .mapToObj(ID::new);
    }
}

long sumOfIDs(List<Range> ranges, Predicate<ID> filterFn) {
    return ranges.stream()
            .flatMap(Range::elements)
            .filter(filterFn)
            .mapToLong(ID::val)
            .sum();
}

void main() {
    List<Range> ranges = new Scanner(System.in)
            .useDelimiter(",")
            .tokens()
            .map(Range::parse)
            .toList();
    IO.println(String.format("Sum of invalid IDs is: %d", sumOfIDs(ranges, ID::isInvalid)));
    IO.println(String.format("Sum of very invalid IDs is: %d", sumOfIDs(ranges, ID::isVeryInvalid)));
}