import java.util.List;
import java.util.Scanner;
import java.util.function.Predicate;
import java.util.stream.IntStream;

record Operation(String op) {
    long run(long a, long b) {
        return switch (op) {
            case "+" -> a + b;
            case "*" -> a * b;
            default ->  throw new RuntimeException("Cephalopod math is broken");
        };
    }
}

record Problem(List<String> ops) {
    long solve() {
        Operation op = new Operation(ops.getLast());
        return ops.subList(0, ops.size() - 1)
                .stream()
                .mapToLong(Long::parseLong)
                .reduce(op::run)
                .getAsLong();
    }
}

record CephalopodProblem(List<String> ops) {
    long solve() {
        String sign = String.valueOf(ops.getFirst().charAt(ops.getFirst().length()-1));
        Operation op = new Operation(sign);
        return ops.stream()
                .map(s -> s.replaceFirst("[+*]$", ""))
                .mapToLong(Long::parseLong)
                .reduce(op::run)
                .getAsLong();
    }
}

record Whiteboard(List<char[]> data, List<String[]> human) {
    Whiteboard(List<char[]> data) {
        this(data, data.stream()
                .map(String::new)
                .map(String::trim)
                .map(s -> s.split("\\s+"))
                .toList());
    }
    Problem problem(int k) {
        return new Problem(human.stream()
                .map(r -> r[k])
                .toList()
        );
    }
    long solveAll() {
        return IntStream.range(0, human.getFirst().length)
                .mapToObj(this::problem)
                .mapToLong(Problem::solve)
                .sum();
    }
    String column(int k) {
        int M = data.size();
        StringBuilder sb = new StringBuilder(M);
        IntStream.range(0, M)
                .map(i -> data.get(i)[k])
                .filter(c -> c != ' ')
                .forEach(c -> sb.append((char) c));
        return sb.toString();
    }
    long solveCephalopod() {
        int k = 0, N = data.getFirst().length;
        long s = 0;
        do {
            List<String> ops = IntStream.range(k, N)
                    .mapToObj(this::column)
                    .takeWhile(Predicate.not(String::isEmpty))
                    .toList();
            s += new CephalopodProblem(ops).solve();
            k += ops.size()+1;
        } while (k < N);
        return s;
    }
}

void main() {
     Whiteboard wb = new Whiteboard(new Scanner(System.in)
            .useDelimiter("\n")
            .tokens()
            .map(String::toCharArray)
            .toList());
     IO.println(String.format("Sum of the solution to all problems is: %d", wb.solveAll()));
     IO.println(String.format("Cephalopod sum of the solution to all problems is: %d", wb.solveCephalopod()));
}