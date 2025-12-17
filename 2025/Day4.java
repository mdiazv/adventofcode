import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;
import java.util.function.Predicate;
import java.util.stream.IntStream;
import java.util.stream.Stream;

record Pos(int i, int j) {
    static Stream<Pos> range(Pos from, Pos to) {
        return IntStream.range(from.i, to.i)
                .boxed()
                .flatMap(ii -> IntStream.range(from.j, to.j)
                        .mapToObj(jj -> new Pos(ii, jj)));
    }
    static Pos of(int i, int j) {
        return new Pos(i, j);
    }
}

record World(char[][] w, int r, int c) {
    static char EMPTY = '.';
    static char ROLL = '@';
    public World(List<String> w) {
        this(w.stream().map(String::toCharArray).toArray(char[][]::new), w.size(), w.getFirst().length());
    }
    char get(Pos p) {
        return p.i < 0 || p.j < 0 || p.i >= r || p.j >= c ? EMPTY : w[p.i][p.j];
    }
    long nearbyRolls(Pos p) {
        return Pos.range(Pos.of(p.i-1, p.j-1), Pos.of(p.i+2, p.j+2))
                .filter(Predicate.not(p::equals))
                .mapToInt(this::get)
                .filter( v -> v == ROLL)
                .count();
    }
    boolean isAccessible(Pos p) {
        return get(p) == ROLL && nearbyRolls(p) < 4;
    }
    Stream<Pos> accessibleRolls() {
        return Pos.range(Pos.of(0, 0), Pos.of(r, c))
                .filter(this::isAccessible);
    }
    void remove(Pos p) {
        w[p.i][p.j] = EMPTY;
    }
}

static class RollCollector {
    static List<Pos> collect(World w) {
        return w.accessibleRolls()
                .peek(w::remove)
                .toList();
    }
    static List<Pos> collectAll(World w) {
        List<Pos> fresh, all = new LinkedList<>();
        do {
            fresh = collect(w);
            all.addAll(fresh);
        } while(!fresh.isEmpty());
        return all;
    }
}

void main() {
    World world = new World(new Scanner(System.in)
            .useDelimiter("\n")
            .tokens()
            .toList());
    long accessibleRolls = world.accessibleRolls().count();
    IO.println(String.format("There are %d initially accessible rolls", accessibleRolls));
    long collectedRolls = RollCollector.collectAll(world).size();
    IO.println(String.format("We collected %d rolls!", collectedRolls));
}