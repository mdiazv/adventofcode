import java.util.List;
import java.util.Scanner;

static class CrackerDial {
    static final int SIZE = 100;
    int val = 50;
    int stopped_at_zero;
    int pointed_at_zero;
    void apply(Move move) {
        var laps = Math.abs(move.d / SIZE);
        var last = val + (move.d % SIZE);
        pointed_at_zero += laps + (val != 0 && (last <= 0 || last >= SIZE) ? 1 : 0);
        val = Math.floorMod(last, SIZE);
        stopped_at_zero += val == 0 ? 1 : 0;
    }
    CrackerDial crack(List<Move> moves) {
        moves.forEach(this::apply);
        return this;
    }
}

record Move(int d) {
    static Move parse(String m) {
        int sign = m.charAt(0) == 'L' ? -1 : 1;
        int val = Integer.parseInt(m.substring(1));
        return new Move(sign * val);
    }
}

void main() {
    List<Move> moves = new Scanner(System.in)
            .useDelimiter("\n")
            .tokens()
            .map(Move::parse)
            .toList();
    CrackerDial dial = new CrackerDial().crack(moves);
    IO.println(String.format("Password is %d", dial.stopped_at_zero));
    IO.println(String.format("Password method 0x434C49434B is %d", dial.pointed_at_zero));
}