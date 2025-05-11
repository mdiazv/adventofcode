package com.github.mdiazv.advent.solution;

import com.github.mdiazv.advent.lib.Pair;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class Day2Test {
    private static List<Pair<String, Boolean>> safeData = List.of(
            Pair.of("7 6 4 2 1", true),
            Pair.of("1 2 7 8 9", false),
            Pair.of("9 7 6 2 1", false),
            Pair.of("1 3 2 4 5", false),
            Pair.of("8 6 4 4 1", false),
            Pair.of("1 3 6 7 9", true)
    );
    private static List<Pair<String, Boolean>> safeishData = List.of(
        Pair.of("7 6 4 2 1", true),
        Pair.of("1 2 7 8 9", false),
        Pair.of("9 7 6 2 1", false),
        Pair.of("1 3 2 4 5", true),
        Pair.of("8 6 4 4 1", true),
        Pair.of("1 3 6 7 9", true)
    );

    @Test
    void reportIsSafe() {
        safeData.forEach(p -> assertEquals(p.b(), Report.parse(p.a()).isSafe(), p.a()));
    }

    @Test
    void reportIsSafeish() {
        safeishData.forEach(p -> assertEquals(p.b(), Report.parse(p.a()).isSafeish(), p.a()));
    }

}