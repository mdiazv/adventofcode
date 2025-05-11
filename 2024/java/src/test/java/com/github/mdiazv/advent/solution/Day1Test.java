package com.github.mdiazv.advent.solution;

import com.github.mdiazv.advent.lib.Pair;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.*;

class Day1Test {
    private static final String input = """
            3   4
            4   3
            2   5
            1   3
            3   9
            3   3""";
    private Solution solution;

    @BeforeEach
    void setUp() {
        InputStream inputStream = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8));
        solution = new Day1();
        solution.parseInput(inputStream);
    }

    @Test
    void test_part1() {
        Pair<String, Integer> part1 = solution.part1();
        assertEquals(11, part1.b());
    }

    @Test
    void test_part2() {
        Pair<String, Integer> part2 = solution.part2();
        assertEquals(31, part2.b());
    }
}