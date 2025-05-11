package com.github.mdiazv.advent.solution;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.*;

class Day3Test {
    private final String multiline = "xmul(2,4)%&mul[3,7]!@^do_\nnot_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
    private final String do_dont = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";
    private final String greedy_do = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64]do()(mul(11,8)undo()?mul(8,5))";
    private Solution solution;

    @BeforeEach
    void setUp() {
        solution = new Day3();
    }

    @Test
    void part1_handlesMultiline() {
        InputStream input = new ByteArrayInputStream(multiline.getBytes(StandardCharsets.UTF_8));
        solution.parseInput(input);
        assertEquals(161, solution.part1().b());
    }

    @Test
    void part2_removesDontDoPairs() {
        InputStream input = new ByteArrayInputStream(do_dont.getBytes(StandardCharsets.UTF_8));
        solution.parseInput(input);
        assertEquals(48, solution.part2().b());
    }

    @Test
    void part2_removeIsNotGreedy() {
        InputStream input = new ByteArrayInputStream(greedy_do.getBytes(StandardCharsets.UTF_8));
        solution.parseInput(input);
        assertEquals(136, solution.part2().b());
    }
}