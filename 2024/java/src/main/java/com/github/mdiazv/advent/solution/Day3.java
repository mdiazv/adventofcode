package com.github.mdiazv.advent.solution;

import com.github.mdiazv.advent.lib.Pair;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Day3 implements Solution {
    private static final String MUL_REGEX = "mul\\((?<X>\\d{1,3}),(?<Y>\\d{1,3})\\)";
    private static final String DONT_DO_REGEX = "don't\\(\\).*?do\\(\\)";
    private String memory;
    public void parseInput(InputStream input) {
        memory = new BufferedReader(new InputStreamReader(input))
            .lines()
            .collect(Collectors.joining());
    }

    public Pair<String, Integer> part1() {
        int total = sumOfMul(memory);
        return new Pair("Total sum of multiplication instructions: %d", total);
    }

    public Pair<String, Integer> part2() {
        String simplified = memory.replaceAll(DONT_DO_REGEX, "");
        int total = sumOfMul(simplified);
        return new Pair("Total sum of enabled multiplication instructions: %d", total);
    }

    private int sumOfMul(String s) {
        return Pattern.compile(MUL_REGEX)
            .matcher(s)
            .results()
            .mapToInt(mr -> Integer.parseInt(mr.group("X")) * Integer.parseInt(mr.group("Y")))
            .sum();
    }
}
