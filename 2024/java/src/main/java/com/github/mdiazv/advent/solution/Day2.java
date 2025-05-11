package com.github.mdiazv.advent.solution;

import com.github.mdiazv.advent.lib.Pair;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;


record Report(List<Integer> levels) {
    public static Report parse(String line) {
        return new Report(Arrays.stream(line.split(" ")).map(Integer::parseInt).toList());
    }
    public boolean isSafe() {
        // The levels are either all increasing or all decreasing.
        // Any two adjacent levels differ by at least one and at most three.
        int sum = Pair.pairwise(levels)
            .filter(p -> {
                int diff = Math.abs(p.a()- p.b());
                return 1 <= diff && diff <= 3;
            })
            .mapToInt(p -> Integer.compare(p.a(), p.b()))
            .sum();
        return Math.abs(sum) == levels.size()-1;
    }

    public boolean isSafeish() {
        // tolerate a single bad level
        return IntStream.range(0, levels.size())
            .mapToObj(k -> new Report(Stream.concat(
                        levels.subList(0, k).stream(),
                        levels.subList(k+1, levels.size()).stream()
                    ).toList()
                )
            )
            .anyMatch(Report::isSafe);
    }
}

public class Day2 implements Solution {
    private List<Report> reports;
    public void parseInput(InputStream input) {
        reports = new BufferedReader(new InputStreamReader(input))
            .lines()
            .map(Report::parse)
            .toList();
    }

    public Pair<String, Integer> part1() {
        return new Pair("Total number of Safe reports: %d", reports.stream().filter(Report::isSafe).count());
    }
    public Pair<String, Integer> part2() {
        return new Pair("Total number of Safeish reports: %d", reports.stream().filter(Report::isSafeish).count());
    }
}
