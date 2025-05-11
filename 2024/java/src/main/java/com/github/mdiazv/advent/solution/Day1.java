package com.github.mdiazv.advent.solution;

import com.github.mdiazv.advent.lib.Pair;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class Day1 implements Solution {
    private List<Integer> first, second;
    public void parseInput(InputStream stream) {
        List<Pair<Integer, Integer>> input = new BufferedReader(new InputStreamReader(stream))
                .lines()
                .map(Pair::toIntPair)
                .toList();
        first = input.stream().map(Pair::a).sorted().toList();
        second = input.stream().map(Pair::b).sorted().toList();
    }

    public Pair<String, Integer> part1() {
        int total_distance = Pair.zip(first, second).stream()
                .mapToInt(p -> Math.abs(p.a() - p.b()))
                .sum();
        return new Pair("Total distance between Lists is: %d", total_distance);
    }

    public Pair<String, Integer> part2() {
        Map<Integer, Integer> count = second.stream()
            .collect(Collectors.groupingBy(Integer::intValue))
            .entrySet().stream()
            .collect(Collectors.toMap(
                    Map.Entry::getKey,
                    e -> e.getValue().size()
            ));
        int similarity_score = first.stream()
            .mapToInt(v -> v * count.getOrDefault(v, 0))
            .sum();

        return new Pair("Similarity score between Lists is: %d", similarity_score);
    }
}
