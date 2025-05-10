package com.github.mdiazv.advent.lib;

import lombok.Data;

import java.util.List;
import java.util.stream.IntStream;

@Data
public class Pair<A, B> {
    private final A a;
    private final B b;

    public static Pair<Integer, Integer> toIntPair(String s) {
        String[] parts = s.split("\\s+", 2);
        return new Pair<>(Integer.parseInt(parts[0]), Integer.parseInt(parts[1]));
    }

    public static <A, B> List<Pair<A, B>> zip(List<A> As, List<B> Bs) {
        return IntStream.range(0, Math.min(As.size(), Bs.size()))
                .mapToObj(k -> new Pair<>(As.get(k), Bs.get(k)))
                .toList();
    }
}
