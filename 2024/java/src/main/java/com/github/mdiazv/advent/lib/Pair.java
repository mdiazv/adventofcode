package com.github.mdiazv.advent.lib;

import lombok.Data;

import java.util.Iterator;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public record Pair<A, B>(A a, B b) {
    public static Pair<Integer, Integer> toIntPair(String s) {
        String[] parts = s.split("\\s+", 2);
        return new Pair(Integer.parseInt(parts[0]), Integer.parseInt(parts[1]));
    }

    public static <A, B> Pair<A, B> of(A a, B b) {
        return new Pair(a, b);
    }

    public static <A, B> List<Pair<A, B>> zip(List<A> As, List<B> Bs) {
        return IntStream.range(0, Math.min(As.size(), Bs.size()))
            .mapToObj(k -> new Pair<>(As.get(k), Bs.get(k)))
            .toList();
    }

    public static <A, B> Stream<Pair<A, B>> zip(Stream<A> As, Stream<B> Bs) {
        Iterator<A> a = As.iterator();
        return Bs.filter(x -> a.hasNext()).map(b -> new Pair<>(a.next(), b));
    }

    public static <A> Stream<Pair<A, A>> pairwise(List<A> As) {
        return Pair.zip(As.stream(), As.stream().skip(1));
    }
}
