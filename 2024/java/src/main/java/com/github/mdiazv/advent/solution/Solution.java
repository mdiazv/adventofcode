package com.github.mdiazv.advent.solution;

import com.github.mdiazv.advent.lib.Pair;

import java.io.InputStream;

public interface Solution {
    void parseInput(InputStream input);
    default Pair<String, Integer> part1() {
        return new Pair("Solution to part 1 is not implemented", -1);
    }
    default Pair<String, Integer> part2() {
        return new Pair("Solution to part 2 is not implemented", -2);
    }
}
