package com.github.mdiazv.advent;

import com.github.mdiazv.advent.lib.AdventWebsite;
import com.github.mdiazv.advent.lib.Pair;
import com.github.mdiazv.advent.solution.Day1;
import com.github.mdiazv.advent.solution.Solution;

import java.io.InputStream;
import java.util.Arrays;

/**
 * Hello world!
 */
public class Main {
    private static String ESC = "\u001B";
    private static String BOLD = ESC + "[1m";
    private static String RESET = ESC + "[0m";

    public static void main(String[] args) {
        AdventWebsite advent = AdventWebsite.load();

        int day = getDay(args);
        InputStream input = advent.getInput(day);
        Solution solution = solutionForDay(1);

        solution.parseInput(input);
        Pair<String,Integer> result1 = solution.part1();
        Pair<String,Integer> result2 = solution.part2();

        System.out.format(BOLD + "Advent of Code - Day %d\n" + RESET, day);
        System.out.format(result1.getA()+"\n", result1.getB());
        System.out.format(result2.getA()+"\n", result2.getB());
        System.out.println(" --- --- --- --- ---");
    }
    private static int getDay(String[] args) {
        return 1;
    }
    private static Solution solutionForDay(int k) {
        return new Day1();
    }

}
