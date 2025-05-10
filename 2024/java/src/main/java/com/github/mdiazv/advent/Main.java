package com.github.mdiazv.advent;

import com.github.mdiazv.advent.lib.AdventWebsite;
import com.github.mdiazv.advent.lib.Pair;
import com.github.mdiazv.advent.solution.Solution;

import java.io.InputStream;

/**
 * Hello world!
 */
public class Main {
    private static String ESC = "\u001B";
    private static String BOLD = ESC + "[1m";
    private static String RESET = ESC + "[0m";

    private static void usage() {
        System.err.println("Usage: ./advent.sh <day>");
        System.err.println("\tSolves https://adventofcode.com/2024 puzzle for given day");
        System.exit(1);
    }

    public static void main(String[] args) {
        int day = getDay(args);
        System.out.format(BOLD + "Advent of Code - Day %d\n" + RESET, day);

        Solution solution = solutionForDay(day);
        AdventWebsite advent = AdventWebsite.load();
        InputStream input = advent.getInput(day);
        solution.parseInput(input);

        Pair<String,Integer> result1 = solution.part1();
        System.out.format(result1.getA()+"\n", result1.getB());

        Pair<String,Integer> result2 = solution.part2();
        System.out.format(result2.getA()+"\n", result2.getB());

        System.out.println(" --- --- --- --- ---");
    }

    private static int getDay(String[] args) {
        if (args.length < 1) {
            usage();
        }
        try {
            return Integer.parseInt(args[0]);
        } catch (NumberFormatException e) {
            usage();
        }
        return -1;
    }

    private static Solution solutionForDay(int k) {
        try {
            String name = String.format("com.github.mdiazv.advent.solution.Day%d", k);
            return Solution.class.cast(Class.forName(name).newInstance());
        } catch (InstantiationException | IllegalAccessException | ClassNotFoundException e) {
            System.err.format("Solution for day %d was not found\n", k);
            System.exit(2);
        }
        return null;
    }

}
