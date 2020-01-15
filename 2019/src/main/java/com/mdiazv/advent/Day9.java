package com.mdiazv.advent;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Vector;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;
import java.util.stream.Stream;

/**
 * --- Day 9: Sensor Boost ---
 * Run IntCode BOOST program with large numbers and large memory
 *
 * https://adventofcode.com/2019/day/9
 */
public class Day9 implements Day {
    public String help() {
        return "Day 9: Sensor Boost - https://adventofcode.com/2019/day/9\n"
                +  "usage: ./advent 9";
    }
    private InputStream getInputStream() {
        return this.getClass().getResourceAsStream("/input/9.txt");
    }
    public static long[] parseProgram(InputStream in) {
        try {
            String text = new BufferedReader(new InputStreamReader(in)).readLine();
            return Stream.of(text.split(","))
                    .mapToLong(Long::valueOf)
                    .toArray();
        } catch (java.io.IOException e) {
            System.err.println("Could not load program");
            System.exit(1);
            return null;
        }
    }
    public static Vector<Long> getBoostKeyCode(long[] program, long input) {
        IntCodeComputer computer = new IntCodeComputer(program);
        return computer.executeWith(new long[]{input});
    }
    public void run() {
        long[] program = parseProgram(getInputStream());
        Vector<Long> key = getBoostKeyCode(program, 1);
        System.out.println(("BOOST key code test "+key));
        key = getBoostKeyCode(program, 2);
        System.out.println(("BOOST key code "+key));
    }
}
