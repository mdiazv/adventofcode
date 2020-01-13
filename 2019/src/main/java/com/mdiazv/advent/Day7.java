package com.mdiazv.advent;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.stream.Stream;

/**
 * --- Day 7: Amplification Circuit ---
 * Run multiple intcode instances to find best permutation
 *
 * https://adventofcode.com/2019/day/7
 */
public class Day7 implements Day {
    public String help() {
        return "Day 7: Amplification Circuit - https://adventofcode.com/2019/day/7\n"
                +  "usage: ./advent 7";
    }
    private InputStream getInputStream() {
        return this.getClass().getResourceAsStream("/input/7.txt");
    }
    public static int[] parseProgram(InputStream in) {
        try {
            String text = new BufferedReader(new InputStreamReader(in)).readLine();
            return Stream.of(text.split(","))
                    .mapToInt(Integer::valueOf)
                    .toArray();
        } catch (java.io.IOException e) {
            System.err.println("Could not load program");
            System.exit(1);
            return null;
        }
    }
    public static int largestOutputSignal(int[] program, IntegerPermutations p) {
        ThreadedIntCodeComputer[] amps = new ThreadedIntCodeComputer[]{
                new ThreadedIntCodeComputer(program),
                new ThreadedIntCodeComputer(program),
                new ThreadedIntCodeComputer(program),
                new ThreadedIntCodeComputer(program),
                new ThreadedIntCodeComputer(program),
        };
        IntCodeIO[] io = new BlockingIntCodeIO[]{
                new BlockingIntCodeIO(),
                new BlockingIntCodeIO(),
                new BlockingIntCodeIO(),
                new BlockingIntCodeIO(),
                new BlockingIntCodeIO(),
        };
        AtomicInteger best = new AtomicInteger(-1);
        p.forEach(x -> {
            int r = runFeedbackLoop(amps, io, x);
            best.updateAndGet(b -> r > b ? r : b);
        });
        return best.intValue();
    }
    public static int runFeedbackLoop(ThreadedIntCodeComputer[] amps, IntCodeIO[] io, Integer[] permutation) {
        for (int i = 0 ; i < amps.length; i++) {
            io[i].put(permutation[i]);
            amps[i].executeWith(io[i], io[(i+1) % io.length]);
        }
        io[0].put(0);
        for (ThreadedIntCodeComputer amp : amps) {
            amp.join();
        }
        return io[0].consume();
    }
    public void run() {
        int[] program = parseProgram(getInputStream());
        int best = largestOutputSignal(program, new IntegerPermutations(new Integer[]{0,1,2,3,4}));
        System.out.println(("Largest output signal: "+best));
        best = largestOutputSignal(program, new IntegerPermutations(new Integer[]{5,6,7,8,9}));
        System.out.println(("Largest output signal with feedback: "+best));
    }
}

class IntegerPermutations {
    private Integer[] seq;
    private boolean[] used;
    public IntegerPermutations(Integer[] seq) {
        this.seq = seq.clone();
    }
    public void forEach(Consumer<Integer[]> cb) {
        Integer[] perm = new Integer[seq.length];
        this.used = new boolean[seq.length];
        permute(perm, 0, cb);
    }
    private void permute(Integer[]perm, int i, Consumer<Integer[]> cb) {
        if (i == perm.length) {
            cb.accept(perm);
            return;
        }
        for (int k = 0; k < perm.length; k++) {
            if (!used[k]) {
                used[k] = true;
                perm[i] = seq[k];
                permute(perm,  i+1, cb);
                used[k] = false;
            }
        }
    }
}