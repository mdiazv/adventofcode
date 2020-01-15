package com.mdiazv.advent;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.function.IntUnaryOperator;
import java.util.stream.Stream;
import java.util.stream.Collectors;

/**
 * --- Day 2: 1202 Program Alarm ---
 * Run the Intcode program for lunar gravity assist
 *
 * https://adventofcode.com/2019/day/2
 */
public class Day2 implements Day {
	public String help() {
		return "Day 2: 1202 Program Alarm - https://adventofcode.com/2019/day/2\n"
			+  "usage: ./advent 2";
	}
	private InputStream getInputStream() {
		return this.getClass().getResourceAsStream("/input/2.txt");
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
	public static int[] searchForOutput(IntCodeComputer computer, int target) {
		for (int noun = 0; noun < 1000; noun++) {
			for (int verb = 0; verb < 1000; verb++) {
				long output = computer.executeWith(noun, verb);
				if (output == target) {
					return new int[]{noun, verb};
				}
			}
		}
		return new int[]{-1, -1};
	}
	public void run() {
		long[] program = parseProgram(getInputStream());
		IntCodeComputer computer = new IntCodeComputer(program);
		long result = computer.executeWith(12, 2);
		System.out.println("1202 produce: " + result);
		int[] inputs = searchForOutput(computer, 19690720);
		System.out.println("19690720 is produced by: "+(inputs[0] * 100 + inputs[1]));
	}
}
