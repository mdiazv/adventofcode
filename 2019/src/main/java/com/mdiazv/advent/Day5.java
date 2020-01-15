package com.mdiazv.advent;

import javax.lang.model.element.Element;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;
import java.util.function.IntUnaryOperator;
import java.util.stream.Stream;
import java.util.stream.Collectors;

/**
 * --- Day 5: Sunny with a Chance of Asteroids ---
 * Improve the intcode computer and run diagnostics
 *
 * https://adventofcode.com/2019/day/5
 */
public class Day5 implements Day {
	public String help() {
		return "Day 5: Sunny with a Chance of Asteroids - https://adventofcode.com/2019/day/5\n"
			+  "usage: ./advent 5";
	}
	private InputStream getInputStream() {
		return this.getClass().getResourceAsStream("/input/5.txt");
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
	public void run() {
		long[] program = parseProgram(getInputStream());
		IntCodeComputer computer = new IntCodeComputer(program);
		Vector<Long> output = computer.executeWith(new long[]{1});
		System.out.println("Diagnostics output for ID 1: "+ output);
		output = computer.executeWith(new long[]{5});
		System.out.println("Diagnostics output for ID 5: "+ output);
	}
}
