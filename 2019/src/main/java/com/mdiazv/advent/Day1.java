package com.mdiazv.advent;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.function.IntUnaryOperator;
import java.util.stream.Stream;

/**
 * --- Day 1: The Tyranny of the Rocket Equation ---
 * Calculate the required amount of fuel to launch the spacecraft
 *
 * https://adventofcode.com/2019/day/1
 */
public class Day1 implements Day {
	public String help() {
		return "Day One - https://adventofcode.com/2019/day/1\n"
			+  "usage: ./advent 1";
	}
	private InputStream getInputStream() {
		return this.getClass().getResourceAsStream("/input/1.txt");
	}
	private int sumFuel(IntUnaryOperator fuelCalcFn) { 
		InputStream in = getInputStream();
		Stream<String> lines = new BufferedReader(new InputStreamReader(in)).lines();
		return lines
			.mapToInt(Integer::valueOf)
			.map(fuelCalcFn)
			.reduce(0, (a, b) -> a + b);

	}
	public void run() {
		System.out.println("Base fuel: " + sumFuel(Day1::fuel));
		System.out.println("Total fuel: " + sumFuel(Day1::fuelForFuel));
	}
	public static int fuel(int mass) {
		return Math.max(0, Math.floorDiv(mass, 3) - 2);
	}
	public static int fuelForFuel(int f) {
		if (f <= 0) {
			return 0;
		}
		int needs = fuel(f);
		return needs + fuelForFuel(needs);
	}
}
