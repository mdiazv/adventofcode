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
		return "Day Two - https://adventofcode.com/2019/day/2\n"
			+  "usage: ./advent 2";
	}
	private InputStream getInputStream() {
		return this.getClass().getResourceAsStream("/input/2.txt");
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
	public static int[] searchForOutput(IntCodeComputer computer, int target) {
		for (int noun = 0; noun < 1000; noun++) {
			for (int verb = 0; verb < 1000; verb++) {
				int output = computer.executeWith(noun, verb);
				if (output == target) {
					return new int[]{noun, verb};
				}
			}
		}
		return new int[]{-1, -1};
	}
	public void run() {
		int[] program = parseProgram(getInputStream());
		IntCodeComputer computer = new IntCodeComputer(program);
		int result = computer.executeWith(12, 2);
		System.out.println("1202 produce: " + result);
		int[] inputs = searchForOutput(computer, 19690720);
		System.out.println("19690720 is produced by: "+(inputs[0] * 100 + inputs[1]));
	}
}

class IntCodeComputer {
	private int pc;
	private boolean exited;
	private int[] original;
	private int[] program;
	public IntCodeComputer(int[] program) {
		this.program = Arrays.copyOf(program, program.length);
		this.original = Arrays.copyOf(program, program.length);
	}
	public void execute() {
		pc = 0;
		exited = false;
		while (!exited) {
			step();
		}
	}
	public int executeWith(int noun, int verb) {
		program = Arrays.copyOf(original, original.length);
		set(1, noun);
		set(2, verb);
		try {
			execute();
		} catch (java.lang.ArrayIndexOutOfBoundsException e) {
			return -1;
		}
		return program[0];
	}
	public String toString() {
		return "Computer\n"
			+  "pc: "+pc+"\n"
			+  "program: "+Arrays.toString(program);
	}
	private void step() {
		int a, b;
		int op = fetch(pc);
		switch (op) {
			case 1:
				a = fetchIndirect(pc+1);
				b = fetchIndirect(pc+2);
				set(fetch(pc+3), a+b);
				break;
			case 2:
				a = fetchIndirect(pc+1);
				b = fetchIndirect(pc+2);
				set(fetch(pc+3), a*b);
				break;
			case 99:
				exited = true;
				break;
			default:
				throw new RuntimeException("Unknown opcode: "+op);
		}
		pc += 4;
	}
	private int fetch(int pos) {
		return program[pos];
	}
	private int fetchIndirect(int pos) {
		return fetch(fetch(pos));
	}
	private void set(int pos, int val) {
		program[pos] = val;
	}
	public int[] state() {
		return Arrays.copyOf(program, program.length);
	}
}
