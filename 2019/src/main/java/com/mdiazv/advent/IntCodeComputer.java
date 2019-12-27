package com.mdiazv.advent;

import java.util.Arrays;
import java.util.Vector;

class IntCodeInstructionMode {
	public static final boolean POSITION = false;
	public static final boolean IMMEDIATE = true;
}

class IntCodeInput {
	private int[] values;
	private int pos;
	public IntCodeInput(int[] values) {
		this.pos = 0;
		this.values = values;
	}
	public int next() {
		return values[pos++];
	}
}

class IntCodeOutput {
	Vector<Integer> values;
	public IntCodeOutput() {
		this.values = new Vector<Integer>();
	}
	public void emit(int value) {
		values.add(value);
	}
	public String toString() {
		return values.toString();
	}
}

class IntCodeComputer {
	private int pc;
	private boolean exited;
	private int[] original;
	private int[] program;
	private IntCodeInput input;
	private IntCodeOutput output;
	public IntCodeComputer(int[] program) {
		this.program = Arrays.copyOf(program, program.length);
		this.original = Arrays.copyOf(program, program.length);
		this.output = new IntCodeOutput();
	}
	public IntCodeComputer(int[] program, int[] input) {
		this(program);
		this.input = new IntCodeInput(input);
	}
	public void execute() {
		pc = 0;
		exited = false;
		this.output = new IntCodeOutput();
		while (!exited) {
			step();
		}
	}
	public String executeWith(int[] input) {
		this.program = Arrays.copyOf(original, original.length);
		this.input = new IntCodeInput(input);
		execute();
		return this.output();
	}
	public int executeWith(int noun, int verb) {
		program = Arrays.copyOf(original, original.length);
		set(1, noun);
		set(2, verb);
		try {
			execute();
		} catch (ArrayIndexOutOfBoundsException e) {
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
		int[] value = new int[3];
		int code = fetch(pc, IntCodeInstructionMode.IMMEDIATE);
		boolean[] mode = modes(code);
		int op = code % 100;
		switch (op) {
			case 1: // Addition
				value[0] = fetch(pc+1, mode[0]);
				value[1] = fetch(pc+2, mode[1]);
				value[2] = fetch(pc+3, IntCodeInstructionMode.IMMEDIATE);
				set(value[2], value[0]+value[1]);
				pc += 4;
				break;
			case 2: // Product
				value[0] = fetch(pc+1, mode[0]);
				value[1] = fetch(pc+2, mode[1]);
				value[2] = fetch(pc+3, IntCodeInstructionMode.IMMEDIATE);
				set(value[2], value[0]*value[1]);
				pc += 4;
				break;
			case 3: // Input
				value[0] = fetch(pc+1, IntCodeInstructionMode.IMMEDIATE);
				set(value[0], input.next());
				pc += 2;
				break;
			case 4: // Output
				value[0] = fetch(pc+1, mode[0]);
				output.emit(value[0]);
				pc += 2;
				break;
			case 5: // Jump if true
				value[0] = fetch(pc+1, mode[0]);
				value[1] = fetch(pc+2, mode[1]);
				pc = value[0] != 0 ? value[1] : pc + 3;
				break;
			case 6:
				value[0] = fetch(pc+1, mode[0]);
				value[1] = fetch(pc+2, mode[1]);
				pc = value[0] == 0 ? value[1] : pc + 3;
				break;
			case 7:
				value[0] = fetch(pc+1, mode[0]);
				value[1] = fetch(pc+2, mode[1]);
				value[2] = fetch(pc+3, IntCodeInstructionMode.IMMEDIATE);
				set(value[2], value[0] < value[1] ? 1 : 0);
				pc += 4;
				break;
			case 8:
				value[0] = fetch(pc+1, mode[0]);
				value[1] = fetch(pc+2, mode[1]);
				value[2] = fetch(pc+3, IntCodeInstructionMode.IMMEDIATE);
				set(value[2], value[0] == value[1] ? 1 : 0);
				pc += 4;
				break;
			case 99:
				exited = true;
				break;
			default:
				throw new RuntimeException("Unknown opcode: "+op);
		}
	}
	private int fetch(int pos, boolean immediate) {
		return immediate ? program[pos] : fetch(program[pos], true);
	}
	private void set(int pos, int val) {
		program[pos] = val;
	}
	private boolean[] modes(int code) {
		return new boolean[]{(code%1000)/100 > 0, (code%10000)/1000 > 0, code / 10000 > 0};
	}
	public int[] state() {
		return Arrays.copyOf(program, program.length);
	}
	public String output() {
		return this.output.values.toString();
	}
}
