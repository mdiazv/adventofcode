package com.mdiazv.advent;

import java.util.Arrays;
import java.util.Vector;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

class IntCodeInstructionMode {
	public static final boolean POSITION = false;
	public static final boolean IMMEDIATE = true;
}

interface IntCodeIO {
    void put(int v);
	int consume();
	void reset();
}

class ArrayIntCodeIO implements IntCodeIO {
	private int[] values;
	private int pos;
	public ArrayIntCodeIO(int[] values) {
		this.pos = 0;
		this.values = values;
	}
	public void put(int v) {
	    throw new RuntimeException("Unsupported method ArrayIntCodeIO.put");
	}
	public int consume() {
		return values[pos++];
	}
	public void reset() {
		pos = 0;
	}
}

class VectorIntCodeIO implements IntCodeIO {
	Vector<Integer> values;
	private int pos;
	public VectorIntCodeIO() {
		this.pos = 0;
		this.values = new Vector<Integer>();
	}
	public void put(int value) {
		values.add(value);
	}
	public int consume() {
		return values.get(pos++);
	}
	public void reset() {
		pos = 0;
		values.setSize(0);
	}
}

class BlockingIntCodeIO implements IntCodeIO {
	private BlockingQueue<Integer> queue;
	public BlockingIntCodeIO() {
		this.queue = new LinkedBlockingQueue<Integer>();
	}
	public BlockingIntCodeIO(int[] values) {
		this();
		for (int v : values) {
			queue.add(v);
		}
	}
	public void put(int v) {
		queue.add(v);
	}
	public int consume() {
		while (true) {
			try {
				return queue.take();
			} catch (InterruptedException e) {}
		}
	}
	public void reset() {
		queue.clear();
	}
}

class IntCodeComputer {
	protected int pc;
	protected boolean exited;
	protected int[] original;
	protected int[] program;
	protected IntCodeIO input;
	protected IntCodeIO output;
	public IntCodeComputer(int[] program) {
		this.program = Arrays.copyOf(program, program.length);
		this.original = Arrays.copyOf(program, program.length);
		this.output = new VectorIntCodeIO();
	}
	public IntCodeComputer(int[] program, int[] input) {
		this(program);
		this.input = new ArrayIntCodeIO(input);
	}
	public void execute() {
		pc = 0;
		exited = false;
		this.output.reset();
		while (!exited) {
			step();
		}
	}
	public Vector<Integer> executeWith(int[] input) {
		this.program = Arrays.copyOf(original, original.length);
		this.input = new ArrayIntCodeIO(input);
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
	protected void step() {
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
				set(value[0], input.consume());
				pc += 2;
				break;
			case 4: // Output
				value[0] = fetch(pc+1, mode[0]);
				output.put(value[0]);
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
	public Vector<Integer> output() {
		return ((VectorIntCodeIO) this.output).values;
	}
}

class ThreadedIntCodeComputer extends IntCodeComputer {
	private Thread thread;
	public ThreadedIntCodeComputer(int[] program) {
		super(program);
	}
	public void executeWith(IntCodeIO input, IntCodeIO output) {
		this.program = Arrays.copyOf(original, original.length);
		this.input = input;
		this.output = output;
		this.thread = new Thread(this::execute);
		this.thread.start();
	}
	public boolean done() {
		return this.thread != null && !this.thread.isAlive();
	}
	public void join() {
		try {
			thread.join();
			this.thread = null;
		} catch (InterruptedException e) {
			if (thread.isAlive()) {
				join();
			}
		}
	}
	public void execute() {
		pc = 0;
		exited = false;
		while (!exited) {
			step();
		}
	}
}
