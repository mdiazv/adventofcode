package com.mdiazv.advent;

import java.util.Arrays;
import java.util.NoSuchElementException;
import java.util.Vector;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

class IntCodeInstructionMode {
	public static final int POSITION = 0;
	public static final int IMMEDIATE = 1;
	public static final int RELATIVE = 2;
}

interface IntCodeIO {
    void put(long v);
	long consume();
	void reset();
}

class ArrayIntCodeIO implements IntCodeIO {
	private long[] values;
	private int pos;
	public ArrayIntCodeIO(long[] values) {
		this.pos = 0;
		this.values = values;
	}
	public void put(long v) {
	    throw new RuntimeException("Unsupported method ArrayIntCodeIO.put");
	}
	public long consume() {
		return values[pos++];
	}
	public void reset() {
		pos = 0;
	}
}

class VectorIntCodeIO implements IntCodeIO {
	Vector<Long> values;
	private int pos;
	public VectorIntCodeIO() {
		this.pos = 0;
		this.values = new Vector<Long>();
	}
	public void put(long value) {
		values.add(value);
	}
	public long consume() {
		return values.get(pos++);
	}
	public void reset() {
		pos = 0;
		values.setSize(0);
	}
}

class BlockingIntCodeIO implements IntCodeIO {
	private BlockingQueue<Long> queue;
	private long timeoutMs;
	public BlockingIntCodeIO() {
		this.queue = new LinkedBlockingQueue<Long>();
	}
	public BlockingIntCodeIO(long timeoutMs) {
		this();
		this.timeoutMs = timeoutMs;
	}
	public BlockingIntCodeIO(long[] values) {
		this(values, -1);
	}
	public BlockingIntCodeIO(long[] values, long timeoutMs) {
	    this(timeoutMs);
		for (long v : values) {
			queue.add(v);
		}
	}
	public void put(long v) {
		queue.add(v);
	}
	public long consume() {
		while (true) {
			try {
				if (timeoutMs <= 0) {
					return queue.take();
				}
				Long val = queue.poll(timeoutMs, TimeUnit.MILLISECONDS);
				if (val == null) {
					throw new NoSuchElementException("Couldn't consume input before timeout");
				}
				return val;
			} catch (InterruptedException e) {}
		}
	}
	public void reset() {
		queue.clear();
	}
}

class IntCodeComputer {
	public boolean debug;
	private static final int MAX_MEMORY = 640 * 1024; // 640 KB should be enough
	protected int pc;
	protected int relative_base;
	protected boolean exited;
	protected long[] original;
	protected long[] program;
	protected IntCodeIO input;
	protected IntCodeIO output;
	public IntCodeComputer(long[] program) {
		long[] memory = new long[MAX_MEMORY];
		System.arraycopy(program, 0, memory, 0, program.length);
		this.program = memory;
		this.original = Arrays.copyOf(memory, memory.length);
		this.output = new VectorIntCodeIO();
	}
	public IntCodeComputer(long[] program, long[] input) {
		this(program);
		this.input = new ArrayIntCodeIO(input);
	}
	public void execute() {
		pc = 0;
		relative_base = 0;
		exited = false;
		this.output.reset();
		while (!exited) {
			step();
		}
	}
	public Vector<Long> executeWith(long[] input) {
		this.program = Arrays.copyOf(original, original.length);
		this.input = new ArrayIntCodeIO(input);
		execute();
		return this.output();
	}
	public long executeWith(int noun, int verb) {
		program = Arrays.copyOf(original, original.length);
		set(1, IntCodeInstructionMode.IMMEDIATE, noun);
		set(2, IntCodeInstructionMode.IMMEDIATE, verb);
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
		long[] value = new long[3];
		long code = fetch(pc, IntCodeInstructionMode.IMMEDIATE);
		int[] mode = modes(code);
		int op = (int) code % 100;
		if (debug)
			System.out.println("step: pc:"+pc+" rb:"+relative_base+" code:"+code);
		switch (op) {
			case 1: // Addition
				value[0] = fetch(pc+1, mode[0]);
				value[1] = fetch(pc+2, mode[1]);
				if (debug) {
					System.out.println("addition: v0:" + program[pc + 1] + ":" + mode[0] + ":" + value[0]);
					System.out.println("addition: v1:" + program[pc + 2] + ":" + mode[1] + ":" + value[1]);
					System.out.println("addition: v2:" + program[pc + 3] + ":" + mode[2]);
				}
				set(pc + 3, mode[2], value[0]+value[1]);
				pc += 4;
				break;
			case 2: // Product
				value[0] = fetch(pc+1, mode[0]);
				value[1] = fetch(pc+2, mode[1]);
				if (debug) {
					System.out.println("product: v0:" + program[pc + 1] + ":" + mode[0] + ":" + value[0]);
					System.out.println("product: v1:" + program[pc + 2] + ":" + mode[1] + ":" + value[1]);
					System.out.println("product: v2:" + program[pc + 3] + ":" + mode[2]);
				}
				set(pc + 3, mode[2], value[0]*value[1]);
				pc += 4;
				break;
			case 3: // Input
				set(pc + 1, mode[0], input.consume());
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
				pc = (int) (value[0] != 0 ? value[1] : pc + 3);
				break;
			case 6: // Jump if false
				value[0] = fetch(pc+1, mode[0]);
				value[1] = fetch(pc+2, mode[1]);
				pc = (int) (value[0] == 0 ? value[1] : pc + 3);
				break;
			case 7: // Less than
				value[0] = fetch(pc+1, mode[0]);
				value[1] = fetch(pc+2, mode[1]);
				//value[2] = fetch(pc+3, IntCodeInstructionMode.IMMEDIATE);
				set(pc + 3, mode[2], value[0] < value[1] ? 1 : 0);
				pc += 4;
				break;
			case 8: // Equals
				value[0] = fetch(pc+1, mode[0]);
				value[1] = fetch(pc+2, mode[1]);
				//value[2] = fetch(pc+3, IntCodeInstructionMode.IMMEDIATE);
				set(pc + 3, mode[2], value[0] == value[1] ? 1 : 0);
				pc += 4;
				break;
			case 9: // Adjust relative base
				value[0] = fetch(pc+1, mode[0]);
				relative_base += value[0];
				pc += 2;
				break;
			case 99:
				exited = true;
				break;
			default:
				throw new RuntimeException("Unknown opcode: "+op);
		}
	}
	private long fetch(int pos, int mode) {
		switch (mode) {
			case IntCodeInstructionMode.POSITION:
				return fetch((int) program[pos], IntCodeInstructionMode.IMMEDIATE);
			case IntCodeInstructionMode.IMMEDIATE:
				return program[pos];
			case IntCodeInstructionMode.RELATIVE:
				return fetch((int) (relative_base+program[pos]), IntCodeInstructionMode.IMMEDIATE);
			default:
				throw new RuntimeException("Instruction mode "+mode+": Not implemented");
		}
	}
	private void set(int pos, int mode, long val) {
		if (mode == IntCodeInstructionMode.IMMEDIATE) {
			throw new RuntimeException("Invalid IMMEDIATE mode for set parameter");
		}
		long target = fetch(pos, IntCodeInstructionMode.IMMEDIATE);
		if (mode == IntCodeInstructionMode.RELATIVE) {
			target += relative_base;
		}
		if (debug) {
			System.out.println("SET pos:"+target+" val:"+val);
		}
		program[(int) target] = val;
	}
	private int[] modes(long code) {
		return new int[]{(int) (code%1000)/100, (int) (code%10000)/1000, (int) code / 10000};
	}
	public long[] state() {
		return Arrays.copyOf(program, program.length);
	}
	public Vector<Long> output() {
		return ((VectorIntCodeIO) this.output).values;
	}
}

class ThreadedIntCodeComputer extends IntCodeComputer {
	private Thread thread;
	public ThreadedIntCodeComputer(long[] program) {
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
