package com.mdiazv.advent;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.stream.IntStream;
import java.util.function.IntPredicate;

/**
 * --- Day 4: Secure Container ---
 * Find how many passwords meet the criteria
 *
 * https://adventofcode.com/2019/day/4
 */
public class Day4 implements Day {
	public String help() {
		return "Day 4: Crossed Wires - https://adventofcode.com/2019/day/4\n"
			+  "usage: ./advent 4";
	}
	private InputStream getInputStream() {
		return this.getClass().getResourceAsStream("/input/4.txt");
	}
	public static int[] parseRange(InputStream in) { 
		try {
			BufferedReader br = new BufferedReader(new InputStreamReader(in));
			String[] parts = br.readLine().split("-");
			return new int[]{Integer.valueOf(parts[0]), Integer.valueOf(parts[1])};
		} catch (java.io.IOException e) {
			System.err.println("Could not parse range");
			System.exit(1);
			return null;
		}
	}
	public static boolean isValidPassword(int x) {
		String p = String.valueOf(x);
		boolean repeated = false, nondecreasing = true;
		for (int i = 0; i < p.length()-1; i++) {
			repeated = repeated || p.charAt(i) == p.charAt(i+1);
			nondecreasing = nondecreasing && p.charAt(i) <= p.charAt(i+1);
		}
		return repeated && nondecreasing && p.length() == 6;
	}
	public static boolean isStrictPassword(int x) {
		String p = String.valueOf(x);
		int same = 0;
		boolean repeated = false, nondecreasing = true;
		for (int i = 0; i < p.length()-1; i++) {
			if (p.charAt(i) == p.charAt(i+1)) {
				if (same == 0) {
					same = 2;
				} else {
					same++;
				}
			} else {
				if (same == 2) {
					repeated = true;
				}
				same = 0;
			}
			nondecreasing = nondecreasing && p.charAt(i) <= p.charAt(i+1);
		}
		repeated = repeated || same == 2;
		return repeated && nondecreasing && p.length() == 6;
	}
	public static long countValidPasswords(int a, int b, IntPredicate p) {
		return IntStream.rangeClosed(a, b)
			.filter(p)
			.count();
	}
	public void run() {
		int[] range = parseRange(getInputStream());
		System.out.println("Valid passwords in range: "
			+ countValidPasswords(range[0], range[1], Day4::isValidPassword));
		System.out.println("Valid strict passwords in range: "
			+ countValidPasswords(range[0], range[1], Day4::isStrictPassword));
	}
}
