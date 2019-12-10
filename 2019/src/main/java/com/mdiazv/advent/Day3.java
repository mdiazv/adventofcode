package com.mdiazv.advent;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.function.IntUnaryOperator;
import java.util.stream.Stream;
import java.util.stream.Collectors;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;
import java.util.Vector;
import java.awt.Point;

/**
 * --- Day 3: Crossed Wires ---
 * Find the intersection of two wires
 *
 * https://adventofcode.com/2019/day/3
 */
public class Day3 implements Day {
	public String help() {
		return "Day 3: Crossed Wires - https://adventofcode.com/2019/day/3\n"
			+  "usage: ./advent 3";
	}
	private InputStream getInputStream() {
		return this.getClass().getResourceAsStream("/input/3.txt");
	}
	public static Wire[] parseWires(InputStream in) { 
		try {
			BufferedReader br = new BufferedReader(new InputStreamReader(in));
			Wire w1 = new Wire(br.readLine());
			Wire w2 = new Wire(br.readLine());
			return new Wire[]{w1, w2};
		} catch (java.io.IOException e) {
			System.err.println("Could not parse wires");
			System.exit(1);
			return null;
		}
	}
	private static final Point ZERO = new Point(0, 0);
	public static int distance(Point p) {
		return Math.abs(p.x) + Math.abs(p.y);
	}
	public static int closestIntersection(Wire w1, Wire w2) {
		return w1.intersections(w2)
			.stream()
			.filter(p -> !p.equals(ZERO))
			.mapToInt(Day3::distance)
			.min()
			.orElse(-1);
	}
	public static int cheapestIntersection(Wire w1, Wire w2) {
		return w1.intersections(w2)
			.stream()
			.filter(p -> !p.equals(ZERO))
			.mapToInt(p -> w1.distanceTo(p) + w2.distanceTo(p))
			.min()
			.orElse(-1);
	}
	public void run() {
		Wire[] wires = parseWires(getInputStream());
		System.out.println("Closest intersection: " + closestIntersection(wires[0], wires[1]));
		System.out.println("Cheapest intersection: " + cheapestIntersection(wires[0], wires[1]));
	}
}

class Wire {
	public int length;
	public Set<Point> points;
	public Vector<Point> path;
	private static final Map<String,Point> dir = Map.of(
		"R", new Point( 1, 0),
		"L", new Point(-1, 0),
		"U", new Point( 0, 1),
		"D", new Point( 0,-1)
	);
	public Wire(String moves) {
		Point pos = new Point(0, 0);
		Vector<Point> path = new Vector<Point>();
		path.add(pos);
		for (String move : moves.split(",")) {
			String d = move.substring(0, 1);
			int steps = Integer.valueOf(move.substring(1));
			Point step = dir.get(d);
			for (int i = 0; i < steps; i++) {
				pos = new Point(pos.x + step.x, pos.y + step.y);
				path.add(pos);
			}
			this.length += steps;
		}
		this.path = path;
		this.points = new HashSet<Point>(path);
	}
	public String toString() {
		return "Wire " + path.toString();
	}
	public Set<Point> intersections(Wire other) {
		Set<Point> s = new HashSet<Point>(this.points);
		s.retainAll(other.points);
		return s;
	}
	public int distanceTo(Point p) {
		return path.indexOf(p);
	}
}
