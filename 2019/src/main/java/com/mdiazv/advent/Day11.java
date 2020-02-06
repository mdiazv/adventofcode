package com.mdiazv.advent;

import java.awt.*;
import java.awt.geom.Point2D;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;
import java.util.stream.Stream;

/**
 * --- Day 11: Space Police ---
 * Run IntCode Paint program on the hull painting robot
 *
 * https://adventofcode.com/2019/day/9
 */
public class Day11 implements Day {
    public String help() {
        return "Day 11: Space Police - https://adventofcode.com/2019/day/11\n"
                +  "usage: ./advent 11";
    }
    private InputStream getInputStream() {
        return this.getClass().getResourceAsStream("/input/11.txt");
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
    public static void paintLicense(long[] program, Integer starting) {
        InfiniteGrid<Integer> hull = renderHull(program, starting);
        System.out.println(("Tiles painted at least once: " + hull.size()));
        String license =  hull.render(Map.of(0, " ", 1, "#"));
        System.out.println(license);
    }
    public static InfiniteGrid<Integer> renderHull(long[] program, Integer starting) {
        InfiniteGrid<Integer> hull = new InfiniteGrid<Integer>(0);
        hull.set(new Point(0, 0), starting);
        HullPaintingRobot robot = new HullPaintingRobot(program);
        robot.paint(hull);
        return hull;
    }
    public void run() {
        long[] program = parseProgram(getInputStream());
        paintLicense(program, 0);
        paintLicense(program, 1);
    }
}

class InfiniteGrid<T> {
    private Map<Point, T> grid;
    private Rectangle boundingBox;
    private T defVal;
    public InfiniteGrid(T defaultValue) {
        this.grid = new TreeMap<Point, T>(
                Comparator
                    .comparingDouble(Point::getY)
                    .thenComparingDouble(Point::getX)
        );
        this.boundingBox = new Rectangle();
        this.defVal = defaultValue;
    }
    public T get(Point p) {
        return grid.getOrDefault(p, defVal);
    }
    public T get(int x, int y) {
        return get(new Point(x, y));
    }
    public void set(Point p, T val) {
        grid.put(p, val);
        boundingBox.add(p);
    }
    public void set(int x, int y, T val) {
        set(new Point(x, y), val);
    }
    public int size() {
        return grid.size();
    }
    public String toString() {
        return boundingBox.toString() + "\n" + grid.toString();
    }
    public String render(Map <T, String> mapping) {
        Point upperLeft = boundingBox.getLocation();
        Dimension size = boundingBox.getSize();
        StringBuilder sb = new StringBuilder();
        Point p = new Point();
        for (int i = 0; i <= size.getHeight(); i++) {
            for (int j = 0; j <= size.getWidth(); j++) {
                p.setLocation(upperLeft.x+j, upperLeft.y+(size.height-i));
                T v = grid.getOrDefault(p, defVal);
                sb.append(mapping.get(v));
            }
            sb.append("\n");
        }
        return sb.toString();
    }
}


class HullPaintingRobot {
    private ThreadedIntCodeComputer computer;
    private IntCodeIO input;
    private IntCodeIO output;
    public HullPaintingRobot(long[] program) {
        this.computer = new ThreadedIntCodeComputer(program);
        this.input = new BlockingIntCodeIO();
        this.output = new BlockingIntCodeIO(100);
    }
    public void paint(InfiniteGrid<Integer> grid) {
        Point pos = new Point(0, 0);
        Point dir = new Point(0, 1);
        input.reset();
        output.reset();
        computer.executeWith(input, output);
        try {
            while (!computer.done()) {
                Integer current = grid.get(pos);
                input.put(current.longValue());
                Long color = output.consume();
                Long rotate = output.consume();
                grid.set(pos, color.intValue());
                rotate(dir, rotate.intValue());
                pos = new Point(pos.x + dir.x, pos.y + dir.y);
            }
        } catch (NoSuchElementException e) {
            System.out.println(e);
        }
        computer.join();
    }
    private static void rotate(Point dir, int r) {
        if (r == 0) turnLeft(dir);
        else        turnRight(dir);
    }
    private static void turnLeft(Point dir) {
        dir.setLocation(-dir.y, dir.x);
    }
    private static void turnRight(Point dir) {
        dir.setLocation(dir.y, -dir.x);
    }
}
