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
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * --- Day 12: The N-Body Problem ---
 * Track trajectories for Jupiter Moons
 *
 * https://adventofcode.com/2019/day/12
 */
public class Day12 implements Day {
    public String help() {
        return "Day 11: Space Police - https://adventofcode.com/2019/day/12\n"
                +  "usage: ./advent 12";
    }
    private InputStream getInputStream() {
        return this.getClass().getResourceAsStream("/input/12.txt");
    }
    public static Moon[] parseProgram(InputStream in) {
        return new BufferedReader(new InputStreamReader(in))
            .lines()
            .map(Moon::new)
            .toArray(Moon[]::new);
    }
    public void run() {
        Moon[] moons = parseProgram(getInputStream());
        Simulator sim = new VectorSimulator(moons);
        long energy = sim.totalEnergy(2772);
        System.out.println("Total Energy after 2772 steps: "+energy);
        IndividualSimulator isim = new IndividualSimulator(moons);
        long cycle = isim.cycleLength();
        System.out.println("First repetition is: " +cycle);
    }
}

interface Simulator {
    public void simulate(int steps);
    public long totalEnergy(int steps);
}

class IndividualSimulator {
    private Moon[] moons;
    private int[][] Ps;
    private int[][] Vs;
    public IndividualSimulator(String moons) {
        this(moons
                .lines()
                .map(Moon::new)
                .toArray(Moon[]::new)
        );
    }
    public IndividualSimulator(Moon[] moons) {
        Ps = new int[3][moons.length];
        Vs = new int[3][moons.length];
        for (int k = 0; k < 3; k++) {
            for (int j = 0; j < moons.length; j++) {
                Ps[k][j] = moons[j].position()[k];
                Vs[k][j] = moons[j].velocity()[k];
            }
        }
    }
    private long gcd(long a, long b) {
        if (b == 0) return a;
        return gcd(b, a % b);
    }
    private long lcm(long a, long b) {
        return a * b / gcd(a, b);
    }
    public long cycleLength() {
        long x = cycleLength(0);
        long y = cycleLength(1);
        long z = cycleLength(2);
        return lcm(z, lcm(x, y));
    }
    private int cycleLength(int k) {
        int cycle = 0;
        int[][] cPs = new int[3][];
        int[][] cVs = new int[3][];
        for (int i = 0; i < 3; i++) {
            cPs[i] = Ps[i].clone();
            cVs[i] = Vs[i].clone();
        }
        do {
            step(cPs[k], cVs[k]);
            cycle++;
        } while (!Arrays.deepEquals(cPs, Ps) || !Arrays.deepEquals(cVs, Vs));
        return cycle;
    }
    private void step(int[] Ps, int[] Vs) {
        for (int i = 0; i < Ps.length; i++) {
            for (int j = 0; j < Ps.length; j++) {
                if (i != j) {
                    int d = Ps[j] - Ps[i];
                    Vs[i] += d != 0 ? d / Math.abs(d) : 0;
                }
            }
        }
        for (int i = 0; i < Ps.length; i++) {
            Ps[i] += Vs[i];
        }
    }
}

class VectorSimulator implements Simulator{
    private static int PRINT_EVERY = -1;
    private Moon[] moons;
    public VectorSimulator(String moons) {
        this(moons
            .lines()
            .map(Moon::new)
            .toArray(Moon[]::new)
        );
    }
    public VectorSimulator(Moon[] moons) {
        this.moons = moons;
    }
    public void simulate(int steps) {
        if (PRINT_EVERY > 0) {
            System.out.println("After 0 steps:");
            System.out.println(this);
        }
        for (int i = 0; i < steps; i++) {
            if (i > 0 && PRINT_EVERY > 0 && i % PRINT_EVERY == 0) {
                System.out.println("After " + i + " steps:");
                System.out.println(this);
            }
            step();
        }
        if (PRINT_EVERY > 0) {
            System.out.println("After " + steps + " steps:");
            System.out.println(this);
        }
    }
    public long totalEnergy(int steps) {
        simulate(steps);
        return Stream.of(moons)
                .mapToLong(Moon::energy)
                .sum();
    }
    private void step() {
        gravityStep();
        velocityStep();
    }
    private void gravityStep() {
        for (Moon m : moons) {
            for (Moon o : moons) {
                if (m != o) {
                    m.applyGravity(o);
                }
            }
        }
    }
    private void velocityStep() {
        for (Moon m : moons) {
            m.applyVelocity();
        }
    }
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (Moon m : moons) {
            sb.append(m.toString());
            sb.append("\n");
        }
        return sb.toString();
    }
}

class Moon {
    private Vector3D pos;
    private Vector3D vel;
    public Moon(String pos) {
        this(Vector3D.parse(pos));
    }
    public Moon(Vector3D pos) {
        this.pos = pos;
        this.vel = new Vector3D();
    }
    public int[] position() {
        return pos.toArray();
    }
    public int[] velocity() {
        return vel.toArray();
    }
    public void applyGravity(Moon other) {
        Vector3D diff = other.pos.subtract(pos);
        vel.translate(diff.sign());
    }
    public void applyVelocity() {
        pos.translate(vel);
    }
    public long potentialEnergy() {
        return pos.absoluteSum();
    }
    public long kineticEnergy() {
        return vel.absoluteSum();
    }
    public long energy() {
        return potentialEnergy() * kineticEnergy();
    }
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("pos=");
        sb.append(pos.toString());
        sb.append(", vel=");
        sb.append(vel.toString());
        return sb.toString();
    }
}

class Vector3D {
    int x, y, z;
    private static Pattern parsePattern = Pattern.compile("<x=([+-]?\\d+), y=([+-]?\\d+), z=([+-]?\\d+)>");
    public Vector3D(int x, int y, int z) {
        this.x = x; this.y = y; this.z = z;
    }
    public Vector3D() {
        this(0, 0 ,0);
    }
    public int[] toArray() {
        return new int[]{x, y, z};
    }
    public static Vector3D parse(String s) {
        Matcher m = parsePattern.matcher(s);
        m.matches();
        return new Vector3D(
            Integer.parseInt(m.group(1)),
            Integer.parseInt(m.group(2)),
            Integer.parseInt(m.group(3))
        );
    }
    public Vector3D add(Vector3D other) {
        return add(this, other);
    }
    public Vector3D subtract(Vector3D other) {
        return subtract(this, other);
    }
    public static Vector3D add(Vector3D a, Vector3D b) {
        return new Vector3D(a.x+b.x, a.y+b.y, a.z+b.z);
    }
    public static Vector3D subtract(Vector3D a, Vector3D b) {
        return new Vector3D(a.x-b.x, a.y-b.y, a.z-b.z);
    }
    public Vector3D translate(Vector3D d) {
        x += d.x; y += d.y; z += d.z;
        return this;
    }
    public Vector3D sign() {
        x = x != 0 ? x / Math.abs(x) : 0;
        y = y != 0 ? y / Math.abs(y) : 0;
        z = z != 0 ? z / Math.abs(z) : 0;
        return this;
    }
    public long absoluteSum() {
        return Math.abs(x) + Math.abs(y) + Math.abs(z);
    }
    public String toString() {
        return "<x="+x+", y="+y+", z="+z+">";
    }
}
