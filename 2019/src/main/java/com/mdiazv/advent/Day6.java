package com.mdiazv.advent;

import javax.lang.model.element.Element;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;
import java.util.function.IntUnaryOperator;
import java.util.stream.Stream;
import java.util.stream.Collectors;

/**
 * --- Day 6: Universal Orbit Map ---
 * Calculate orbit dependencies
 *
 * https://adventofcode.com/2019/day/6
 */
public class Day6 implements Day {
    public String help() {
        return "Day 6: Universal Orbit Map - https://adventofcode.com/2019/day/6\n"
                +  "usage: ./advent 6";
    }
    private InputStream getInputStream() {
        return this.getClass().getResourceAsStream("/input/6.txt");
    }
    public static String[][] parseUniverse(InputStream in) {
        BufferedReader input = new BufferedReader(new InputStreamReader(in));
        return input.lines()
                .map(line -> line.split("\\)"))
                .toArray(String[][]::new);
    }
    public void run() {
        String[][] universe = parseUniverse(getInputStream());
        OrbitTree tree = new OrbitTree();
        for (String[] dep : universe) {
            tree.addOrbit(dep[0], dep[1]);
        }
        System.out.println("Total orbits: "+tree.orbits());
        System.out.println("Distance to Santa's orbit: "+tree.distanceToOrbit("YOU", "SAN"));
    }
}

class OrbitTree {
    class Node {
        public String name;
        public String parent;
        public Vector<String> children;
        public int directOrbits;
        public int indirectOrbits;
        public Node(String name) {
            this.name = name;
            this.children = new Vector<String>();
        }
        public int orbits() {
            return directOrbits + indirectOrbits;
        }
    }
    private Map<String, Node> t;
    public OrbitTree() {
        this.t = new HashMap<String, Node>();
    }
    public void addOrbit(String inner, String outer) {
        t.computeIfAbsent(inner, _dummy -> new Node(inner));
        t.computeIfAbsent(outer, _dummy -> new Node(outer));
        Node i = t.get(inner);
        Node o = t.get(outer);
        o.parent = inner;
        i.children.add(outer);
    }
    private void calcOrbits(String root) {
        Node n = t.get(root);
        if (n.parent != null) {
            Node p = t.get(n.parent);
            n.directOrbits = 1;
            n.indirectOrbits = p.orbits();
        }
        n.children.forEach(this::calcOrbits);
    }
    public int orbits() {
        calcOrbits("COM");
        return t.values().stream()
                .mapToInt(n -> n.orbits())
                .sum();
    }
    public int distanceToOrbit(String a, String b) {
        calcOrbits("COM");
        Node na = t.get(a);
        Node nb = t.get(b);
        Node common = commonAncestor(a, b);
        return na.orbits() + nb.orbits() - 2 * common.orbits() - 2;
    }
    private Stream<Node> ancestors(String s) {
        Stream.Builder<Node> b = Stream.builder();
        Node n = t.get(s);
        while (n != null) {
            b.accept(n);
            n = t.get(n.parent);
        }
        return b.build();
    }
    private Node commonAncestor(String a, String b) {
        Set<Node> s = ancestors(a).collect(Collectors.toSet());
        Optional<Node> common = ancestors(b)
                .filter(s::contains)
                .findFirst();
        return common.orElse(null);
    }
}
