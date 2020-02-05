package com.mdiazv.advent;

import javax.swing.*;
import javax.swing.text.html.Option;
import java.awt.*;
import java.awt.geom.Point2D;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * --- Day 10: Monitoring Station ---
 * Find the best place for and asteroid monitoring station
 *
 * https://adventofcode.com/2019/day/10
 */
public class Day10 implements Day {
    public String help() {
        return "Day 10: Monitoring Station - https://adventofcode.com/2019/day/10\n"
                +  "usage: ./advent 10";
    }
    private InputStream getInputStream() {
        return this.getClass().getResourceAsStream("/input/10.txt");
    }
    public static PolarAsteroidBeltMap parseMap(InputStream in) {
        Stream<String> input = new BufferedReader(new InputStreamReader(in)).lines();
        return new PolarAsteroidBeltMap(input);
    }
    public static Map.Entry<Asteroid, Integer> bestMonitoringLocation(AsteroidBeltMap map) {
        Asteroid bestLocation = null;
        int best = -1;
        Dimension dim = map.getDimension();
        int[][] sees = new int[dim.height][dim.width];
        for (Asteroid asteroid : map.getAsteroids()) {
            Set<Asteroid> visible = map.view(asteroid);
            int n = visible.size();
            Point2D pos = asteroid.cartesian();
            sees[(int) pos.getY()][(int) pos.getX()] = n;
            if (n > best) {
                bestLocation = asteroid;
                best = n;
            }
        }
        return new AbstractMap.SimpleImmutableEntry<Asteroid, Integer>(bestLocation, best);
    }
    public Asteroid destructionOrder(PolarAsteroidBeltMap map, Asteroid station, int n) {
        int k = 0;
        while (k < n) {
            List<Asteroid> order = map.viewOrder(station);
            if (order.size()+k < n) {
                map.destroy(order);
                k += order.size();
            } else {
                return order.get(n-k-1);
            }
        }
        return null;
    }
    public void run() {
        PolarAsteroidBeltMap map = parseMap(getInputStream());
        Map.Entry<Asteroid, Integer> best = bestMonitoringLocation(map);
        System.out.println("Best monitoring location is "+best.getKey()+" which sees "+best.getValue());
        Asteroid blown = destructionOrder(map, best.getKey(), 200);
        System.out.println("The 200th asteroid to be destroyed: "+blown+ " code: "+(100*blown.getX()+blown.getY()));
    }
}

class Asteroid {
    private Point position;
    public Asteroid(int x, int y) {
        this.position = new Point(x, y);
    }
    public int getX() {
        return position.x;
    }
    public int getY() {
        return position.y;
    }
    public Point2D cartesian() {
        return new Point(position);
    }
    public double distance(Asteroid origin) {
        return polar(origin).getX();
    }
    public double angle(Asteroid origin) {
        return polar(origin).getY();
    }
    public Point2D polar(Asteroid origin) {
        return polar(origin.position);
    }
    public Point2D polar(Point2D origin) {
        double x = position.getX() - origin.getX();
        double y = position.getY() - origin.getY();
        double radius = Math.sqrt( x * x + y * y );
        double angleInRadians = Math.acos( x / radius );
        if (y > 0) {
            // Angles over 180 need to be flipped over
            angleInRadians = 2*Math.PI - angleInRadians;
        }
        return new Point.Double(radius, angleInRadians);
    }
    public String toString() {
        return "Ct["+position.x+","+position.y+"]";
    }
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof Asteroid)) {
            return false;
        }
        Asteroid oo = (Asteroid) o;
        return this.position.equals(oo.position);
    }
    public int hashCode() {
        return position.hashCode();
    }
}

interface AsteroidBeltMap {
    Dimension getDimension();
    List<Asteroid> getAsteroids();
    Set<Asteroid> view(Asteroid origin);
    List<Asteroid> viewOrder(Asteroid origin);
    boolean destroy(List<Asteroid> ast);
}

abstract class AbstractAsteroidBeltMap implements AsteroidBeltMap {
    protected int w, h;
    protected Vector<String> raw;
    protected List<Asteroid> asteroids;
    protected AbstractAsteroidBeltMap(Stream<String> lines) {
        this.raw = new Vector<String>();
        this.asteroids = new LinkedList<Asteroid>();
        AtomicInteger index = new AtomicInteger();
        lines.forEachOrdered(line -> {
            raw.add(line);
            int y = index.getAndIncrement();
            for (int x = 0; x < line.length(); x++) {
                if (line.charAt(x) == '#') {
                    asteroids.add(new Asteroid(x, y));
                }
            }
        });
        this.h = index.get();
        this.w = raw.get(0).length();
    }
    public Dimension getDimension() {
        return new Dimension(this.w, this.h);
    }
    public List<Asteroid> getAsteroids() {
        return asteroids;
    }
    public boolean destroy(List<Asteroid> ast) {
        return asteroids.removeAll(ast);
    }
}

class PolarAsteroidBeltMap extends AbstractAsteroidBeltMap {
    private static long RESOLUTION = 1000000;
    public PolarAsteroidBeltMap(Stream<String> lines) {
        super(lines);
    }
    private Map<Long, Optional<Asteroid>> viewFrom(Asteroid origin) {
        return getAsteroids().stream()
                .filter(ast -> !ast.equals(origin))
                .collect(Collectors.groupingBy(
                        ast -> Math.round(ast.angle(origin) * RESOLUTION),
                        Collectors.minBy(
                                Comparator.comparingDouble(ast -> ast.distance(origin))
                        )
                ));
    }
    public Set<Asteroid> view(Asteroid origin) {
        Map<Long, Optional<Asteroid>> map = viewFrom(origin);
        return map.values().stream()
                .map(Optional::get)
                .collect(Collectors.toSet());
    }
    public List<Asteroid> viewOrder(Asteroid origin) {
        Map<Long, Optional<Asteroid>> view = viewFrom(origin);
        double ninety = Math.toRadians(90);
        List<Asteroid> lst = view.values().stream()
                .map(Optional::get)
                .sorted(
                        Comparator.comparingDouble(
                                ast -> {
                                    double angle = ast.angle(origin);
                                    if (angle <= ninety) {
                                        angle += Math.PI * 2;
                                    }
                                    return -angle;
                                }
                        )
                ).collect(Collectors.toList());
        return lst;
    }
}

class CartesianAsteroidBeltMap extends AbstractAsteroidBeltMap {
    public CartesianAsteroidBeltMap(Stream<String> lines) {
        super(lines);
    }
    private static int GCD(int a, int b) {
        if (b==0) return a;
        return GCD(b,a%b);
    }
    public Set<Asteroid> view(Asteroid origin) {
        Set<Asteroid> visible = new HashSet<Asteroid>(asteroids);
        for (Asteroid asteroid : asteroids) {
            visible.removeAll(blocks(origin, asteroid));
        }
        visible.remove(origin);
        return visible;
    }
    public Set<Asteroid> blocks(Asteroid origin, Asteroid obstacle) {
        Set<Asteroid> blocked = new HashSet<Asteroid>();
        if (!origin.equals(obstacle)) {
            int dx = obstacle.getX() - origin.getX();
            int dy = obstacle.getY() - origin.getY();
            int gcd = GCD(Math.abs(dx), Math.abs(dy));
            dx /= gcd;
            dy /= gcd;
            Asteroid p = new Asteroid(obstacle.getX()+dx, obstacle.getY()+dy);
            while (p.getX() >= 0 && p.getY() >= 0 && p.getX() < this.w && p.getY() < this.h) {
                blocked.add(p);
                p = new Asteroid(p.getX() + dx, p.getY() + dy);
            }
        }
        return blocked;
    }
    public List<Asteroid> viewOrder(Asteroid origin) {
        throw new RuntimeException("Not Implemented");
    }
}
