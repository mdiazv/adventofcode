package com.mdiazv.advent;

import java.awt.*;
import java.awt.geom.Point2D;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.util.*;
import java.util.List;
import java.util.stream.Stream;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import static org.junit.Assert.*;

/**
 * Unit test for https://adventofcode.com/2019/day/10
 */
public class Day10Test
        extends TestCase {
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public Day10Test(String testName) {
        super(testName);
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite() {
        return new TestSuite(Day10Test.class);
    }

    /**
     * Test blocked positions
     */
    public void testBlockedPositions() {
        String data =
                "#.........\n" +
                "...#......\n" +
                "...#..a...\n" +
                ".####....a\n" +
                "..#.c.b...\n" +
                ".....c....\n" +
                "..efd.c.gb\n" +
                ".......c..\n" +
                "....f...c.\n" +
                "...e..d..c";
        Asteroid[] cases = new Asteroid[]{
                new Asteroid(3,1), // A
                new Asteroid(3,2), // B
                new Asteroid(3,3), // C
                new Asteroid(2,3), // D
                new Asteroid(1,3), // E
                new Asteroid(2,4), // F
                new Asteroid(4,3), // G
        };
        Asteroid[][] expected = new Asteroid[][]{
                { new Asteroid(6, 2), new Asteroid(9, 3) },
                { new Asteroid(6, 4), new Asteroid(9, 6) },
                {
                    new Asteroid(4, 4), new Asteroid(5, 5),
                    new Asteroid(6, 6), new Asteroid(7, 7),
                    new Asteroid(8, 8), new Asteroid(9, 9),
                },
                { new Asteroid(4, 6), new Asteroid(6, 9) },
                { new Asteroid(2, 6), new Asteroid(3, 9) },
                { new Asteroid(3, 6), new Asteroid(4, 8) },
                { new Asteroid(8, 6)},
        };
        CartesianAsteroidBeltMap map = new CartesianAsteroidBeltMap(data.lines());
        for (int i = 0; i < cases.length; i++) {
            Set<Asteroid> blocked = map.blocks(new Asteroid(0, 0), cases[i]);
            Asteroid[] exp = expected[i];
            Asteroid[] res = blocked.toArray(new Asteroid[0]);
            Arrays.sort(exp, Comparator.comparingDouble(Asteroid::getX).thenComparingDouble(Asteroid::getY));
            Arrays.sort(res, Comparator.comparingDouble(Asteroid::getX).thenComparingDouble(Asteroid::getY));
            assertArrayEquals("Case "+i, exp, res);
        }
    }
    /**
     * Test angle calculations
     */
    public void testAngle() {
        Asteroid origin = new Asteroid(2, 2);
        Asteroid[] As = new Asteroid[]{
                new Asteroid(4, 2),
                new Asteroid(2, 0),
                new Asteroid(0, 2),
                new Asteroid(2, 4),
        };
        double[] Bs = new double[]{
                0, 90, 180, 270
        };
        for (int i = 0; i < As.length; i++) {
            double angle = As[i].angle(origin);
            double deg = Math.toDegrees(angle);
            assertEquals("origin:"+origin+" ast:"+As[i], Bs[i], deg);
        }
    }
    /**
     * Test viewOrder
     */
    public void testViewOrder() {
        String data =
            "###\n" +
            "###\n" +
            "###\n";
        PolarAsteroidBeltMap map = new PolarAsteroidBeltMap(data.lines());
        Asteroid origin = new Asteroid(1, 1);
        List<Asteroid> order = map.viewOrder(origin);
        List<Asteroid> expected = new ArrayList<Asteroid>(List.of(
                new Asteroid(1, 0), new Asteroid(2, 0), new Asteroid(2,1),
                new Asteroid(2, 2), new Asteroid(1, 2), new Asteroid(0,2),
                new Asteroid(0, 1), new Asteroid(0, 0)
        ));

        assertEquals("ViewOrder", expected, order);
    }
    /**
     * Test bestMonitoringLocation
     */
    public void testBestMonitoringLocationCartesian() throws Exception {
        bestMonitoringLocationScenario(CartesianAsteroidBeltMap.class);
    }
    public void testBestMonitoringLocationPolar() throws Exception {
        bestMonitoringLocationScenario(PolarAsteroidBeltMap.class);
    }
    public void bestMonitoringLocationScenario(Class mapClass) throws Exception {
        String[] data = new String[]{
                ".#..#\n" +
                ".....\n" +
                "#####\n" +
                "....#\n" +
                "...##",
                "......#.#.\n" +
                "#..#.#....\n" +
                "..#######.\n" +
                ".#.#.###..\n" +
                ".#..#.....\n" +
                "..#....#.#\n" +
                "#..#....#.\n" +
                ".##.#..###\n" +
                "##...#..#.\n" +
                ".#....####",
                "#.#...#.#.\n" +
                ".###....#.\n" +
                ".#....#...\n" +
                "##.#.#.#.#\n" +
                "....#.#.#.\n" +
                ".##..###.#\n" +
                "..#...##..\n" +
                "..##....##\n" +
                "......#...\n" +
                ".####.###.",
                ".#..#..###\n" +
                "####.###.#\n" +
                "....###.#.\n" +
                "..###.##.#\n" +
                "##.##.#.#.\n" +
                "....###..#\n" +
                "..#.#..#.#\n" +
                "#..#.#.###\n" +
                ".##...##.#\n" +
                ".....#.#..",
                ".#..##.###...#######\n" +
                "##.############..##.\n" +
                ".#.######.########.#\n" +
                ".###.#######.####.#.\n" +
                "#####.##.#.##.###.##\n" +
                "..#####..#.#########\n" +
                "####################\n" +
                "#.####....###.#.#.##\n" +
                "##.#################\n" +
                "#####.##.###..####..\n" +
                "..######..##.#######\n" +
                "####.##.####...##..#\n" +
                ".#####..#.######.###\n" +
                "##...#.##########...\n" +
                "#.##########.#######\n" +
                ".####.#.###.###.#.##\n" +
                "....##.##.###..#####\n" +
                ".#.#.###########.###\n" +
                "#.#.#.#####.####.###\n" +
                "###.##.####.##.#..##",
        };
        int[] expected = {8, 33, 35, 41, 210};
        for (int i = 0; i < data.length; i++) {
            Constructor<?> cons = mapClass.getConstructor(Stream.class);
            AsteroidBeltMap map = (AsteroidBeltMap) cons.newInstance(data[i].lines());
            Map.Entry<Asteroid, Integer> best = Day10.bestMonitoringLocation(map);
            assertEquals(expected[i], best.getValue().intValue());
        }
    }
}
