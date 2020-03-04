package com.mdiazv.advent;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Map;
import java.util.Vector;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import static org.junit.Assert.*;

/**
 * Unit test for https://adventofcode.com/2019/day/12
 */
public class Day12Test
        extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public Day12Test( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( Day12Test.class );
    }

    /**
     * Test totalEnergy
     */
    public void testSimulateTotalEnergy()
    {
        // takes no input and produces a copy of itself as output.
        String[] data = new String[]{
                "<x=-1, y=0, z=2>\n" +
                "<x=2, y=-10, z=-7>\n" +
                "<x=4, y=-8, z=8>\n" +
                "<x=3, y=5, z=-1>",

                "<x=-8, y=-10, z=0>\n" +
                "<x=5, y=5, z=10>\n" +
                "<x=2, y=-7, z=3>\n" +
                "<x=9, y=-8, z=-3>",
        };
        int steps[] = {10, 100};
        int expectedEnergy[] = {179, 1940};

        for (int i = 0; i < expectedEnergy.length; i++) {
            Simulator sim = new VectorSimulator(data[i]);
            long energy = sim.totalEnergy(steps[i]);
            assertEquals("Total Energy", expectedEnergy[i], energy);
        }
    }

    /**
     * Test painting
     */
    public void testCycleLength()
    {
        // takes no input and produces a copy of itself as output.
        String[] data = new String[]{
                "<x=-1, y=0, z=2>\n" +
                        "<x=2, y=-10, z=-7>\n" +
                        "<x=4, y=-8, z=8>\n" +
                        "<x=3, y=5, z=-1>",

                "<x=-8, y=-10, z=0>\n" +
                        "<x=5, y=5, z=10>\n" +
                        "<x=2, y=-7, z=3>\n" +
                        "<x=9, y=-8, z=-3>",
        };
        long expectedCycleLength[] = {2772, 4686774924L};

        for (int i = 0; i < expectedCycleLength.length; i++) {
            IndividualSimulator sim = new IndividualSimulator(data[i]);
            long cycle = sim.cycleLength();
            assertEquals("Cycle length", expectedCycleLength[i], cycle);
        }
    }
}
