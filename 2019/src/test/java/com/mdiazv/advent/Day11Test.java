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
 * Unit test for https://adventofcode.com/2019/day/11
 */
public class Day11Test
        extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public Day11Test( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( Day11Test.class );
    }

    /**
     * Test painting
     */
    public void testPainting()
    {
        // takes no input and produces a copy of itself as output.
        long[] data = {
            109,100,
            3,0,104,1,104,0,
            3,0,104,0,104,0,
            3,0,104,1,104,0,
            3,0,104,1,104,0,
            3,0,104,0,104,1,
            3,0,104,1,104,0,
            3,0,104,1,104,0,
            99
        };
        InfiniteGrid<Integer> hull = Day11.renderHull(data, 0);
        assertEquals("Painted count", 6, hull.size());
        String render =  hull.render(Map.of(0, " ", 1, "#"));
        String expected =
                "  #\n" +
                "  #\n" +
                "## \n";
        assertEquals("Render", expected, render);
    }
}
