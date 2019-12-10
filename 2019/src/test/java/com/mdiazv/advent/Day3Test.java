package com.mdiazv.advent;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Unit test for https://adventofcode.com/2019/day/1
 */
public class Day3Test 
    extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public Day3Test( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( Day3Test.class );
    }

    /**
	 * Find the closest intersection to the origin
     */
    public void testClosestIntersection()
    {
		String[][] data = new String[][]{
			{"R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", "159"},
			{"R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", "135"}
		};
		for (String[] test : data) {
			Wire w1 = new Wire(test[0]);
			Wire w2 = new Wire(test[1]);
			int expected = Integer.valueOf(test[2]);
			assertEquals("Checking min distance", expected, Day3.closestIntersection(w1, w2));
		}
    }

    /**
	 * Find the intersection with the minimum combined cost
     */
    public void testCheapestIntersection()
    {
		String[][] data = new String[][]{
			{"R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", "610"},
			{"R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", "410"}
		};
		for (String[] test : data) {
			Wire w1 = new Wire(test[0]);
			Wire w2 = new Wire(test[1]);
			int expected = Integer.valueOf(test[2]);
			assertEquals("Checking cheapest distance", expected, Day3.cheapestIntersection(w1, w2));
		}
    }
}
