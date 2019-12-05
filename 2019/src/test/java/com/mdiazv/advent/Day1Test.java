package com.mdiazv.advent;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Unit test for https://adventofcode.com/2019/day/1
 */
public class Day1Test 
    extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public Day1Test( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( Day1Test.class );
    }

    /**
	 * For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.
	 * For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2.
	 * For a mass of 1969, the fuel required is 654.
	 * For a mass of 100756, the fuel required is 33583.
     */
    public void testFuel()
    {
		int[][] tests = {
			{12, 2},
			{14, 2},
			{1969, 654},
			{100756, 33583}
		};
		for (int[] test : tests) {
			int mass = test[0];
			int expected = test[1];
			int fuel = Day1.fuel(mass);
			assertEquals("testing fuel for mass " + Integer.toString(mass), expected, fuel);
		}
    }

    /**
	 * A module of mass 14 requires 2 fuel. This fuel requires no further fuel. So the total fuel required is still just 2.
	 * So, the total fuel required for a module of mass 1969 is 654 + 216 + 70 + 21 + 5 = 966.
	 * The fuel required by a module of mass 100756 and its fuel is: 33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2 = 50346.
     */
    public void testFuelForFuel()
    {
		int[][] tests = {
			{14, 2},
			{1969, 966},
			{100756, 50346}
		};
		for (int[] test : tests) {
			int mass = test[0];
			int expected = test[1];
			int fuel = Day1.fuelForFuel(mass);
			assertEquals("testing fuel for mass " + Integer.toString(mass), expected, fuel);
		}
    }
}

