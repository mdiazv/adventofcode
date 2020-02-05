package com.mdiazv.advent;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Unit test for https://adventofcode.com/2019/day/2
 */
public class Day2Test 
    extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public Day2Test( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( Day2Test.class );
    }

    /**
	 * A parsed program is an integer array with the same values as the original
	 * comma separated string
     */
    public void testParseProgram()
    {
		String text = "1,9,10,3,2,3,11,0,99,30,40,50";
		long[] expected = {1,9,10,3,2,3,11,0,99,30,40,50};
		InputStream in = new ByteArrayInputStream(text.getBytes());
		long[] result = Day2.parseProgram(in);
		assertEquals("Checking same length", expected.length, result.length);
		for (int i = 0; i < expected.length; i++) {
			assertEquals("Position "+i, expected[i], result[i]);
		}
    }

	/**
	 * Execute the sample program and recover the result at position 0
	 */
	public void testExecuteProgram()
	{
		long[][][] data = {
			{{1,0,0,0,99},{2,0,0,0,99}},
			{{2,3,0,3,99},{2,3,0,6,99}},
			{{2,4,4,5,99,0},{2,4,4,5,99,9801}},
			{{1,1,1,4,99,5,6,0,99},{30,1,1,4,2,5,6,0,99}},
			{{1,9,10,3,2,3,11,0,99,30,40,50},{3500,9,10,70,2,3,11,0,99,30,40,50}}
		};
		for (long[][] test : data) {
			long[] initial = test[0];
			long[] terminal = test[1];
			IntCodeComputer computer = new IntCodeComputer(initial);
			computer.execute();
			long[] result = computer.state();
			for (int i = 0; i < terminal.length; i++) {
				assertEquals("Position "+i, terminal[i], result[i]);
			}
		}
	}

}

