package com.mdiazv.advent;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Unit test for https://adventofcode.com/2019/day/1
 */
public class Day4Test 
    extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public Day4Test( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( Day4Test.class );
    }

    /**
	 * Test for valid passwords.
	 *
	 * - It is a six-digit number.
	 * - Two adjacent digits are the same (like 22 in 122345).
	 * - Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
     */
    public void testValidPassword()
    {
		String[][] data = new String[][]{
			{"111111", "true"},
			{"223450", "false"},
			{"123789", "false"},
			{"122345", "true"},
			{"111123", "true"},
			{"135679", "false"},
			{"11", "false"}
		};
		for (String[] test : data) {
			int n = Integer.valueOf(test[0]);
			boolean expected = Boolean.valueOf(test[1]);
			assertEquals("Checking valid password "+n, expected, Day4.isValidPassword(n));
		}
    }
    /**
	 * Test for valid more strict passwords.
	 *
	 * - It is a six-digit number.
	 * - Two adjacent digits are the same (like 22 in 122345).
	 * - Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
     */
    public void testStrictPassword()
    {
		String[][] data = new String[][]{
			{"112233", "true"},
			{"123444", "false"},
			{"111122", "true"},
		};
		for (String[] test : data) {
			int n = Integer.valueOf(test[0]);
			boolean expected = Boolean.valueOf(test[1]);
			assertEquals("Checking strict password "+n, expected, Day4.isStrictPassword(n));
		}
    }
}
