package com.mdiazv.advent;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Unit test for https://adventofcode.com/2019/day/5
 */
public class Day5Test
        extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public Day5Test( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( Day5Test.class );
    }

    /**
     * Execute a program that inputs a value and outputs the same value
     */
    public void testIO()
    {
        Integer expected = 42;
        IntCodeComputer computer = new IntCodeComputer(new int[]{3,0,4,0,99}, new int[]{expected});
        computer.execute();
        String result = computer.output();
        assertEquals("Matching output", "[42]", result);
    }

    /**
     * Execute programs with io, jumps and comparisons
     */
    public void testAdvanced()
    {
        int[][] data = {
                // Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
                {3,9,8,9,10,9,4,9,99,-1,8},
                // Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
                {3,9,7,9,10,9,4,9,99,-1,8},
                // Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
                {3,3,1108,-1,8,3,4,3,99},
                // Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
                {3,3,1107,-1,8,3,4,3,99},
                // take an input, then output 0 if the input was zero or 1 if the input was non-zero (position mode)
                {3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9},
                // take an input, then output 0 if the input was zero or 1 if the input was non-zero (immediate mode)
                {3,3,1105,-1,9,1101,0,0,12,4,12,99,1}
        };
        int[] inputs = {0,7,8,9};
        String[][] expected = {
                {"[0]", "[0]", "[1]", "[0]"},
                {"[1]", "[1]", "[0]", "[0]"},
                {"[0]", "[0]", "[1]", "[0]"},
                {"[1]", "[1]", "[0]", "[0]"},
                {"[0]", "[1]", "[1]", "[1]"},
                {"[0]", "[1]", "[1]", "[1]"}
        };
        for (int i = 0; i < data.length; i++) {
            IntCodeComputer computer = new IntCodeComputer(data[i]);
            for (int j = 0; j < inputs.length; j++) {
                String result = computer.executeWith(new int[]{inputs[j]});
                assertEquals("Matching output "+i+","+j, expected[i][j], result);
            }
        }
    }
}
