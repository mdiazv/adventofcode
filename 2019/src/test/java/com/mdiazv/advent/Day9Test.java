package com.mdiazv.advent;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Vector;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import static org.junit.Assert.*;

/**
 * Unit test for https://adventofcode.com/2019/day/9
 */
public class Day9Test
        extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public Day9Test( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( Day9Test.class );
    }

    /**
     * Test relative base instruction
     */
    public void testRelativeBase()
    {
        // takes no input and produces a copy of itself as output.
        long[] data = {109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99};
        IntCodeComputer computer = new IntCodeComputer(data);
        Vector<Long> output = computer.executeWith(new long[]{});
        long[] actual = output.stream().mapToLong(i->i).toArray();
        assertArrayEquals("Quine", data, actual);
    }

    /**
     * Test large numbers
     */
    public void testLargeNumbers()
    {
        long[][] data = {
                // should output a 16-digit number.
                {1102,34915192,34915192,7,4,7,99,0},
                // should output the large number in the middle.
                {104,1125899906842624L,99},
                // test relative lvalue
                {109, 1,21101,5,5,0,204,0,99},
        };
        long[] expected = { 1219070632396864L, 1125899906842624L, 10};

        for (int i = 0; i < data.length; i++) {
            IntCodeComputer computer = new IntCodeComputer(data[i]);
            Vector<Long> output = computer.executeWith(new long[]{});
            assertEquals("Program" + i, expected[i], (long) output.get(0));
        }
    }

}
