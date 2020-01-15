package com.mdiazv.advent;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Unit test for https://adventofcode.com/2019/day/7
 */
public class Day7Test
        extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public Day7Test( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( Day7Test.class );
    }

    /**
     * Execute programs with io, jumps and comparisons
     */
    public void testPermutations()
    {
        long[][] data = {
                // Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0):
                {3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0},
                // Max thruster signal 54321 (from phase setting sequence 0,1,2,3,4):
                {3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0},
                // Max thruster signal 65210 (from phase setting sequence 1,0,4,3,2):
                {3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0},
        };
        long[] expected = {43210,54321,65210};
        for (int i = 0; i < data.length; i++) {
            long result = Day7.largestOutputSignal(data[i], new IntegerPermutations(new Integer[]{0,1,2,3,4}));
            assertEquals("Program: "+i, expected[i], result);
        }
    }

    /**
     * Execute programs concurrently with io, jumps and comparisons
     */
    public void testThreaded()
    {
        long[][] data = {
                // Max thruster signal 139629729 (from phase setting sequence 9,8,7,6,5):
                {3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                        27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5},
                // Max thruster signal 18216 (from phase setting sequence 9,7,8,5,6):
                {3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                        -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                        53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10},
        };
        Integer[][] permutation = {
                {9,8,7,6,5},
                {9,7,8,5,6},
        };
        long[] expected = {139629729, 18216};

        for (int i = 0; i < data.length; i++) {
            ThreadedIntCodeComputer[] amps = new ThreadedIntCodeComputer[]{
                    new ThreadedIntCodeComputer(data[i]),
                    new ThreadedIntCodeComputer(data[i]),
                    new ThreadedIntCodeComputer(data[i]),
                    new ThreadedIntCodeComputer(data[i]),
                    new ThreadedIntCodeComputer(data[i]),
            };
            IntCodeIO[] io = new BlockingIntCodeIO[]{
                    new BlockingIntCodeIO(),
                    new BlockingIntCodeIO(),
                    new BlockingIntCodeIO(),
                    new BlockingIntCodeIO(),
                    new BlockingIntCodeIO(),
            };
            long result = Day7.runFeedbackLoop(amps, io, permutation[i]);
            assertEquals("Program: "+i, expected[i], result);
        }
    }
}
