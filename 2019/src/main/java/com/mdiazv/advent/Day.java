package com.mdiazv.advent;

/**
 * Day represents a solution to the problem for a given Day on
 * https://adventofcode.com/2019
 */
public interface Day {
	/**
	 * help should return an usage message about this day
	 */
	public String help();
	/**
	 * run is the main entrypoint for a given day
	 */
	public void run();
}

