package com.mdiazv.advent;

import java.lang.Class;
import java.util.Set;

/**
 * App is the entrypoint that should dispatch to the run method of the appropriate
 * module for each day. Classes should be named DayX and belong to this package.
 *
 * https://adventofcode.com/2019
 */
public class App 
{
	private static Class getDayClass(String day) {
		Integer.parseInt(day);
		String pkg = App.class.getPackage().getName();
		try {
			Class<?> cls = Class.forName(pkg + ".Day" + day);
			return cls;
		} catch (java.lang.ClassNotFoundException e) {
			System.err.println("Solution for Day "+ day +" not implemented!");
			System.exit(1);
			return null;
		}
	}
	private static Day getDay(String day) {
		Class dayClass = getDayClass(day);
		try {
			return (Day) dayClass.getConstructor().newInstance();
		} catch (java.lang.InstantiationException | java.lang.IllegalAccessException e) {
			System.err.println("Solution for Day "+ day +" not found!");
			System.exit(1);
			return null;
		} catch (java.lang.NoSuchMethodException | java.lang.reflect.InvocationTargetException e) {
			System.err.println("Solution for Day "+ day +" missing constructor!");
			System.exit(1);
			return null;
		}
	}
    public static void main( String[] args )
    {
		Day day = getDay(args[0]);
		System.out.println(day.help());
		day.run();
    }
}
