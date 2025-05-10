package com.github.mdiazv.advent.lib;

import lombok.RequiredArgsConstructor;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Scanner;

@RequiredArgsConstructor
public class AdventWebsite {
    private static final String SESSION_FILE = "session.cookie";
    private final String session;

    public static AdventWebsite load() {
        InputStream input = AdventWebsite.class.getClassLoader().getResourceAsStream(SESSION_FILE);
        String session = new Scanner(input).nextLine();
        return new AdventWebsite(session);
    }

    public InputStream getInput(int day) {
        try {
            URL url = new URL(String.format("https://adventofcode.com/2024/day/%d/input", day));
            URLConnection connection = url.openConnection();
            connection.addRequestProperty("Cookie", String.format("session=%s", session));
            return connection.getInputStream();
        } catch (MalformedURLException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }
}
