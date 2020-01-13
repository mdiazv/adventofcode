package com.mdiazv.advent;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * --- Day 8: Space Image Format ---
 * Receive a password from a Space Encoded Image
 *
 * https://adventofcode.com/2019/day/8
 */
public class Day8 implements Day {
    public String help() {
        return "Day 8: Space Image Format - https://adventofcode.com/2019/day/8\n"
                +  "usage: ./advent 8";
    }
    private InputStream getInputStream() {
        return this.getClass().getResourceAsStream("/input/8.txt");
    }
    public static SpaceImage parseImage(InputStream in, int w, int h) {
        try {
            String text = new BufferedReader(new InputStreamReader(in)).readLine();
            return new SpaceImage(text, w, h);
        } catch (java.io.IOException e) {
            System.err.println("Could not load program");
            System.exit(1);
            return null;
        }
    }
    public static int spaceImageChecksum(SpaceImage image) {
        SpaceImageLayer layer = Stream.of(image.layers)
                .sorted( (a, b) -> a.freq[0] - b.freq[0] )
                .findFirst()
                .orElse(null);
        return layer.freq[1] * layer.freq[2];
    }
    public static String renderImage(SpaceImage image) {
        SpaceImageLayer layer = image.render();
        return layer.toString();
    }
    public void run() {
        SpaceImage image = parseImage(getInputStream(), 25, 6);
        int checksum = spaceImageChecksum(image);
        System.out.println("Frequency checksum: "+checksum);
        String render = renderImage(image);
        System.out.println(render);
    }
}

class Pixel {
    public static final int BLACK = 0;
    public static final int WHITE = 1;
    public static final int TRANSPARENT = 2;
    public static String render(int pixel) {
        switch (pixel) {
            case TRANSPARENT:
            case BLACK:
                return " ";
            case WHITE: return "X";
        }
        return "?";
    }
}

class SpaceImage {
    public int w, h;
    public SpaceImageLayer[] layers;
    public SpaceImageLayer rendered;

    public SpaceImage(String text, int w, int h) {
        this.w = w;
        this.h = h;
        int lsize = w * h;
        int n = text.length();
        int layers = n / lsize;
        this.layers = new SpaceImageLayer[layers];
        for (int i = 0; i < layers; i++) {
            this.layers[i] = new SpaceImageLayer(text.substring(i*lsize, (i+1)*lsize), w, h);
        }
    }
    public SpaceImageLayer render() {
        if (rendered == null) {
            rendered = Stream.of(layers)
                    .reduce(SpaceImageLayer::merge)
                    .orElse(null);
        }
        return rendered;
    }
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < layers.length; i++) {
            sb.append(" ").append(i).append(" -> ").append(layers[i]).append("\n");
        }
        return sb.toString();
    }
}

class SpaceImageLayer {
    public int w, h;
    public int[] freq;
    public int[][] pixels;
    public SpaceImageLayer(int w, int h) {
        this.w = w;
        this.h = h;
        this.freq = new int[10];
        this.pixels = new int[h][w];
    }
    public SpaceImageLayer(String text, int w, int h) {
        this(w, h);
        int n = text.length();
        int lsize = w * h;
        IntStream.range(0, n)
                .map(k -> text.charAt(k)-'0')
                .forEach(v -> this.freq[v]++);
        for (int i = 0; i < n; i++) {
            pixels[i / w][i % w] = text.charAt(i)-'0';
        }
    }
    public static SpaceImageLayer merge(SpaceImageLayer top, SpaceImageLayer bottom) {
        SpaceImageLayer result = new SpaceImageLayer(top.w, top.h);
        for (int i = 0; i < top.h; i++) {
            for (int j = 0; j < top.w; j++) {
                int pixel = top.pixels[i][j] == Pixel.TRANSPARENT
                        ? bottom.pixels[i][j]
                        : top.pixels[i][j];
                result.freq[pixel]++;
                result.pixels[i][j] = pixel;
            }
        }
        return result;
    }
    public String toString() {
        StringBuilder sb = new StringBuilder("[");
        for (int i = 0; i < freq.length; i++) {
            sb.append(" ").append(i).append(":").append(freq[i]);
        }
        sb.append(" ]\n---\n");
        for (int i = 0; i < h; i++) {
            for (int j = 0; j < w; j++) {
                sb.append(Pixel.render(pixels[i][j]));
            }
            sb.append("\n");
        }
        sb.append("---");
        return sb.toString();
    }
}