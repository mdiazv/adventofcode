package main

import (
	"fmt"
)

func sum(fs []int) (s int) {
	for _, f := range fs {
		s += f
	}
	return
}

func firstRepeated(fs []int) (freq int) {
	n := len(fs)
	seen := make(map[int]bool)
	for i := 0; !seen[freq]; i++ {
		seen[freq] = true
		freq += fs[i%n]
	}
	return
}

func main() {
	var f int
	var fs []int
	for {
		if _, err := fmt.Scan(&f); err != nil {
			break
		}
		fs = append(fs, f)
	}
	fmt.Printf("sum: %d\n", sum(fs))
	fmt.Printf("first repeated: %d\n", firstRepeated(fs))
}
