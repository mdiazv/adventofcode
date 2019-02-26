package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"strings"
)

func lower(a byte) byte {
	return bytes.ToLower([]byte{a})[0]
}

func doesReact(a, b byte) bool {
	return lower(a) == lower(b) && a != b
}

func eliminate(u byte, polymer string) string {
	var builder strings.Builder
	n := len(polymer)
	for i := 0; i < n; i++ {
		if lower(polymer[i]) != u {
			builder.WriteByte(polymer[i])
		}
	}
	return builder.String()
}

func react(polymer string) string {
	var builder strings.Builder
	n := len(polymer)
	builder.Grow(n)
	i := 0
	for ; i < n-1; i++ {
		if doesReact(polymer[i], polymer[i+1]) {
			i++
		} else {
			builder.WriteByte(polymer[i])
		}
	}
	if i < n {
		builder.WriteByte(polymer[n-1])
	}
	new := builder.String()
	if polymer == new {
		return polymer
	}
	return react(new)
}

func bestReaction(polymer string) string {
	have := make(map[byte]bool)
	n := len(polymer)
	for i := 0; i < n; i++ {
		have[lower(polymer[i])] = true
	}
	var best string
	for u, _ := range have {
		res := react(eliminate(u, polymer))
		if len(res) < n {
			best = res
			n = len(res)
		}
	}
	return best
}

func main() {
	in := bufio.NewReader(os.Stdin)
	polymer, _ := in.ReadString('\n')
	polymer = polymer[:len(polymer)-1]
	fmt.Printf("After reacting: (%d)\n", len(react(polymer)))
	fmt.Printf("Shortest possible: (%d)\n", len(bestReaction(polymer)))
}
