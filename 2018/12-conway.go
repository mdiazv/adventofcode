package main

import (
	"fmt"
	"strings"
)

type World struct {
	gen         int
	left, right int
	pots        map[int]rune
	patterns    map[string]rune
}

func makeWorld(initial string, patterns map[string]rune) *World {
	var pots = make(map[int]rune)
	for i, r := range initial {
		pots[i] = r
	}
	return &World{
		gen:      0,
		left:     0,
		right:    len(initial),
		pots:     pots,
		patterns: patterns,
	}
}

func (w *World) sequenceAt(i int) string {
	var s []rune
	for di := 0; di < 5; di++ {
		var r, present = w.pots[i+di]
		if !present {
			r = '.'
		}
		s = append(s, r)
	}
	return string(s)
}

func (w *World) NextGen() *World {
	const offset = 5
	var pots = make(map[int]rune)
	var left, right = 10000, -10000
	for i := w.left - offset; i <= w.right+offset; i++ {
		var p = w.sequenceAt(i)
		var v, present = w.patterns[p]
		if !present {
			v = '.'
		}
		k := i + offset/2
		pots[k] = v
		if v == '#' && k < left {
			left = k
		}
		if v == '#' && k > right {
			right = k
		}
	}
	return &World{
		gen:      w.gen + 1,
		left:     left,
		right:    right,
		patterns: w.patterns,
		pots:     pots,
	}
}

func (w *World) sumPlantedPots() (s int) {
	for i, r := range w.pots {
		if r == '#' {
			s += i
		}
	}
	return
}

func predict(currentGen, targetGen, value, delta int) int {
	return value + (targetGen-currentGen)*delta
}

func (w *World) String() string {
	var sb strings.Builder
	sb.WriteRune('.')
	for i := w.left; i <= w.right; i++ {
		sb.WriteRune(w.pots[i])
	}
	sb.WriteRune('.')
	sb.WriteString(fmt.Sprintf(" (left: %d, right: %d)", w.left, w.right))
	return sb.String()
}

func main() {
	var initial string
	var patterns = make(map[string]rune)
	fmt.Scanf("initial state: %s\n\n", &initial)
	for {
		var p string
		var c rune
		if _, err := fmt.Scanf("%s => %c\n", &p, &c); err != nil {
			break
		}
		patterns[p] = c
	}
	var prev, sum, delta int
	var w = makeWorld(initial, patterns)
	fmt.Printf("First 20 generations:\n")
	for w.gen <= 20 {
		sum = w.sumPlantedPots()
		delta = sum - prev
		prev = sum
		fmt.Printf("%.2d: %s (sum of planted pots %d, delta %d)\n", w.gen, w, sum, delta)
		w = w.NextGen()
	}
	for w.gen < 150 {
		sum = w.sumPlantedPots()
		delta = sum - prev
		prev = sum
		w = w.NextGen()
	}
	var targetGen = 50000000000
	fmt.Printf("At gen %d sum should be %d\n", targetGen, predict(w.gen-1, targetGen, sum, delta))
}
