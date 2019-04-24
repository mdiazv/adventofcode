package main

import (
	"fmt"
)

type Elf struct {
	pos int
}

type ChocolateMaker struct {
	a, b    Elf
	recipes []int
}

func makeChocolateMaker() ChocolateMaker {
	return ChocolateMaker{
		a:       Elf{0},
		b:       Elf{1},
		recipes: []int{3, 7},
	}
}

func (m *ChocolateMaker) makeRecipe() (vs []int) {
	a := m.recipes[m.a.pos]
	b := m.recipes[m.b.pos]
	d := (a + b) / 10
	r := (a + b) % 10
	if d > 0 {
		m.recipes = append(m.recipes, d)
		vs = append(vs, d)
	}
	m.recipes = append(m.recipes, r)
	vs = append(vs, r)
	n := len(m.recipes)
	m.a.pos = (m.a.pos + 1 + a) % n
	m.b.pos = (m.b.pos + 1 + b) % n
	return
}

func (m *ChocolateMaker) patternAt(k, len int) (s string) {
	for i := 0; i < len; i++ {
		s += fmt.Sprintf("%d", m.recipes[k+i])
	}
	return
}

func (m *ChocolateMaker) NextTenAfter(n int) (s string) {
	for len(m.recipes) < n+10 {
		m.makeRecipe()
	}
	return m.patternAt(n, 10)
}

func (m *ChocolateMaker) FirstOcurrenceOf(s string) int {
	var n = len(s)
	var k = n
	// Ensure minimum length
	for i := 0; i < n; i++ {
		m.makeRecipe()
	}
	var pat = m.patternAt(0, n)
	for {
		m.makeRecipe()
		var nn = len(m.recipes)
		for k < nn {
			if pat == s {
				return k - n
			}
			var v = m.recipes[k]
			pat = pat[1:] + string(rune(v)+'0')
			k++
		}
	}
	return -1
}

func makeRecipes(n int) string {
	m := makeChocolateMaker()
	return m.NextTenAfter(n)
}

func firstOcurrenceOf(s string) int {
	m := makeChocolateMaker()
	return m.FirstOcurrenceOf(s)
}

func part1() {
	cases := []struct {
		n        int
		expected string
	}{
		{9, "5158916779"},
		{5, "0124515891"},
		{18, "9251071085"},
		{2018, "5941429882"},
		{880751, ""},
	}
	for _, c := range cases {
		scores := makeRecipes(c.n)
		fmt.Printf("After %d recipes, the next ten would be %s", c.n, scores)
		if c.expected != "" {
			fmt.Printf(" (expected %s) %t", c.expected, scores == c.expected)
		}
		fmt.Println()
	}
}

func part2() {
	cases := []struct {
		s        string
		expected int
	}{
		{"51589", 9},
		{"01245", 5},
		{"92510", 18},
		{"59414", 2018},
		{"880751", -1},
	}
	for _, c := range cases {
		n := firstOcurrenceOf(c.s)
		fmt.Printf("%s first appears after %d recipes", c.s, n)
		if c.expected >= 0 {
			fmt.Printf(" (expected %d) %t", c.expected, n == c.expected)
		}
		fmt.Println()
	}
}

func main() {
	part1()
	part2()
}
