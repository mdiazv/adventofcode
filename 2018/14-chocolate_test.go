package main

import (
	. "main"
	"testing"
)

func TestChocolate(t *testing.T) {
	type Case struct {
		n        int
		expected string
	}
	cases := []Case{
		{9, "5158916779"},
		{5, "0124515891"},
		{18, "9251071085"},
		{2018, "5941429882"},
	}
	for _, c := range cases {

		scores := makeRecipes(c.n)
		if scores != c.expected {
			t.Errorf(
				"After %d recipes, the next ten would be %s (expected %s)",
				c.n, scores, c.expected,
			)
		}
	}
}
