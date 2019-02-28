package main

import "fmt"

type Coord struct {
	X, Y int
}

func (c Coord) String() string {
	return fmt.Sprintf("<%d, %d>", c.X, c.Y)
}

type Grid struct {
	Serial int
	w, h   int
	cells  [][]int
}

func (g *Grid) power(x, y int) int {
	var rackID = x + 10
	var initial = (rackID*y + g.Serial) * rackID
	var hundreds = (initial % 1000) / 100
	return hundreds - 5
}

func (g *Grid) populate(w, h int) {
	g.w, g.h = w, h
	g.cells = make([][]int, h)
	for i := 0; i < h; i++ {
		g.cells[i] = make([]int, w)
		for j := 0; j < w; j++ {
			g.cells[i][j] = g.power(j+1, i+1)
		}
	}
}

func (g *Grid) regionValue(i, j, w, h int) (v int) {
	for ii := 0; ii < h; ii++ {
		for jj := 0; jj < w; jj++ {
			v += g.cells[i+ii][j+jj]
		}
	}
	return
}

func (g *Grid) maxRegion(side int) (int, Coord) {
	var best = -10000
	var where Coord
	for i := 0; i < g.h-side; i++ {
		for j := 0; j < g.w-side; j++ {
			var score = g.regionValue(i, j, side, side)
			if score > best {
				best = score
				where = Coord{X: j + 1, Y: i + 1}
			}
		}
	}
	return best, where
}

func highestPowerRegion(serial int, maxsize int) {
	var g = Grid{Serial: serial}
	g.populate(300, 300)
	var size, best int
	var where Coord
	for s := 1; s <= maxsize; s++ {
		var b, w = g.maxRegion(s)
		if b > best {
			best, where, size = b, w, s
		}
	}
	fmt.Printf("highestPowerRegion (%d) = %d at %s size %d\n", serial, best, where, size)
}

func main() {
	highestPowerRegion(18, 3)
	highestPowerRegion(42, 3)
	highestPowerRegion(7989, 3)
	highestPowerRegion(7989, 20)
}
