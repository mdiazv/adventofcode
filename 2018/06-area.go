package main

import (
	"fmt"
	"math"
)

type ID int

type coordinate struct {
	id       ID
	x, y     int
	infinite bool
	count    int
}

type location struct {
	closest       map[ID]bool
	distance      int
	totalDistance int
}

func count(Cs []coordinate, grid [][]location, i, j int) {
	var n = len(grid[i][j].closest)
	if n > 1 {
		// close to none
		return
	}
	var id ID
	for k, _ := range grid[i][j].closest {
		id = k
	}
	if i == 0 || j == 0 || i == len(grid)-1 || j == len(grid[0])-1 {
		Cs[id].infinite = true
	}
	if !Cs[id].infinite {
		Cs[id].count += 1
	}
}

func largestAreaSize(Cs []coordinate, grid [][]location, mx, my int) int {
	for i := 0; i <= my; i++ {
		for j := 0; j <= mx; j++ {
			count(Cs, grid, i, j)
		}
	}
	var largest int
	for _, c := range Cs {
		if !c.infinite && c.count > largest {
			largest = c.count
		}
	}
	return largest
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func dist(i, j, ii, jj int) int {
	return abs(ii-i) + abs(jj-j)
}

func fillDistances(Cs []coordinate, grid [][]location, mx, my int) {
	for i := 0; i <= my; i++ {
		for j := 0; j <= mx; j++ {
			for _, c := range Cs {
				var d = dist(i, j, c.y, c.x)
				if d < grid[i][j].distance {
					grid[i][j].closest = map[ID]bool{}
					grid[i][j].distance = d
				}
				if d == grid[i][j].distance {
					grid[i][j].closest[c.id] = true
				}
				grid[i][j].totalDistance += d
			}
		}
	}
}

func areaWithDistancelessThan(dist int, grid [][]location, mx, my int) (s int) {
	for i := 0; i <= my; i++ {
		for j := 0; j <= mx; j++ {
			if grid[i][j].totalDistance < dist {
				s += 1
			}
		}
	}
	return
}

func main() {
	var mx, my int
	var Cs []coordinate
	for id := ID(0); ; id++ {
		var c coordinate
		if _, err := fmt.Scanf("%d, %d\n", &c.x, &c.y); err != nil {
			break
		}
		c.id = id
		if c.x > mx {
			mx = c.x
		}
		if c.y > my {
			my = c.y
		}
		Cs = append(Cs, c)
	}
	var grid = make([][]location, my+1)
	for i := 0; i <= my; i++ {
		grid[i] = make([]location, mx+1)
		for j := 0; j <= mx; j++ {
			grid[i][j].distance = math.MaxInt32
		}
	}
	fillDistances(Cs, grid, mx, my)
	fmt.Printf("Largest non-infinite area: %d\n", largestAreaSize(Cs, grid, mx, my))
	fmt.Printf("Region with total distance < 32: %d\n", areaWithDistancelessThan(32, grid, mx, my))
	fmt.Printf("Region with total distance < 10000: %d\n", areaWithDistancelessThan(10000, grid, mx, my))
}
