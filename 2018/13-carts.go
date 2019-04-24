package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)

type Tile rune

func (t Tile) turn(road Tile) Tile {
	switch road {
	case TILE_FWD_TURN: //        = '/'
		switch t {
		case TILE_CART_LEFT:
			return TILE_CART_DOWN
		case TILE_CART_DOWN:
			return TILE_CART_LEFT
		case TILE_CART_RIGHT:
			return TILE_CART_UP
		case TILE_CART_UP:
			return TILE_CART_RIGHT
		}
	case TILE_BCK_TURN: //        = '\\'
		switch t {
		case TILE_CART_LEFT:
			return TILE_CART_UP
		case TILE_CART_UP:
			return TILE_CART_LEFT
		case TILE_CART_RIGHT:
			return TILE_CART_DOWN
		case TILE_CART_DOWN:
			return TILE_CART_RIGHT
		}
	}
	return t
}

const (
	TILE_HORIZONTL  Tile = '-'
	TILE_VERTICAL        = '|'
	TILE_CROSS           = '+'
	TILE_FWD_TURN        = '/'
	TILE_BCK_TURN        = '\\'
	TILE_CART_LEFT       = '<'
	TILE_CART_UP         = '^'
	TILE_CART_RIGHT      = '>'
	TILE_CART_DOWN       = 'v'
	TILE_CART_CRASH      = 'X'
)

func (t Tile) isCart() bool {
	return t == TILE_CART_LEFT ||
		t == TILE_CART_UP ||
		t == TILE_CART_RIGHT ||
		t == TILE_CART_DOWN ||
		t == TILE_CART_CRASH
}

func (t Tile) isTurn() bool {
	return t == TILE_FWD_TURN || t == TILE_BCK_TURN
}

func (t Tile) isIntersection() bool {
	return t == TILE_CROSS
}

func (t Tile) actualTile() Tile {
	if t == TILE_CART_LEFT || t == TILE_CART_RIGHT {
		return TILE_HORIZONTL
	}
	if t == TILE_CART_UP || t == TILE_CART_DOWN {
		return TILE_VERTICAL
	}
	return t
}

type Direction int

var tileToDir = map[Tile]Direction{
	TILE_CART_LEFT:  0,
	TILE_CART_UP:    1,
	TILE_CART_RIGHT: 2,
	TILE_CART_DOWN:  3,
}

var dirToTile = map[Direction]Tile{
	0: TILE_CART_LEFT,
	1: TILE_CART_UP,
	2: TILE_CART_RIGHT,
	3: TILE_CART_DOWN,
}

type Cart struct {
	tile Tile
	turn Direction
	i, j int
}

func (c Cart) advance() (int, int) {
	switch c.tile {
	case TILE_CART_LEFT:
		return c.i, c.j - 1
	case TILE_CART_UP:
		return c.i - 1, c.j
	case TILE_CART_RIGHT:
		return c.i, c.j + 1
	case TILE_CART_DOWN:
		return c.i + 1, c.j
	}
	panic("Can't advance")
}

func (c Cart) stepon(road Tile) (Tile, Direction) {
	if road.isTurn() {
		return c.tile.turn(road), c.turn
	}
	if road.isIntersection() {
		d := tileToDir[c.tile]
		d = Direction((int(d+c.turn) + 4) % 4)
		t := c.turn + 1
		if t == 2 {
			t = -1
		}
		return dirToTile[d], t
	}
	return c.tile, c.turn
}

func (c Cart) update(ts [][]Tile) Cart {
	c.i, c.j = c.advance()
	c.tile, c.turn = c.stepon(ts[c.i][c.j])
	return c
}

func (c Cart) String() string {
	return fmt.Sprintf("<%d, %d> %c [next turn %d]", c.j, c.i, c.tile, c.turn)
}

type Track struct {
	md [][]Tile
	ts [][]Tile
	cs []Cart
}

func makeCart(i, j int, t Tile) Cart {
	return Cart{tile: t, i: i, j: j, turn: -1}
}

func (t *Track) add(line string) {
	var ts []Tile
	var md []Tile
	for j, r := range line {
		var tt = Tile(r)
		ts = append(ts, tt.actualTile())
		md = append(md, tt)
		if tt.isCart() {
			t.cs = append(t.cs, makeCart(len(t.ts), j, tt))
		}
	}
	t.ts = append(t.ts, ts)
	t.md = append(t.md, md)
}

func (t *Track) sortedCarts() []Cart {
	sort.Slice(t.cs, func(i, j int) bool {
		var c1, c2 = t.cs[i], t.cs[j]
		if c1.i == c2.i {
			return c1.j < c2.j
		}
		return c1.i < c2.i
	})
	return t.cs
}

type Location struct {
	i, j int
}

func (t *Track) removeCrashes(cs []Cart, crashes map[Location]bool) (ncs []Cart) {
	for _, c := range cs {
		if !crashes[Location{i: c.i, j: c.j}] {
			ncs = append(ncs, c)
		}
	}
	for loc, _ := range crashes {
		t.md[loc.i][loc.j] = t.ts[loc.i][loc.j]
	}
	t.cs = ncs
	return
}

func (t *Track) update() {
	var cs []Cart
	var crashes = make(map[Location]bool)
	for _, c := range t.sortedCarts() {
		if crashes[Location{i: c.i, j: c.j}] {
			// Already crashed, nothing to do
			continue
		}
		var nc = c.update(t.ts)
		// Remove the cart from the old tile
		t.md[c.i][c.j] = t.ts[c.i][c.j]
		if t.md[nc.i][nc.j].isCart() {
			fmt.Printf("crash @ <%d, %d>\n", nc.j, nc.i)
			// Remove crashed tile
			nc.tile = TILE_CART_CRASH
			// Register crash for removing the other cart
			crashes[Location{i: nc.i, j: nc.j}] = true
		} else {
			// Place the cart on the new tile
			t.md[nc.i][nc.j] = nc.tile
			cs = append(cs, nc)
		}
	}
	t.removeCrashes(cs, crashes)
}

func (t Track) String() string {
	var sb strings.Builder
	for _, row := range t.md {
		for _, c := range row {
			sb.WriteRune(rune(c))
		}
		sb.WriteRune('\n')
	}
	return sb.String()
}

func main() {
	var t Track
	var input = bufio.NewReader(os.Stdin)
	for {
		var line, err = input.ReadString('\n')
		if err != nil {
			break
		}
		t.add(line[:len(line)-1])
	}

	for len(t.cs) > 1 {
		// fmt.Printf("%s\n", t)
		t.update()
	}
	fmt.Printf("%s\n", t)
	if len(t.cs) > 0 {
		fmt.Printf("Last cart: %s\n", t.cs[0])
	} else {
		fmt.Printf("No carts left\n")
	}
}
