package main

import "fmt"

type cid int

type claim struct {
	id   cid
	i, j int
	w, h int
}

type sqinch struct {
	claimed []cid
}

type fabric struct {
	w, h  int
	cloth [][]sqinch
}

func (c claim) String() string {
	return fmt.Sprintf("#%d @ %d,%d: %dx%d\n", c.id, c.i, c.j, c.w, c.h)
}

func NewSqInch() sqinch {
	return sqinch{
		claimed: make([]cid, 0),
	}
}

func (s sqinch) String() string {
	n := len(s.claimed)
	if n == 0 {
		return "."
	}
	if n > 1 {
		return "X"
	}
	return fmt.Sprintf("%d", s.claimed[0])
}

func (s *sqinch) claim(c claim) {
	s.claimed = append(s.claimed, c.id)
}

func NewFabric(w, h int) *fabric {
	f := fabric{
		w:     w,
		h:     h,
		cloth: make([][]sqinch, 0, h),
	}

	for i := 0; i < h; i++ {
		row := make([]sqinch, 0, w)
		for j := 0; j < w; j++ {
			row = append(row, NewSqInch())
		}
		f.cloth = append(f.cloth, row)
	}
	return &f
}

func (f *fabric) String() (s string) {
	for i := 0; i < f.h; i++ {
		for j := 0; j < f.w; j++ {
			s += f.cloth[i][j].String()
		}
		s += "\n"
	}
	return
}

func (f *fabric) apply(c claim) {
	for di := 0; di < c.h; di++ {
		for dj := 0; dj < c.w; dj++ {
			f.cloth[c.i+di][c.j+dj].claim(c)
		}
	}
}

func (f *fabric) countIntersections() (r int) {
	for i := 0; i < f.h; i++ {
		for j := 0; j < f.w; j++ {
			if len(f.cloth[i][j].claimed) > 1 {
				r += 1
			}
		}
	}
	return
}

func (f *fabric) findPerfectClaim(cs []claim) cid {
	for _, c := range cs {
		isPerfect := true
		for di := 0; di < c.h && isPerfect; di++ {
			for dj := 0; dj < c.w && isPerfect; dj++ {
				isPerfect = len(f.cloth[c.i+di][c.j+dj].claimed) == 1
			}
		}
		if isPerfect {
			return c.id
		}
	}
	return -1
}

func main() {
	var cs []claim
	f := NewFabric(1000, 1000)
	for {
		var c claim
		if _, err := fmt.Scanf("#%d @ %d,%d: %dx%d\n", &c.id, &c.j, &c.i, &c.w, &c.h); err != nil {
			break
		}
		cs = append(cs, c)
		f.apply(c)
	}
	fmt.Printf("intersection: %d\n", f.countIntersections())
	fmt.Printf("non overlapped: %d\n", f.findPerfectClaim(cs))
}
