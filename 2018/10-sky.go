package main

import (
	"fmt"
	"math"
	"strings"
)

type Vector struct {
	X, Y float64
}

func (v Vector) Add(o Vector) Vector {
	return Vector{X: v.X + o.X, Y: v.Y + o.Y}
}
func (v Vector) Sub(o Vector) Vector {
	return Vector{X: v.X - o.X, Y: v.Y - o.Y}
}

func (v Vector) Div(n float64) Vector {
	return Vector{X: v.X / n, Y: v.Y / n}
}

func (v Vector) Dist(o Vector) float64 {
	return math.Sqrt(math.Pow(float64(v.X-o.X), 2) + math.Pow(float64(v.Y-o.Y), 2))
}

func (v Vector) String() string {
	return fmt.Sprintf("< %f, %f >", v.X, v.Y)
}

type Entity struct {
	Pos, Vel Vector
}

func (e *Entity) Step() {
	e.Pos = e.Pos.Add(e.Vel)
}

func (e *Entity) Translate(center, viewport Vector) Vector {
	var relativeToCenter = e.Pos.Sub(center)
	var relativeToViewport = relativeToCenter.Add(viewport.Div(2))
	return relativeToViewport
}

type World struct {
	Entities []Entity
	Center   Entity
}

func parseWorld() (w World) {
	var center Entity
	for {
		var e Entity
		_, err := fmt.Scanf("position=<%f,%f> velocity=<%f,%f>\n", &e.Pos.X, &e.Pos.Y, &e.Vel.X, &e.Vel.Y)
		if err != nil {
			fmt.Printf("err: %s\n", err)
			break
		}
		w.Entities = append(w.Entities, e)
		center.Pos = center.Pos.Add(e.Pos)
		center.Vel = center.Vel.Add(e.Vel)
	}
	w.Center.Pos = center.Pos.Div(float64(len(w.Entities)))
	w.Center.Vel = center.Vel.Div(float64(len(w.Entities)))
	return
}

func (w *World) Step() {
	var n = len(w.Entities)
	for i := 0; i < n; i++ {
		w.Entities[i].Step()
	}
	w.Center.Step()
}

func (w *World) Distance() (d float64) {
	for _, e := range w.Entities {
		d += e.Pos.Dist(w.Center.Pos)
	}
	return
}

func (w *World) Render(width, height int) string {
	var s = make([][]rune, height)
	for i := 0; i < height; i++ {
		s[i] = make([]rune, width)
		for j := 0; j < width; j++ {
			s[i][j] = '.'
		}
	}
	var empty = true
	var viewport = Vector{X: float64(width), Y: float64(height)}
	for _, e := range w.Entities {
		var p = e.Translate(w.Center.Pos, viewport)
		var i, j = int(p.Y), int(p.X)
		if i >= 0 && j >= 0 && j < width && i < height {
			s[i][j] = '#'
			empty = false
		}
	}
	if empty {
		return ""
	}
	var sb strings.Builder
	for i := 0; i < height; i++ {
		sb.WriteString(string(s[i]))
		sb.WriteRune('\n')
	}
	return sb.String()
}

func (w World) String() string {
	return w.Render(170, 40)
}

func main() {
	var w = parseWorld()
	fmt.Printf("There are %d entities\n", len(w.Entities))
	var dist = w.Distance()
	for i := 0; ; i++ {
		fmt.Printf("Step %d - World center is: %s, distance %f\n", i, w.Center, w.Distance())
		fmt.Printf("World:\n%s\n", w)
		w.Step()
		var newdist = w.Distance()
		if newdist > dist {
			break
		}
		dist = newdist
	}
}
