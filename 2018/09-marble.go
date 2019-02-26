package main

import (
	"fmt"
	"strings"
)

type CircleList struct {
	nodes *Node
}

func (l *CircleList) String() string {
	if l.nodes == nil {
		return "[]"
	}
	var n = l.nodes
	var sb strings.Builder
	sb.WriteString("[")
	for {
		sb.WriteString(fmt.Sprintf(" %d", n.Value))
		n = n.Next()
		if n == l.nodes {
			break
		}
	}
	sb.WriteString(" ]")
	return sb.String()
}

func (l *CircleList) InsertAfter(value interface{}, ref *Node) (new *Node) {
	new = makeNode(value)
	if l.nodes == nil {
		l.nodes = new
	} else if ref != nil {
		new.prev = ref
		new.next = ref.next
		new.prev.next = new
		new.next.prev = new
	}
	return
}

func (l *CircleList) Remove(ref *Node) interface{} {
	if ref == l.nodes {
		l.nodes = ref.next
	}
	var v = ref.Value
	ref.prev.next = ref.next
	ref.next.prev = ref.prev
	ref.next = nil
	ref.prev = nil
	return v
}

type Node struct {
	next  *Node
	prev  *Node
	Value interface{}
}

func makeNode(v interface{}) (n *Node) {
	n = &Node{}
	n.next = n
	n.prev = n
	n.Value = v
	return n
}

func (n *Node) Next() *Node {
	return n.next
}

func (n *Node) Prev() *Node {
	return n.prev
}

type World struct {
	players    int
	marbles    int
	nextPlayer int
	nextMarble int
	current    *Node
	circle     *CircleList
	score      []int
}

func makeWorld(players, marbles int) World {
	var list = &CircleList{}
	first := list.InsertAfter(0, nil)
	return World{
		players:    players,
		marbles:    marbles,
		nextPlayer: 0,
		nextMarble: 1,
		current:    first,
		circle:     list,
		score:      make([]int, players),
	}
}

func (w *World) NextTurn() {
	//fmt.Printf("%s\n", w.circle)
	if w.nextMarble%23 != 0 {
		//fmt.Printf("Inserting marble #%d\n", w.nextMarble)
		next := w.current.Next()
		w.current = w.circle.InsertAfter(w.nextMarble, next)
	} else {
		//fmt.Printf("Taking marble #%d\n", w.nextMarble)
		w.score[w.nextPlayer] += w.nextMarble
		for i := 0; i < 6; i++ {
			w.current = w.current.Prev()
		}
		remove := w.current.Prev()
		add := w.circle.Remove(remove)
		//fmt.Printf("Also taking marble #%d\n", add)
		w.score[w.nextPlayer] += add.(int)
	}
	w.nextPlayer = (w.nextPlayer + 1) % w.players
	w.nextMarble++
}

func (w *World) Play() []int {
	for w.nextMarble <= w.marbles {
		w.NextTurn()
	}
	return w.score
}

func play(players, points int) (best int) {
	var W = makeWorld(players, points)
	var score = W.Play()
	for _, s := range score {
		if s > best {
			best = s
		}
	}
	return
}

func main() {
	// 9 players; last marble is worth 25 points: high score is 32
	for {
		var expected = -1
		var players, points int
		if _, err := fmt.Scanf("%d players; last marble is worth %d points", &players, &points); err != nil {
			break
		}
		fmt.Scanf(": high score is %d", &expected)
		var score = play(players, points)
		if expected < 0 {
			fmt.Printf("high score is %d\n", score)
			score = play(players, points*100)
			fmt.Printf("100x high score is %d\n", score)
		} else {
			fmt.Printf("high score should be %d, actual: %d\n", expected, score)
		}
	}
}
