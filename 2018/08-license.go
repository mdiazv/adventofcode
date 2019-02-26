package main

import (
	"fmt"
)

type Node struct {
	child, meta int
	children    []Node
	metas       []int
}

func (n *Node) sum() (s int) {
	for _, c := range n.children {
		s += c.sum()
	}
	for _, m := range n.metas {
		s += m
	}
	return
}

func (n *Node) value() (v int) {
	if n.child == 0 {
		for _, m := range n.metas {
			v += m
		}
		return
	}
	for _, m := range n.metas {
		if m > 0 && m <= n.child {
			v += n.children[m-1].value()
		}
	}
	return
}

func (n *Node) parse() {
	fmt.Scanf("%d %d", &n.child, &n.meta)
	for i := 0; i < n.child; i++ {
		var c Node
		c.parse()
		n.children = append(n.children, c)
	}
	for i := 0; i < n.meta; i++ {
		var v int
		fmt.Scanf("%d", &v)
		n.metas = append(n.metas, v)
	}
}

func main() {
	var N Node
	N.parse()
	fmt.Printf("sum: %d\n", N.sum())
	fmt.Printf("value: %d\n", N.value())
}
