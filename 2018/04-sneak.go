package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"time"
)

type EventType int

const (
	GuardChange EventType = iota
	GuardSleep
	GuardAwake
)

func toEventType(s string) EventType {
	switch s {
	case " falls asleep\n":
		return GuardSleep
	case " wakes up\n":
		return GuardAwake
	}
	return GuardChange
}

func (t EventType) String() string {
	switch t {
	case GuardSleep:
		return "Sleep"
	case GuardAwake:
		return "Awake"
	}
	return "Change"
}

type Event struct {
	at    time.Time
	kind  EventType
	guard int
}

func newEvent(t time.Time, s string) Event {
	var guard int
	kind := toEventType(s)
	fmt.Sscanf(s, " Guard #%d begins shift", &guard)
	return Event{
		at:    t,
		kind:  kind,
		guard: guard,
	}
}

func (e *Event) String() string {
	return fmt.Sprintf("%s (%d) %s", e.at, e.guard, e.kind)
}

type Guard struct {
	id       int
	total    int
	sleeping []int
}

func NewGuard(id int) *Guard {
	return &Guard{
		id:       id,
		sleeping: make([]int, 60),
	}
}

func (g *Guard) snooze(s, e time.Time) {
	for m := s.Minute(); m < e.Minute(); m++ {
		g.sleeping[m] += 1
		g.total += 1
	}
}

func (g *Guard) mostSleptMinute() int {
	var bm, bv int
	for m, v := range g.sleeping {
		if v > bv {
			bm = m
			bv = v
		}
	}
	return bm
}

func mostSleepy(Gs map[int]*Guard) *Guard {
	var bi, bt int
	for i, g := range Gs {
		if g.total > bt {
			bi = i
			bt = g.total
		}
	}
	return Gs[bi]
}

func mostFrequentlySleptAt(Gs map[int]*Guard) (*Guard, int) {
	var bi, bm, record int
	for i, g := range Gs {
		m := g.mostSleptMinute()
		if g.sleeping[m] > record {
			record = g.sleeping[m]
			bm = m
			bi = i
		}
	}
	return Gs[bi], bm
}

func preprocessGuards(Es []Event) map[int]*Guard {
	var gid int
	var start time.Time
	Gs := make(map[int]*Guard)
	for _, e := range Es {
		switch e.kind {
		case GuardChange:
			gid = e.guard
			if _, present := Gs[gid]; !present {
				Gs[gid] = NewGuard(gid)
			}
		case GuardSleep:
			start = e.at
		case GuardAwake:
			Gs[gid].snooze(start, e.at)
		}
	}
	return Gs
}

func strategy1(Es []Event) {
	Gs := preprocessGuards(Es)
	g := mostSleepy(Gs)
	m := g.mostSleptMinute()
	fmt.Printf("S1: guard %d X minute %d = %d\n", g.id, m, g.id*m)
}

func strategy2(Es []Event) {
	Gs := preprocessGuards(Es)
	g, m := mostFrequentlySleptAt(Gs)
	fmt.Printf("S2: guard %d X minute %d = %d\n", g.id, m, g.id*m)
}

func main() {
	var Es []Event
	in := bufio.NewReader(os.Stdin)
	for {
		var err error
		var date, msg string
		var m time.Month
		var y, d, H, M int

		if date, err = in.ReadString(']'); err != nil {
			break
		}
		if msg, err = in.ReadString('\n'); err != nil {
			break
		}

		fmt.Sscanf(date, "[%d-%d-%d %d:%d]", &y, &m, &d, &H, &M)
		t := time.Date(y, m, d, H, M, 0, 0, time.Local)
		e := newEvent(t, msg)
		Es = append(Es, e)
	}
	sort.Slice(Es, func(i, j int) bool {
		return Es[i].at.Before(Es[j].at)
	})
	strategy1(Es)
	strategy2(Es)
}
