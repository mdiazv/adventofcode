package main

import (
	"bufio"
	"fmt"
	"os"
)

type taskID rune
type seconds int

const NOTASK = taskID('|')

type durationCalculator interface {
	duration(id taskID) seconds
}

type constantDuration struct {
	constant seconds
}

func (d constantDuration) duration(id taskID) seconds {
	return d.constant
}

type linearDuration struct {
	penalty seconds
}

func (d linearDuration) duration(id taskID) seconds {
	return d.penalty + seconds(id-'A') + 1
}

type task struct {
	id        taskID
	startedAt seconds
	duration  seconds
	deps      map[taskID]bool
}

func makeTask(id taskID) *task {
	return &task{
		id:        id,
		startedAt: -1,
		deps:      make(map[taskID]bool),
	}
}

func (task *task) isAvailable(t seconds) bool {
	return len(task.deps) == 0 && task.startedAt < 0
}

func (task *task) addDep(d taskID) {
	task.deps[d] = true
}

func (task *task) start(t seconds, c durationCalculator) {
	task.startedAt = t
	task.duration = c.duration(task.id)
}

func (task *task) completed(t seconds) bool {
	return task.startedAt >= 0 && t >= task.startedAt+task.duration
}

type worker struct {
	workingOn *task
}

func (w *worker) isAvailable(t seconds) bool {
	return w.workingOn == nil || w.workingOn.completed(t)
}

type workers []worker

func (ws workers) getAvailable(t seconds) int {
	for i, w := range ws {
		if w.isAvailable(t) {
			//fmt.Printf("Worker %d: %+v is available!\n", i, w)
			return i
		}
	}
	return -1
}

type instructions struct {
	tasks map[taskID]*task
}

func makeInstructions() instructions {
	return instructions{
		tasks: make(map[taskID]*task),
	}
}

func (is *instructions) addTask(s string) {
	var A, B taskID
	fmt.Sscanf(s, "Step %c must be finished before step %c can begin.", &A, &B)
	if _, present := is.tasks[A]; !present {
		is.tasks[A] = makeTask(A)
	}
	if _, present := is.tasks[B]; !present {
		is.tasks[B] = makeTask(B)
	}
	is.tasks[B].addDep(A)
}

func (is *instructions) done(t seconds) bool {
	for _, task := range is.tasks {
		if !task.completed(t) {
			return false
		}
	}
	return true
}

func (is *instructions) nextTask(t seconds) taskID {
	var candidate = NOTASK
	for id, task := range is.tasks {
		//fmt.Printf("Checking task %v\n", task)
		if task.isAvailable(t) && task.id < candidate {
			candidate = id
		}
	}
	return candidate
}

func (is *instructions) start(id taskID, t seconds, c durationCalculator) {
	is.tasks[id].startedAt = t
	is.tasks[id].duration = c.duration(id)
}

func (is *instructions) complete(id taskID) {
	for _, w := range is.tasks {
		delete(w.deps, id)
	}
}

func (is instructions) executionOrder(nw int, d durationCalculator) (order string, t seconds) {
	var workers = make(workers, nw)
	for !is.done(t) {
		//fmt.Printf("t: %d, order: %s, workers: %+v\n", t, order, workers)
		var candidate = is.nextTask(t)
		var w = workers.getAvailable(t)
		for w >= 0 && candidate != NOTASK {
			//	fmt.Printf("starting task: %c\n", candidate)
			is.tasks[candidate].start(t, d)
			workers[w].workingOn = is.tasks[candidate]
			//	fmt.Printf("starting task %+v on worker %d %+v\n", is.tasks[candidate], w, workers[w])
			candidate = is.nextTask(t)
			w = workers.getAvailable(t)
		}
		t += 1
		for i, w := range workers {
			//	fmt.Printf("checking worker %d: %+v\n", i, w)
			if w.workingOn != nil && w.workingOn.completed(t) {
				//		fmt.Printf("completing task: %c\n", w.workingOn.id)
				is.complete(w.workingOn.id)
				order += string(w.workingOn.id)
				workers[i].workingOn = nil
			}
		}
	}
	return order, t
}

func solve(tasks []string, w int, c durationCalculator) {
	var Is = makeInstructions()
	for _, t := range tasks {
		Is.addTask(t)
	}
	order, t := Is.executionOrder(w, c)
	fmt.Printf("Task execution time with %d workers and %+v: %d, order: %s\n", w, c, t, order)
}

func main() {
	var input = bufio.NewReader(os.Stdin)
	var tasks []string
	for {
		line, err := input.ReadString('\n')
		if err != nil {
			break
		}
		tasks = append(tasks, line)
	}

	solve(tasks, 1, &constantDuration{constant: 1})
	solve(tasks, 2, &linearDuration{penalty: 0})
	solve(tasks, 5, &linearDuration{penalty: 60})
}
