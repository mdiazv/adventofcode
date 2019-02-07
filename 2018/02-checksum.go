package main

import "fmt"

func hasRepeatedOcurrences(s string, n int) bool {
	seen := make(map[rune]int)
	for _, c := range s {
		seen[c] += 1
	}
	for _, times := range seen {
		if times == n {
			return true
		}
	}
	return false
}

func countRepeatedOcurrences(ids []string, n int) (s int) {
	for _, id := range ids {
		if hasRepeatedOcurrences(id, n) {
			s += 1
		}
	}
	return
}

func checksum(ids []string) int {
	twos := countRepeatedOcurrences(ids, 2)
	threes := countRepeatedOcurrences(ids, 3)
	return twos * threes
}

func differByOne(a, b string) (bool, int) {
	n := len(a)
	diffs := 0
	index := -1
	for i := 0; i < n; i++ {
		if a[i] != b[i] {
			diffs += 1
			index = i
		}
	}
	if diffs == 1 {
		return true, index
	}
	return false, -1
}

func findMatchingIDs(ids []string) (string, int) {
	n := len(ids)
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			if match, index := differByOne(ids[i], ids[j]); match {
				return ids[i], index
			}
		}
	}
	return "", -1
}

func commonLetters(ids []string) string {
	id, index := findMatchingIDs(ids)
	return id[0:index] + id[index+1:len(id)]
}

func main() {
	var id string
	var ids []string
	for _, err := fmt.Scan(&id); err == nil; _, err = fmt.Scan(&id) {
		ids = append(ids, id)
	}
	fmt.Printf("checksum: %d\n", checksum(ids))
	fmt.Printf("common letters: %s\n", commonLetters(ids))
}
