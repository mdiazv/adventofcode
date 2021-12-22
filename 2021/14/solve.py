from collections import Counter
import sys

def polymerize(start, tr, steps=1):
    elems = Counter(start)
    pairs = Counter(a+b for a, b in zip(start, start[1:]))
    for _ in range(steps):
        for p, k in pairs.most_common():
            pairs[p] -= k
            pairs[p[0]+tr[p]] += k
            pairs[tr[p]+p[1]] += k
            elems[tr[p]] += k
    return elems.most_common()

start = input()
input()
tr = dict(map(lambda s: s[:-1].split(' -> '), sys.stdin))

for k in [10, 40]:
    freq = polymerize(start, tr, k)
    diff = freq[0][1] - freq[-1][1]
    print (f'Difference of the most and least common element after {k} steps: {diff}')
