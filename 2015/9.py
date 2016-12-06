from collections import defaultdict
from itertools import combinations, imap
from sys import stdin
import re

inf = 0x7fffffff
city = {}
D = {}

def tsp(start, op, inf):
    T = defaultdict(lambda: inf)
    T[(frozenset([start]), start)] = 0
    for m in xrange(2, len(city)+1):
        for S in imap(frozenset, combinations(xrange(1, len(city)+1), m)):
            for j in S - set([start]):
                for k in S - set([j]):
                    T[S, j] = op (T[S, j], T[S - set([j]), k] + D[(k, j)])

    return op(T[frozenset(xrange(1, len(city)+1)), j] for j in xrange(2, len(city)+1))

n = 1
for line in stdin:
    match = re.match('([A-Za-z]+) to ([A-Za-z]+) = ([0-9]+)', line)
    if not match.group(1) in city:
        city[match.group(1)] = n
        n += 1
    if not match.group(2) in city:
        city[match.group(2)] = n
        n += 1

    a = city[match.group(1)]
    b = city[match.group(2)]

    D[(a, b)] = int(match.group(3))
    D[(b, a)] = int(match.group(3))

print min(tsp(i, min, inf) for i in xrange(1, len(city)+1))
print max(tsp(i, max, -inf) for i in xrange(1, len(city)+1))
