from functools import reduce
import operator
import sys

def npos(hmap, i, j):
    return [(ii, jj) for ii, jj in [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]
            if ii >= 0 and jj >= 0 and ii < len(hmap) and jj < len(hmap[0])]

def neighbors(hmap, i, j):
    return [hmap[ii][jj] for ii, jj in npos(hmap, i, j)]

def lowpoints(hmap):
    return [(i, j) for i in range(len(hmap)) for j in range(len(hmap[0]))
            if all(hmap[i][j] < n for n in neighbors(hmap, i, j))]

def basin(hmap, lps):
    def flood(i, j, k):
        if hmap[i][j] in {9, k}:
            return set()
        hmap[i][j] = k
        return reduce(operator.or_, (flood(ii, jj, k) for ii, jj in npos(hmap, i, j)), {(i, j)})
    return sorted((flood(i, j, k+20) for k, (i, j) in enumerate(lps)), key=len)

hmap = [list(map(int, list(line[:-1]))) for line in sys.stdin]

lps = lowpoints(hmap)
risk = sum(hmap[i][j]+1 for i, j in lps)
print (f'Sum of the lowpoint risk levels: {risk}')

basins = basin(hmap, lps)
prod = reduce(operator.mul, map(len, basins[-3:]), 1)
print (f'Product of 3 largest basins: {prod}')
