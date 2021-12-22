import heapq as H
import sys

def lowest_risk(grid):
    R, C = len(grid), len(grid[0])
    def nbs(i, j):
        return [(ii, jj) for ii, jj in [(i+1, j), (i-1, j), (i, j+1), (i, j-1)]
                if 0 <= ii < R and 0 <= jj < C]

    visited = set()
    states = [(0, (0, 0))]
    while states:
        risk, (i, j) = H.heappop(states)
        if (i, j) == (R-1, C-1):
            return risk
        if (i, j) in visited:
            continue
        visited.add( (i, j) )
        for ii, jj in nbs(i, j):
            H.heappush(states, (risk+grid[ii][jj], (ii, jj)))

def expand(grid, times):
    R, C = len(grid), len(grid[0])
    exp = [[0 for _ in range(times*C)] for _ in range(times*R)]
    for oi in range(0, R*times, R):
        for oj in range(0, C*times, C):
            add = oi // R + oj // R
            for i in range(R):
                for j in range(C):
                    v = grid[i][j] + add
                    exp[i+oi][j+oj] = v if v <= 9 else v - 9
    return exp

grid = [list(map(int, line[:-1])) for line in sys.stdin]
risk = lowest_risk(grid)
print (f'Lowest total risk: {risk}')

risk = lowest_risk(expand(grid, 5))
print (f'Lowest total risk on expanded grid: {risk}')
