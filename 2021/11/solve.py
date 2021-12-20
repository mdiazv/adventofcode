import sys

class World:
    def __init__(self, w):
        self.w = w
        self.R, self.C = len(w), len(w[0])
        self.gen, self.flashes = 0, 0
        self.superflashes = []
    def step(self):
        self.flashed = set()
        for i in range(self.R):
            for j in range(self.C):
                self.inc(i, j)
        for i, j in self.flashed:
            self.w[i][j] = 0
        self.flashes += len(self.flashed)
        if len(self.flashed) == self.R*self.C:
            self.superflashes.append(self.gen)
        self.gen += 1
    def inc(self, i, j):
        self.w[i][j] += 1
        if self.w[i][j] > 9 and (i, j) not in self.flashed:
            self.flashed.add( (i, j) )
            for ii, jj in self.neighbors(i, j):
                self.inc(ii, jj)
    def neighbors(self, i, j):
        return ((ii, jj) for ii in range(i-1, i+2) for jj in range(j-1, j+2)
                if (i, j) != (ii, jj) and ii >= 0 and jj >= 0 and ii < self.R and jj < self.C)
    def __repr__(self):
        return '\n'.join(''.join(map(str, row)) for row in self.w) + f'\ngen {self.gen} - {self.flashes} flashes'

w = World([list(map(int, line[:-1])) for line in sys.stdin])
while not w.superflashes:
    if w.gen == 100:
        print (w)
        print(f'Flashes after 100 steps: {w.flashes}')
    w.step()
print (w)
print(f'First super flash at step {w.gen}')
