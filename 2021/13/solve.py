from itertools import takewhile
from operator import itemgetter
import sys

class Paper:
    def __init__(self, dots):
        self.dots = set(dots)
        self.R, self.C = max(map(itemgetter(1), dots))+1, max(map(itemgetter(0), dots))+1
    def __repr__(self):
        w = [['.' for _ in range(self.C)] for _ in range(self.R)]
        for j, i in self.dots:
            w[i][j] = '#'
        return '\n'.join(map(''.join, w)) + '\n --- '
    def fold(self, f):
        axis, k = f[0], int(f[1])
        if axis == 'y':
            self.dots = set(((x, y if y < k else self.R-y-1) for x, y in self.dots))
            self.R //= 2
        else:
            self.dots = set(((x if x < k else self.C-x-1, y) for x, y in self.dots))
            self.C //= 2

paper = Paper([tuple(map(int, s.split(','))) for s in takewhile(lambda s: s != '\n', sys.stdin)])
folds = [line[:-1].split()[2].split('=') for line in sys.stdin]
f, fs = folds[0], folds[1:]

paper.fold(f)
print (f'There are {len(paper.dots)} dots visible after 1 fold')

for f in fs:
    paper.fold(f)
print ('The code is:')
print (paper)

