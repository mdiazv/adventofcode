from functools import partial
import sys

class Board:
    def __init__(self, rows):
        self.ns = set([n for row in rows for n in row])
        self.hit = set()
        self.R = [5] * 5
        self.C = [5] * 5
        self.rows = rows
        self.pos = {rows[i][j]: (i, j) for i in range(5) for j in range(5)}
    def __repr__(self):
        s = ' B  I  N  G  O\n'
        return s + '\n'.join(['{:2d} {:2d} {:2d} {:2d} {:2d}'.format(*row) for row in self.rows])
    def call(self, n):
        if n in self.pos:
            i, j = self.pos[n]
            self.R[i] -= 1
            self.C[j] -= 1
            self.hit.add(n)
            return self.R[i] == 0 or self.C[j] == 0
    def score(self):
        return sum(self.ns-self.hit)

def play(seq, boards):
    bingo = [False] * len(boards)
    for n in seq:
        for k, board in enumerate(boards):
            if not bingo[k]:
                bingo[k] = board.call(n)
                if bingo[k]:
                    yield n, k, board

def win_order(seq, boards):
    return list(play(seq, boards))

seq = list(map(int, input().split(',')))
lines = [line[:-1] for line in sys.stdin]
boards = [Board([list(map(int, s.split())) for s in lines[k+1:k+6]]) for k in range(0, len(lines), 6)]

win_order = list(play(seq, boards))
n, k, board = win_order[0]
print (f'First Bingo called with {n} on board {k+1}. Winning board score: {board.score()*n}')
n, k, board = win_order[-1]
print (f'Last Bingo called with {n} on board {k+1}. Winning board score: {board.score()*n}')
