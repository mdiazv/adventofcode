# https://adventofcode.com/2021/day/5
from collections import Counter
from dataclasses import dataclass
import sys

@dataclass
class Point:
    x: int
    y: int
    @staticmethod
    def parse(s):
        return Point(*map(int, s.split(',')))
    def __hash__(self):
        return hash((self.x, self.y))
    def __add__(self, other):
        return Point(self.x+other.x, self.y+other.y)
    def __mul__(self, n):
        if isinstance(n, int):
            return Point(self.x*n, self.y*n)
        return NotImplemented

class Line:
    def __init__(self, s):
        parts = s.split()
        self.p1 = Point.parse(parts[0])
        self.p2 = Point.parse(parts[2])
        dx = self.p2.x - self.p1.x
        dy = self.p2.y - self.p1.y
        self.diag = dx and dy
        self.delta = Point(dx//abs(dx) if dx else 0, dy//abs(dy) if dy else 0)
        self.count = max(abs(dx), abs(dy))
    def __repr__(self):
        return f'{self.p1} -> {self.p2}'
    def points(self, diagonal):
        if not diagonal and self.diag:
            return []
        return [self.p1 + self.delta*d for d in range(self.count+1)] # slow (:

def overlap(Ls, diagonal):
    c = Counter(p for l in Ls for p in l.points(diagonal))
    return sum(v > 1 for _, v in c.items())

Ls = [Line(line) for line in sys.stdin]
print (f'Sum of overlapping horizontal and vertical lines: {overlap(Ls, diagonal=False)}')
print (f'Sum of overlapping lines: {overlap(Ls, diagonal=True)}')
