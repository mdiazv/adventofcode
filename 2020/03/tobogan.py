import sys

class World:
    def __init__(self, world):
        self.world = world
        self.w, self.h = len(world[0]), len(world)
    def get(self, i, j):
        return self.world[i][j%self.w]
    def count_trees(self, slope):
        x, y = slope
        i, j, trees = 0, 0, 0
        while i < self.h:
            trees += self.get(i, j) == '#'
            i, j = i+y, j+x
        return trees

slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
W = World([line[:-1] for line in sys.stdin])

prod = 1
for slope in slopes:
    trees = W.count_trees(slope)
    prod *= trees
    print ('slope', slope, 'trees', trees)

print ('Product of total trees:', prod)
