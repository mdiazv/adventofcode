import re
import sys


class Rule:
    @staticmethod
    def parse(s):
        parts = s.split(' ')
        k = parts.index('bags')
        name = ' '.join(parts[:k])
        rest = parts[k+2:]
        contains = [
            (rest[k+1] + ' ' + rest[k+2], int(rest[k]))
            for k in range(0, len(rest), 4)
            if rest[k] != 'no'
        ]
        return name, contains


def can_contain(Rs, goal, start):
    closed = set()
    def dfs(bag):
        if bag == goal:
            return True
        if bag in closed:
            return False
        closed.add(bag)
        return bool(any(dfs(b) for b, n in Rs[bag]))
    return dfs(start)


def must_contain(Rs, bag):
    return sum(n*(1+must_contain(Rs, b)) for b, n in Rs[bag])


Rs = dict(Rule.parse(line[:-1]) for line in sys.stdin)

goal = 'shiny gold'
s = sum(can_contain(Rs, goal, c) for c in Rs.keys() if c != goal)
print ('Colors that can contain', goal , s)

n = must_contain(Rs, goal)
print ('A', goal, 'bag requires', n, 'other bags')
