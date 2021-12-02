from dataclasses import dataclass
from functools import partial, reduce
import sys
sys.setrecursionlimit(100000)

@dataclass
class Sub1:
    x: int
    y: int

InstructionSet1 = {
    'forward': lambda x, s: Sub1(s.x+x, s.y),
    'down': lambda x, s: Sub1(s.x, s.y+x),
    'up': lambda x, s: Sub1(s.x, s.y-x),
}

@dataclass
class Sub2(Sub1):
    aim: int

InstructionSet2 = {
    'forward': lambda x, s: Sub2(s.x+x, s.y+s.aim*x, s.aim),
    'down': lambda x, s: Sub2(s.x, s.y, s.aim+x),
    'up': lambda x, s: Sub2(s.x, s.y, s.aim-x),
}

def run_with(IS, s):
    cmd, x = s.split()
    return partial(IS[cmd], int(x))

def _compose(f, g):
    return lambda *a, **kw: f(g(*a, **kw))

def compose(*fs):
    return reduce(_compose, reversed(fs))

def parse(InstructionSet, cmds):
    return compose(*map(partial(run_with, InstructionSet), cmds))

cmds = list(map(str, sys.stdin))
p = parse(InstructionSet1, cmds)(Sub1(0, 0))
print (f'Arrived to position {p}. Product: {p.x * p.y}')

p = parse(InstructionSet2, cmds)(Sub2(0, 0, 0))
print (f'Arrived to position {p}. Product: {p.x * p.y}')
