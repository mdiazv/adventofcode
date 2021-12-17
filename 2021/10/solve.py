from functools import reduce
from statistics import median
import sys

corrupt_score = { ')': 3, ']': 57, '}': 1197, '>': 25137 }
autocomplete_score = { ')': 1, ']': 2, '}': 3, '>': 4 }
closing = { '(': ')', '[': ']', '{': '}', '<': '>' }

def times5plus(a, b):
    return a*5 + b

def score(s, stack=[]):
    if not s:
        return 0, [reduce(times5plus, map(autocomplete_score.get, reversed(stack)), 0)]
    if stack and s[0] not in closing and s[0] != stack[-1]:
        return corrupt_score[s[0]], []
    if s[0] in closing:
        return score(s[1:], stack + [closing[s[0]]])
    return score(s[1:], stack[:-1])

def tuplesum(a, b):
    return (a[0]+b[0], a[1]+b[1])

nav = [line[:-1] for line in sys.stdin]
corrupted, autocomplete = reduce(tuplesum, map(score, nav), (0, []))
print (f'Corrupted syntax error score: {corrupted}')
print (f'Autocomplete score: {median(autocomplete)}')
