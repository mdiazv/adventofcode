# https://adventofcode.com/2021/day/3
from functools import partial, reduce
from operator import eq, gt, le
import sys

def rates(ss):
    n, m = len(ss[0]), len(ss)
    r = reduce(lambda r, s: [r[k]+int(s[k]) for k in range(n)], ss, [0]*n)
    gamma = ''.join(str(int(r[k] > m//2)) for k in range(n))
    epsilon = ''.join('01'[c == '0'] for c in gamma)
    return int(gamma, base=2), int(epsilon, base=2)

def keep(cmp, ss, k=0):
    n = len(ss)
    if n == 1:
        return int(ss[0], base=2)
    r = sum(int(s[k]) for s in ss)
    v = '01'[cmp(r, n)]
    return keep(cmp, list(filter(lambda s: eq(v, s[k]), ss)), k+1)

def base_cmp(cmp, on_eq, r, n):
    return on_eq if n%2 == 0 and r == n//2 else cmp(r, n//2)

ss = [line[:-1] for line in sys.stdin]
gamma, epsilon = rates(ss)
print (f'Power consumption: {gamma*epsilon}')

oxigen_cmp, co2_cmp = partial(base_cmp, gt, True), partial(base_cmp, le, False)
oxigen, co2 = keep(oxigen_cmp, ss), keep(co2_cmp, ss)
print (f'Life support rating: {oxigen*co2}')
