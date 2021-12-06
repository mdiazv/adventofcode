# https://adventofcode.com/2021/day/6

def simulate(P, n):
    for k in range(1, n+1):
        z, P = P[0], P[1:]+[P[0]]
        P[6] += z
    return P

P = [0] * 9
for k in map(int, input().split(',')):
    P[k] += 1

for d in [80, 256, 1024, 8192]:
    print (f'Population after {d} days: {sum(simulate(P, d))}')
