from sys import stdin

s = 0
r = 0
for line in stdin:
    d = map(int, line.split('x'))
    s += 2 * (d[0] * d[1] + d[1] * d[2] + d[2] * d[0]) + min(d[0] * d[1], d[1] * d[2], d[2] * d[0])
    sides = sorted([2 * d[0], 2 * d[1], 2 * d[2]])
    r += sides[0] + sides[1] + d[0] * d[1] * d[2]

print s
print r
