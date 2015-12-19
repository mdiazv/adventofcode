from sys import stdin

p = (0, 0)
v = set([p])
ds = stdin.read()

moves = {
    '^': (0,  1),
    'v': (0, -1),
    '<': (-1, 0),
    '>': ( 1, 0), 
}

def add(a, b):
    return (a[0]+b[0], a[1]+b[1])

for d in ds:
    if d in moves:
        p = add(p, moves[d])
        v.add(p)

print len(v)

i = 0
v = set([(0, 0)])
p = [(0, 0), (0, 0)]

for d in ds:
    if d in moves:
        p[i] = add(p[i], moves[d])
        v.add(p[i])
        i = (i+1) % 2

print len(v)
