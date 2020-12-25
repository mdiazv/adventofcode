import sys

expected = 2020
A = set(int(line) for line in sys.stdin.readlines())
Ds = {expected - a: a for a in A}

for a in A:
    if a in Ds:
        print (a, '*', Ds[a], '=', a * Ds[a])
        break

got_it = False
for a in A:
    DDs = {d - a: d for d in Ds}
    for b in A:
        if b in DDs:
            print (a, '*', b, '*', DDs[b], '=', a * b * (2020-b-a))
            got_it = True
            break
    if got_it:
        break

