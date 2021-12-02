import sys

def increases(seq):
    return sum(a < b for a, b in zip(seq, seq[1:]))

ms = list(map(int, sys.stdin))
print (f'Measurment increased {increases(ms)} times')

avg3 = list(map(sum, zip(ms, ms[1:], ms[2:])))
print (f'Averages increased {increases(avg3)} times')
