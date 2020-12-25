from functools import reduce
import sys

raw = sys.stdin.read(8192*1024)
groups = raw[:-1].split('\n\n')
Gs = [g.split('\n') for g in groups]

Yes = [set(''.join(g)) for g in Gs]
print ('Sum of counts (Anyone)', sum(map(len, Yes)))

Int = [reduce(set.intersection, g, set('abcdefghijklmnopqrstuvwxyz')) for g in Gs]
print ('Sum of counts (Everyone)', sum(map(len, Int)))
