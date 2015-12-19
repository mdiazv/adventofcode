from sys import stdin
import re

cmds = {
    'turn on': lambda x: 1,
    'turn off': lambda x: 0,
    'toggle': lambda x: x ^ 1,
}

newcmds = {
    'turn on': lambda x: x+1,
    'turn off': lambda x: max(x-1, 0),
    'toggle': lambda x: x+2,
}

m = [[0] * 1000 for i in xrange(1000)]
M = [[0] * 1000 for i in xrange(1000)]

def do(cmd, m, start, end):
    for i in xrange(start[0], end[0]+1):
        for j in xrange(start[1], end[1]+1):
            m[i][j] = cmd(m[i][j])

for line in stdin:
    match = re.match(r'(turn on|turn off|toggle) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)', line)
    cmd = cmds[match.group(1)]
    newcmd = newcmds[match.group(1)]
    start = tuple(map(int, match.group(2, 3)))
    end = tuple(map(int, match.group(4, 5)))

    do(cmd, m, start, end)
    do(newcmd, M, start, end)

print sum(m[i][j] for i in xrange(1000) for j in xrange(1000))
print sum(M[i][j] for i in xrange(1000) for j in xrange(1000))
