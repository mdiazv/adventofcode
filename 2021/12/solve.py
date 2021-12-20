from collections import defaultdict, Counter
import sys

def small_once(k, path):
    return k.isupper() or k not in path

def small_twice(k, path):
    if k.isupper():
        return True
    if k in {'start', 'end'} or Counter(filter(str.islower, path)).most_common(1)[0][1] > 1:
        return k not in path
    return True

def paths(g, can_visit):
    r = []
    def dfs(n, path):
        if n == 'end':
            r.append(path)
            return
        for k in g[n]:
            if can_visit(k, path):
                dfs(k, path+[k])
    dfs('start', ['start'])
    return r

g = defaultdict(lambda: [])
for a, b in (line[:-1].split('-') for line in sys.stdin):
    g[a].append(b)
    g[b].append(a)

ps = paths(g, small_once)
print (f'There are {len(ps)} paths visiting small caves only once')

ps = paths(g, small_twice)
print (f'There are {len(ps)} paths visiting at most one small cave twice and the rest once')
