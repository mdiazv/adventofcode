
def looksay(s):
    r = 0
    p = None
    out = ''
    for c in s:
        if c != p:
            if p:
                out += '{}{}'.format(r, p)
            r = 0
            p = c
        r += 1
    out += '{}{}'.format(r, p)
    return out
                

key = '1321131112'

for _ in xrange(40):
    key = looksay(key)

print len(key)

for _ in xrange(10):
    key = looksay(key)

print len(key)
