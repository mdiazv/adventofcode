from sys import stdin

s = stdin.read()
print s.count('(') - s.count(')')

c = 0
for p in xrange(len(s)):
    if s[p] == '(':
        c += 1
    else:
        c -= 1
    if c == -1:
        print p+1
        break
