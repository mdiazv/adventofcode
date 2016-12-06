from sys import stdin

r = 0
R = 0
for s in stdin:
    s = s[:-1]
    r += len(s) - len(eval(s))

    v = R
    for c in s:
        if c == '"':
            R += 2
        if c == '\\':
            R += 1
    R -= s.count('"') - 2

print(r)
print(R)
