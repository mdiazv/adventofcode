from sys import stdin

vowels = set('aeiou')
n = ['ab', 'cd', 'pq', 'xy']

def nice(s):
    v = 0
    t = False
    l = len(s)
    for i in xrange(l):
        v += int(s[i] in vowels)
        if i > 0:
            if s[i] == s[i-1]:
                t = True
            if ((s[i] == 'b' and s[i-1] == 'a')
                or (s[i] == 'd' and s[i-1] == 'c')
                or (s[i] == 'q' and s[i-1] == 'p')
                or (s[i] == 'y' and s[i-1] == 'x')):
                    return False
    return v >= 3 and t

def nicer(s):
    p = False
    r = False
    l = len(s)
    ps = {}

    for i in xrange(l):
        if i > 0:
            if i > 1:
                if s[i] == s[i-2]:
                    r = True
            pp = s[i-1:i+1]
            if pp in ps:
                if i > ps[pp]:
                    p = True
            else:
                ps[pp] = i+1
        if p and r:
            return True

    return False

s = stdin.readlines()
print sum(1 for line in s if nice(line))
print sum(1 for line in s if nicer(line))
