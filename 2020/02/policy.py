from collections import Counter
import sys

class Policy:
    @classmethod
    def from_string(cls, p):
        ps = p.split(' ')
        a, b = list(map(int, ps[0].split('-')))
        return Policy(a, b, ps[1])
    @classmethod
    def from_policy(cls, p):
        return cls(p.a, p.b, p.char)
    def __init__(self, a, b, char):
        self.a, self.b, self.char = a, b, char
    def __repr__(self):
        return 'Policy({s.a}, {s.b}, {s.char})'.format(s=self)
    def check(self, password):
        pass

class CountPolicy(Policy):
    def check(self, password):
        c = Counter(password)
        rep = c.get(self.char, -1)
        return self.a <= rep and rep <= self.b

class PositionPolicy(Policy):
    def check(self, password):
        Cs = [password[self.a-1], password[self.b-1]]
        return sum(c == self.char for c in Cs) == 1

def parse(policy, password):
    return (Policy.from_string(policy), password[1:-1])

Ps = [parse(*s.split(':')) for s in sys.stdin.readlines()]
for cls in [CountPolicy, PositionPolicy]:
    print (cls.__name__, sum(cls.from_policy(policy).check(password) for policy, password in Ps))
