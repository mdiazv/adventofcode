from sys import stdin
import operator
import re

ops = {}

class OP:
    def __init__(self, a, b, op, dest):
        self.aa = a
        self.bb = b
        self.op = op
        self.dest = dest
        self.reset()
    def eval(self):
        if self.r is None:
            print 'figuring out {}'.format(self.dest)
            if not isinstance(self.a, int):
                print 'first I need to solve {}'.format(self.a)
                self.a = ops[self.a].eval()
            if not isinstance(self.b, int):
                print 'second I need to solve {}'.format(self.b)
                self.b = ops[self.b].eval()
            print 'applying {} {} {} -> {}'.format(self.a, self.op, self.b, self.dest)
            self.r = self.op(self.a, self.b) & 0xFFFF
        return self.r
    def reset(self):
        self.a = int(self.aa) if self.aa.isdigit() else self.aa
        self.b = int(self.bb) if self.bb.isdigit() else self.bb
        self.r = None

for line in stdin:
    match = re.match(r'([A-Za-z0-9 ]+) -> ([a-z]+)', line)
    source = match.group(1)
    dest = match.group(2)

    match = re.match(r'([a-z0-9]+)$', source)
    if match:
        ops[dest] = OP(match.group(1), '0', operator.add, dest)

    match = re.match(r'([a-z0-9]+) (AND|OR) ([a-z0-9]+)$', source)
    if match:
        if match.group(2) == 'AND':
            ops[dest] = OP(match.group(1), match.group(3), operator.and_, dest)
        else:
            ops[dest] = OP(match.group(1), match.group(3), operator.or_, dest)

    match = re.match(r'([a-z0-9]+) (LSHIFT|RSHIFT) ([a-z0-9]+)$', source)
    if match:
        if match.group(2) == 'LSHIFT':
            ops[dest] = OP(match.group(1), match.group(3), operator.lshift, dest)
        else:
            ops[dest] = OP(match.group(1), match.group(3), operator.rshift, dest)

    match = re.match(r'NOT ([a-z0-9]+)$', source)
    if match:
        ops[dest] = OP(match.group(1), '1', operator.xor, dest)

print ops['a'].eval()

ops['b'] = OP(str(ops['a'].eval()), '0', operator.add, 'b')
for d, op in ops.items():
    op.reset()

print ops['a'].eval()
