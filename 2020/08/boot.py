import sys


class Instruction:
    def __init__(self, k):
        self.k = k
    def __repr__(self):
        return 'Instruction({s.__class__.__name__}, {s.k})'.format(s=self)
    def run(self, computer):
        pass
    @staticmethod
    def parse(s):
        parts = s.split()
        return globals()[parts[0]](int(parts[1]))

class acc(Instruction):
    def run(self, computer):
        computer.acc += self.k
class jmp(Instruction):
    def run(self, computer):
        computer.pc += self.k - 1
class nop(Instruction):
    pass

class Computer:
    def __init__(self):
        pass
    def reset(self):
        self.acc, self.pc = 0, 0
    def run(self, p, verbose=True):
        self.reset()
        seen = set()
        while self.pc < len(p):
            i = p[self.pc]
            if i in seen:
                if verbose:
                    print ('Repeated instruction', i, 'acc', self.acc)
                return False
            seen.add(i)
            i.run(self)
            self.pc += 1
        print ('End of program', 'acc', self.acc)
        return True

def fix(C, P):
    for k in range(len(P)):
        i = P[k]
        if isinstance(i, nop):
            P[k] = jmp(i.k)
        elif isinstance(i, jmp):
            P[k] = nop(i.k)
        if C.run(P, verbose=False):
            print ('Success by changing instruction', k, i, 'to', P[k])
            return True
        P[k] = i

P = [Instruction.parse(line[:-1]) for line in sys.stdin]
C = Computer()
C.run(P)

fix(C, P)
