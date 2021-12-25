from dataclasses import dataclass
from functools import reduce, partial
import operator

DEBUG = False

def debug(*a):
    if DEBUG:
        print (*a)

def to_bin(s):
    return ''.join(f'{int(c, 16):0>4b}' for c in s)

mul = lambda ls: reduce(operator.mul, ls)
lt = lambda ls: int(reduce(operator.lt, ls))
gt = lambda ls: int(reduce(operator.gt, ls))
eq = lambda ls: int(reduce(operator.eq, ls))

class Packet:
    ops = {0: sum, 1: mul, 2: min, 3: max, 5: gt, 6: lt, 7: eq}
    @staticmethod
    def parse(raw):
        return Packet._parse(to_bin(raw), 0)[0]
    @staticmethod
    def _parse(bs, off):
        v, t = int(bs[off:off+3], 2), int(bs[off+3:off+6], 2)
        debug (f'parse packed from {off}: {bs[off:]}')
        debug (f'v {v}, t {t}')
        if t == 4:
            val, pos = Literal.parse(v, t, bs, off)
            return val, pos
        else:
            i = int(bs[off+6])
            val, pos = Operator.parse(v, t, i, bs, off)
            return val, pos

@dataclass
class Literal(Packet):
    version: int
    type: int
    value: int
    @staticmethod
    def parse(ver, typ, bs, off):
        debug (f'parse literal from {off}: {bs[off:]}')
        assert typ == 4
        k, v = 6, ''
        while k < len(bs):
            chunk = bs[off+k:off+k+5] 
            v += chunk[1:]
            k += 5
            if chunk[0] == '0':
                break
        return Literal(ver, typ, int(v, 2)), k+off
    def versum(self):
        return self.version
    def eval(self):
        return self.value

@dataclass
class Operator(Packet):
    version: int
    type: int
    operands: list[Packet]
    @staticmethod
    def parse(ver, typ, lt, bs, off):
        assert typ != 4
        debug ('length type', lt)
        if lt == 0:
            ops, pos = Operator._parse_ops_fixed(bs, off+7+15, int(bs[off+7:off+7+15], 2))
        else:
            ops, pos = Operator._parse_ops_n(bs, off+7+11, int(bs[off+7:off+7+11], 2))
        return Operator(ver, typ, ops), pos
    @staticmethod
    def _parse_ops_n(bs, off, n):
        ops = []
        debug (f'parse {n} ops from {off}: {bs[off:]}')
        for _ in range(n):
            p, off = Packet._parse(bs, off)
            ops.append(p)
        return ops, off
    @staticmethod
    def _parse_ops_fixed(bs, off, ln):
        ops, start = [], off
        debug (f'parse {ln} bits of ops from {off}: {bs[off:]}')
        while off < start+ln:
            p, off = Packet._parse(bs, off)
            ops.append(p)
        return ops, off
    def versum(self):
        return self.version + sum(map(lambda o: o.versum(), self.operands))
    def eval(self):
        return self.ops[self.type](o.eval() for o in self.operands)

packets = ['8A004A801A8002F478', '620080001611562C8802118E34', 'C0015000016115A2E0802F182340',
           'A0016C880162017C3686B18A3D4780']
expected = [16, 12, 23, 31]
for p, e in zip(packets, expected):
    c = Packet.parse(p)
    assert c.versum() == e

packets = ['C200B40A82', '04005AC33890', '880086C3E88112', 'CE00C43D881120', 'D8005AC2A8F0', 'F600BC2D8F',
           '9C005AC2F8F0', '9C0141080250320F1802104A08']
expected = [3, 54, 7, 9, 1, 0, 0, 1]
for p, e in zip(packets, expected):
    c = Packet.parse(p)
    assert c.eval() == e

raw = input()
packet = Packet.parse(raw)
print (f'Version sum: {packet.versum()}')
print (f'Packet evaluates to: {packet.eval()}')
