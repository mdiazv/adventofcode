import re
import sys

def byr(v):
    return _int(v, 4, 1920, 2002)
def iyr(v):
    return _int(v, 4, 2010, 2020)
def eyr(v):
    return _int(v, 4, 2020, 2030)
def hgt(v):
    m = re.match(r'^(\d+)(cm|in)$', v)
    if m:
        if m.group(2) == 'cm':
            return _int(m.group(1), 3, 150, 193)
        return _int(m.group(1), 2, 59, 76)
def hcl(v):
    return re.match(r'^#[0-9a-f]{6}$', v)
def ecl(v):
    return v in { 'amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth' }
def pid(v):
    return re.match(r'^\d{9}$', v)
def cid(v):
    return True
def _int(v, digits, min, max):
    if len(v) != digits:
        return False
    try:
        k = int(v)
        return min <= k and k <= max
    except:
        return False

fields = { 
    'byr': byr,
    'iyr': iyr,
    'eyr': eyr,
    'hgt': hgt,
    'hcl': hcl,
    'ecl': ecl,
    'pid': pid,
    'cid': cid,
}


class Passport(dict):
    def __init__(self, s):
        self.d = {}
        for kv in s.split(' '):
            if kv: 
                k, v = kv.split(':')
                self.d[k] = v
    def __repr__(self):
        return 'Passport({s.d})'.format(s=self)
    def __getitem__(self, key):
        return self.d[key]
    def __contains__(self, key):
        return key in self.d

class Validator:
    _required = [k for k in fields if k != 'cid']
    @classmethod
    def validate(cls, passport):
        return all(f in passport for f in cls._required)

class StrictValidator(Validator):
    @classmethod
    def validate(cls, passport):
        if not super().validate(passport):
            return False
        for f in cls._required:
            if not fields[f](passport[f]):
                return False
        return True
raw = sys.stdin.read(1024*1024)
joined = re.sub(r'(\w)\n', '\g<1> ', raw)

P = [Passport(line) for line in joined.split('\n') if line]
V = [Validator, StrictValidator]

for v in V:
    print (v.__name__, sum(map(v.validate, P)))
