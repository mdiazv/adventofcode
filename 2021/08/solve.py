import sys

def parse(line):
    obs, data = map(str.split, line.split(' | '))
    return (sorted(map(frozenset, obs), key=len), list(map(frozenset, data)))

def decode(obs):
    ob, data = obs
    # ['ab', 'abd', 'abef', 'abcdf', 'acdfg', 'bcdef', 'abcdef', 'abcdeg', 'bcdefg', 'abcdefg']
    # [ 1  ,    7 ,     4 ,      3 ,       2,      5 ,        9,        0,        6,        8 ]

    le_1 = 0
    le_4 = 2
    le_3 = [k for k in range(3, 6) if len(ob[k]-ob[le_1]) == 3][0]
    le_5 = [k for k in range(3, 6) if k != le_3 and len(ob[k]-ob[le_4]) == 2][0]
    le_2 = 3 + 4 + 5 - le_3 - le_5
    le_9 = [k for k in range(6, 9) if ob[k] == ob[le_5]|ob[le_1]][0]
    le_6 = [k for k in range(6, 9) if ob[k] == (ob[le_2]-ob[le_3])|ob[le_5]][0]
    le_0 = 6 + 7 + 8 - le_6 - le_9

    tr = {ob[0]: 1, ob[1]: 7, ob[2]: 4, ob[9]: 8,
          ob[le_3]: 3, ob[le_5]: 5, ob[le_2]: 2,
          ob[le_6]: 6, ob[le_9]: 9, ob[le_0]: 0}

    return [tr[p] for p in data]

Obs = [parse(line) for line in sys.stdin]
Dec = [decode(ob) for ob in Obs]

times = sum(len(list(filter(lambda k: k in {1, 4, 7, 8}, d))) for d in Dec)
print (f'Ocurrences of digits 1, 4, 7 and 8 appear: {times}')

val = sum(int(''.join(map(str, d))) for d in Dec)
print (f'Sum of all outputs: {val}')
