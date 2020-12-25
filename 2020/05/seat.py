import sys

tr = {
    'F': 0, 'L': 0,
    'B': 1, 'R': 1,
}

def bp_to_int(seq):
    return [tr[c] for c in seq]

def bsp(seq):
    low, high = 0, 2 ** len(seq)
    steps = bp_to_int(seq)
    for step in steps:
        half = (high + low) // 2
        if step == 0:
            high = half
        else:
            low = half
    return low

def decode(bp):
    row, column = bp[:7], bp[7:]
    return (bp, bsp(row), bsp(column))

def seat_id(decoded):
    _, r, c = decoded
    return r*8+c

BPs = [decode(line[:-1]) for line in sys.stdin]
print ('Highest Seat ID', max(map(seat_id, BPs)))

SIDs = sorted(seat_id(bp) for bp in BPs)
print (SIDs)

for a, b in zip(SIDs, SIDs[1:]):
    if b != a+1:
        print ('My Seat ID', a+1)
        break
