import hashlib

key = 'yzbqklnj'
md5 = hashlib.md5()
md5.update(key)

i = 0
h = ''
while not h.startswith('00000'):
    m = md5.copy()
    m.update(str(i))
    h = m.hexdigest()
    i += 1

print i-1

i = 0
h = ''
while not h.startswith('000000'):
    m = md5.copy()
    m.update(str(i))
    h = m.hexdigest()
    i += 1

print i-1
