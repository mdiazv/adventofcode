from statistics import median

def linear_cost(Ps):
    target = int(median(Ps))
    fuel = sum(abs(target-p) for p in Ps)
    return target, fuel

def exp_cost(Ps):
    m, M = min(Ps), max(Ps)
    C = [0] * (M+1)
    for i, p in enumerate(Ps):
        for k in range(M+1):
            n = abs(Ps[i]-k)
            C[k] += (n * (n+1)) // 2
    target = min(range(len(C)), key=C.__getitem__)
    return target, C[target]

Ps = list(map(int, input().split(',')))
for cost in [linear_cost, exp_cost]:
    target, fuel = cost(Ps)
    print (f'Minimum fuel to align the crabs @ {target} with {cost.__name__}: {fuel}')
