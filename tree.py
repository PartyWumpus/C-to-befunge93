
tree: dict[tuple[int,...], str] = {}

queue = [((), "")]

def traverse_from():
    global tree

    start_points: list[tuple[tuple[int,...], str]] = [((), "")]
    # 0 val operations
    for a in range(9,-1,-1):
        start_points.append(((a,), str(a)))
        for b in range(9,-1,-1):
            start_points.append(((a,b), str(b) + str(a)))

    for a in range(32,127):
        if a == 34:
            continue
        start_points.append(((a,), f'"{chr(a)}"'))
        for b in range(32,127):
            if b == 34:
                continue
            start_points.append(((a,b,), f'"{chr(b)}{chr(a)}"'))

    while len(start_points) > 0:
        queue = [start_points[0]]
        del start_points[0]
        while len(queue) > 0:
            tup, steps = queue[0]
            del queue[0]
            tup = normalize(tup)

            if len(steps) > 10:
                continue

            if len(tup) > 6:
                continue

            x = tree.get(tup)
            if x is None or len(steps) < len(x):
                tree[tup] = steps
            else:
                continue

            # 2 val operations
            if len(tup) >= 2:
                queue.append((tup[:-2] + (tup[-2] * tup[-1],), steps + '*'))
                queue.append((tup[:-2] + (tup[-2] + tup[-1],), steps + '+'))
                queue.append((tup[:-2] + (tup[-2] - tup[-1],), steps + '-'))

            # 2 val operations
            if len(tup) >= 1 and tup[-1] != 0:
                queue.append((tup + (tup[-1],), steps + ':'))

            # 1 val operations
            if len(tup) == 1:
                #queue.append(((-tup[0],), steps + '-'))
                pass



def normalize(tup: tuple[int,...]):
    for i, v in enumerate(tup):
        if v != 0:
            return tup[i:]
    return ()

try:
    traverse_from()
except BaseException as e:
    raise e 

out = {}
for k, v in tree.items():
    if len(k) == 1: 
        out[k[0]] = v

print(out)
