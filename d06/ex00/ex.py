import re as re
import numpy as np

def str_to_tuple (s) :
    m = re.match(r"(\d+), (\d+)", s)
    return (int)(m.group(1)), (int)(m.group(2))

fname = 'input.txt'
with open(fname) as f:
    lines = [str_to_tuple(line) for line in f]
min_x = min([i for i, _ in lines])
min_y = min([j for _, j in lines])
lines = [(i - min_x, j - min_y) for i, j in lines]
max_x = max([i for i, _ in lines])
max_y = max([j for _, j in lines])
arr = np.zeros((max_x + 1, max_y + 1))

def dist (i, j) :
	(a, b), (x, y) = i, j
	return abs(a - x) + abs(b - y)

for x in range(max_x + 1) :
	for y in range(max_y + 1) :
		distances = np.array([dist((x, y), p) for p in lines])
		min_dist = min(distances)
		if sum(distances == min_dist) > 1 :
			arr[x][y] = -1
		else :
			arr[x][y] = np.argmin(distances)

eligible = range(len(lines))
eligible = [i for i in eligible if i not in arr[:, 0]]
eligible = [i for i in eligible if i not in arr[:, max_y]]
eligible = [i for i in eligible if i not in arr[0, :]]
eligible = [i for i in eligible if i not in arr[max_x, :]]

counts = [sum(sum(arr == i)) for i in eligible]
print(max(counts))