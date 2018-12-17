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
		if sum(distances) < 10000 :
			arr[x][y] = 1

print(sum(sum(arr)))
