#!/usr/bin/env python3

from itertools import cycle
from functools import reduce
from map import read_map
from math import lcm

map = read_map('actual_input.txt')
nodes = [node for node in map["mapping"].keys() if node.endswith('A')]

start_node = None
count = 0
repeat = 3

counts = []

# Check data for loops
for node in nodes:
  start_node = node
  for t in range(repeat):
    for step_no, direction in enumerate(cycle(map["directions"]), start=1):
      node = map["mapping"][node][direction]
      if node.endswith('Z'):
        count = step_no
        if t == 1:
          counts.append(count)
        print(f'[{start_node}][{t}] Trip from {start_node} to {node} in {step_no} steps.')
        break

multiplication = reduce(lambda a, b: a * b, counts)
print(multiplication)

counts_lcm = reduce(lcm, counts)
print(counts_lcm)
