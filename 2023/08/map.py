#!/usr/bin/env python3

import sys
import os
import getopt
import re
from itertools import cycle
from functools import reduce
from math import lcm

def read_map(filename):
  directions = None
  mapping = {}

  with open(filename) as file:
    while raw_line := file.readline():
      line = raw_line.rstrip()
      if directions == None:
        directions = re.findall(r'\w', line)
      elif not line:
        continue
      else:
        node, line_mapping = parse_map_line(line)
        mapping.update(line_mapping)

  return { "directions": directions, "mapping": mapping }

def parse_map_line(line):
  node = None
  mapping = {}
  if m := re.match(r'^(\w+) = \(([^\)]*)\)', line):
    node = m.group(1)
    if nodes := re.findall(r'\w+', m.group(2)):
      mapping[node] = { "L": nodes[0], "R": nodes[1] }
  return [node, mapping]

def follow_map(map, verbose=False):
  node = 'AAA'
  print(f'Following directions from {node}...')
  for step_no, direction in enumerate(cycle(map["directions"]), start=1):
    if node == 'ZZZ':
      print(f'Reached {node} in {step_no - 1} steps!')
      break
    if verbose:
      print(f'[{step_no}] Going {direction}.')
    node = map["mapping"][node][direction]

def calculate_steps(map, verbose=False):
  # start nodes are any that end with 'A'
  nodes = [node for node in map["mapping"].keys() if node.endswith('A')]

  # list of how many steps each node takes to finish
  node_jumps = []

  print(nodes)
  # run through the list to determine node_jumps
  for step_no, direction in enumerate(cycle(map["directions"]), start=1):
    next_nodes = []
    for node in nodes:
      next_node = map["mapping"][node][direction]
      if next_node.endswith('Z'):
        node_jumps.append(step_no)
      else:
        next_nodes.append(next_node)
    nodes = next_nodes
    if not nodes:
      break

  jump_lcm = reduce(lcm, node_jumps)
  print(f'End would be reached after {jump_lcm} steps.')

def parse_args(argv):
  filename = None
  verbose = False
  ghosts = False

  opts, args = getopt.getopt(argv, 'hvgf:', ['filename='])
  for opt, arg in opts:
    if opt == '-h':
      print('map.py -f <inputfile> [-vg]')
      sys.exit(1)
    elif opt == '-v':
      verbose = True
    elif opt == '-g':
      ghosts = True
    elif opt in ('-f', '--filename'):
      filename = arg

  if not filename:
    print('-f/--filename is a required argument.')
    sys.exit(2)
  if not os.path.isfile(filename):
    print(f'Filename {filename} is not a file!')
    sys.exit(2)

  return { "filename": filename, "verbose": verbose, "ghosts": ghosts }

def main(argv):
  args = parse_args(argv)
  map = read_map(args.pop("filename"))
  if args.pop("ghosts"):
    calculate_steps(map, **args)
  else:
    follow_map(map, **args)

if __name__ == '__main__':
  main(sys.argv[1:])