#!/usr/bin/env python3

import sys

# Recursive calculation of diffs between terms
def next_point(points):
  # reached all zeroes
  if all(p == 0 for p in points):
    return 0

  # difference between each two points, resulting in n-1 points
  diffs = [points[i + 1] - points[i] for i in range(len(points) - 1)]

  # last point plus the next point of the diffs below
  return points[-1] + next_point(diffs)

# Alternative - use polynomial interpolation to determine next term
def lagrange_interpolate(points):
  n = len(points)
  x = list(range(0, n))

  # next point
  xp = n

  yp = 0
  for i in range(n):
    p = 1
    for j in range(n):
      if i != j:
        p = p * (xp - x[j]) / (x[i] - x[j])
    yp += points[i] * p

  return round(yp)

def main(argv):
  filename = argv[0]
  histories = [
    [int(point) for point in line.strip().split()]
      for line in open(filename).readlines()
  ]

  print(sum(next_point(history) for history in histories))
  print(sum(next_point(history[::-1]) for history in histories))

  print(sum(lagrange_interpolate(history) for history in histories))
  print(sum(lagrange_interpolate(history[::-1]) for history in histories))

if __name__ == '__main__':
  main(sys.argv[1:])
