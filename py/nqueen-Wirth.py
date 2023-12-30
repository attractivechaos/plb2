#!/usr/bin/env python

import sys

# From the sample code in wiki, retrieved on 12/30/2023.
# According to wiki, it is adapted from an algorithm by Niklaus Wirth
def queens(n, i, a, b, c):
	if i < n:
		for j in range(n):
			if j not in a and i+j not in b and i-j not in c:
				yield from queens(n, i+1, a+[j], b+[i+j], c+[i-j])
	else:
		yield a

def main():
	n = 15
	if (len(sys.argv) > 1): n = int(sys.argv[1])
	m = 0
	for solution in queens(n, 0, [], [], []):
		m += 1
	print(m)

if __name__ == '__main__': main()
