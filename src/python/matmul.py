#!/usr/bin/env python

import sys, array

# Writen by Attractive Chaos; distributed under the MIT license
# Modified by brentp: using array instead of list

def matmul(a, b):
	if len(a[0]) != len(b): raise
	ra, rb, rb0 = list(range(len(a))), list(range(len(b))), list(range(len(b[0])))
	d = [array.array('d', [0 for j in rb0]) for i in ra]
	for i in ra:
		for k in rb:
			aik, di, bk = a[i][k], d[i], b[k] # faster 
			for j in rb0:
				di[j] += aik * bk[j]
	return d

def main():
	n = 1500
	if (len(sys.argv) > 1): n = int(sys.argv[1])
	tmp = 1. / n / n
	a = [[tmp * (i - j) * (i + j) for j in range(n)] for i in range(n)]
	b = [[tmp * (i - j) * (i + j) for j in range(n)] for i in range(n)]
	d = matmul(a, b)
	print(d[n//2][n//2])

if __name__ == '__main__': main()
