#!/usr/bin/env python

import sys

def matmul(a, b):
	n, m, p = len(a), len(b[0]), len(a[0])
	c = []
	for i in range(n):
		ci = [0.0] * m
		for k in range(p):
			aik, bk = a[i][k], b[k]
			for j in range(m):
				ci[j] += aik * bk[j]
		c.append(ci)
	return c

def main():
	n = 1500
	if (len(sys.argv) > 1): n = int(sys.argv[1])
	tmp = 1. / n / n
	a = [[tmp * (i - j) * (i + j) for j in range(n)] for i in range(n)]
	b = [[tmp * (i - j) * (i + j) for j in range(n)] for i in range(n)]
	d = matmul(a, b)
	print(d[n//2][n//2])

if __name__ == '__main__': main()
