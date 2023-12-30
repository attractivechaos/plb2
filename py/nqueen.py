#!/usr/bin/env python

import sys

def nq_solve(n):
	m = 0
	a = [-1] * n
	l = [0] * n
	c = [0] * n
	r = [0] * n
	y0 = (1<<n) - 1
	k = 0
	while k >= 0:
		y = (l[k] | c[k] | r[k]) & y0;
		if (y ^ y0) >> (a[k] + 1):
			i = a[k] + 1
			while i < n:
				if (y & 1<<i) == 0: break
				i += 1
			if k < n - 1:
				z = 1<<i
				a[k] = i
				k += 1
				l[k] = (l[k-1]|z)<<1
				c[k] = c[k-1]|z
				r[k] = (r[k-1]|z)>>1
			else:
				m += 1
				k -= 1
		else:
			a[k] = -1
			k -= 1
	return m

def main():
	n = 15
	if (len(sys.argv) > 1): n = int(sys.argv[1])
	print(nq_solve(n))

if __name__ == '__main__': main()
