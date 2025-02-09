#!/usr/bin/env python3

def iit_index(a):
	i, last_i, k = 0, 0, 1
	for i in range(0, len(a), 2):
		a[i][2] = a[i][1]
		last, last_i = a[i][2], i
	while 1<<k < len(a):
		i0, step, x = (1<<k) - 1, 1<<(k+1), 1<<(k-1);
		for i in range(i0, len(a), step):
			end_left = a[i - x][2];
			end_right = a[i + x][2] if i + x < len(a) else last
			end = a[i][1]
			a[i][2] = max(end, end_left, end_right)
		last_i = last_i - x if (last_i>>k&1) == 1 else last_i + x
		if last_i < len(a):
			if a[last_i][2] > last:
				last = a[last_i][2]
		k += 1
	return k - 1

def iit_overlap(a, max_level, start, end):
	stack, b = [], []
	stack.append((max_level, (1<<max_level) - 1, 0))
	while len(stack):
		(k, x, w) = stack.pop()
		if k <= 3:
			i0 = x >> k << k
			i1 = i0 + (1<<(k+1)) - 1
			if i1 >= len(a): i1 = len(a)
			i = i0
			while i < i1 and a[i][0] < end:
				if start < a[i][1]:
					b.append(a[i])
				i += 1
		elif w == 0:
			stack.append((k, x, 1))
			y = x - (1<<(k-1))
			if y >= len(a) or a[y][2] > start:
				stack.append((k - 1, y, 0))
		elif x < len(a) and a[x][0] < end:
			if start < a[x][1]: b.append(a[x])
			stack.append((k - 1, x + (1<<(k-1)), 0))
	return b

def splitmix32(a):
	def next():
		nonlocal a
		a = (a + 0x9e3779b9) & 0xffffffff;
		t = a ^ a >> 16
		t = t * 0x21f0aaad & 0xffffffff;
		t = t ^ t >> 15
		t = t * 0x735a2d97 & 0xffffffff;
		return t ^ t >> 15
	return next

def gen_intv(n, rng, bit_st, bit_len):
	mask_st, mask_len = (1<<bit_st) - 1, (1<<bit_len) - 1
	a = []
	for i in range(n):
		st = rng() & mask_st
		l = rng() & mask_len
		a.append([st, st + l, 0])
	return a

import sys
if __name__ == "__main__":
	bit_st, bit_len, seed, n = 28, 14, 11, 1000000
	if len(sys.argv) >= 2: n = int(sys.argv[1])
	rng = splitmix32(seed)
	a1_raw = sorted(gen_intv(n, rng, bit_st, bit_len), key=lambda t:t[0])
	a1 = []
	for i in range(len(a1_raw)):
		a1.append(a1_raw[i].copy())
	a2 = gen_intv(n, rng, bit_st, bit_len)
	max_level = iit_index(a1)
	tot_cov = 0
	for i in range(len(a2)):
		cov, cov_st, cov_en = 0, 0, 0
		st1, en1 = a2[i][0], a2[i][1]
		b = iit_overlap(a1, max_level, st1, en1)
		for item in b:
			st0, en0 = item[0], item[1]
			if (st0 < st1): st0 = st1
			if (en0 > en1): en0 = en1
			if (st0 > cov_en):
				cov += cov_en - cov_st
				cov_st, cov_en = st0, en0
			elif cov_en < en0: cov_en = en0
		cov += cov_en - cov_st
		tot_cov += cov
	print(tot_cov)
