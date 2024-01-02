fn nq_solve(n: Int) -> Int:
	var a = StaticTuple[32, Int](-1)
	var l = StaticTuple[32, Int](0)
	var c = StaticTuple[32, Int](0)
	var r = StaticTuple[32, Int](0)
	for i in range(0, n):
		a[i] = -1
		l[i], c[i], r[i] = 0, 0, 0
	var m = 0
	let y0 = (1<<n) - 1
	var k = 0
	while k >= 0:
		let y = (l[k] | c[k] | r[k]) & y0;
		if (y ^ y0) >> (a[k] + 1):
			var i = a[k] + 1
			while i < n:
				if (y & 1<<i) == 0: break
				i += 1
			if k < n - 1:
				let z = 1<<i
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
	print(nq_solve(15))
