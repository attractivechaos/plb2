func nq_solve(n: Int) -> Int {
	var a = Array(repeating: -1, count: n)
	var l = Array(repeating: 0, count: n)
	var c = Array(repeating: 0, count: n)
	var r = Array(repeating: 0, count: n)
	var m = 0
	let y0 = (1<<n) - 1
	var k = 0
	while k >= 0 {
		let y = (l[k] | c[k] | r[k]) & y0
		if (y ^ y0) >> (a[k] + 1) != 0 {
			var i = a[k] + 1
			while i < n && (y & 1<<i) != 0 {
				i += 1
			}
			if k < n - 1 {
				let z = 1<<i
				a[k] = i
				k += 1
				l[k] = (l[k-1]|z)<<1
				c[k] =  c[k-1]|z
				r[k] = (r[k-1]|z)>>1
			}
			else {
				m += 1
				k -= 1
			}
		}
		else {
			a[k] = -1
			k -= 1
		}
	}
	return m
}

let n = 15
print(nq_solve(n:n))
