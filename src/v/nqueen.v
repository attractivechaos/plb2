@[direct_array_access]
fn nq_solve(n int) int {
	mut a := []int{len: n, init: -1}
	mut l := []u32{len: n, init: 0}
	mut c := []int{len: n, init: 0}
	mut r := []int{len: n, init: 0}
	mut m := 0
	y0 := (1 << n) - 1
	mut k := 0
	for k >= 0 {
		y := (l[k] | c[k] | r[k]) & y0
		if (y ^ y0) >> (a[k] + 1) != 0 {
			mut i := a[k] + 1
			for i < n {
				if (y & (1 << i)) == 0 {
					break
				}
				i += 1
			}
			if k < n - 1 {
				z := u32(1) << i
				a[k] = i
				k += 1
				l[k] = (l[k - 1] | z) << 1
				c[k] = c[k - 1] | z
				r[k] = (r[k - 1] | z) >> 1
			} else {
				m += 1
				k -= 1
			}
		} else {
			a[k] = -1
			k -= 1
		}
	}
	return m
}

fn main() {
	n := 15
	println(nq_solve(n))
}
