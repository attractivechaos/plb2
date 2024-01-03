@[direct_array_access]
fn matgen(n int) [][]f64 {
	mut a := [][]f64{len: n, init: []f64{len: n}}
	tmp := 1.0 / f64(n) / f64(n)
	for i in 0 .. n {
		for j in 0 .. n {
			a[i][j] = tmp * (i - j) * (i + j)
		}
	}
	return a
}

@[direct_array_access]
fn matmul(a [][]f64, b [][]f64) [][]f64 {
	n := a.len
	mut c := [][]f64{len: n, init: []f64{len: n}}
	for i in 0 .. a.len {
		for k in 0 .. b.len {
			t := a[i][k]
			l := b[0].len
			for j in 0 .. l {
				c[i][j] += t * b[k][j]
			}
		}
	}
	return c
}

fn main() {
	n := 1500;
	a := matgen(n)
	b := matgen(n)
	c := matmul(a, b)
	println(c[n>>1][n>>1])
}
