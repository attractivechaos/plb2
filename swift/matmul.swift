func matgen(n: Int) -> [[Float64]] {
	var a: [[Float64]] = Array(repeating: Array(repeating: 0, count: n), count: n)
	let tmp = 1.0 / Float64(n) / Float64(n);
	for i in 0...n-1 {
		for j in 0...n-1 {
			a[i][j] = tmp * Float64(i - j) * Float64(i + j)
		}
	}
	return a
}

func matmul(n: Int, a: [[Float64]], b: [[Float64]]) -> [[Float64]] {
	var c: [[Float64]] = Array(repeating: Array(repeating: 0, count: n), count: n)
	for i in 0...n-1 {
		for k in 0...n-1 {
			for j in 0...n-1 {
				c[i][j] += a[i][k] * b[k][j]
			}
		}
	}
	return c
}

var n = 1000
let a = matgen(n:n)
let b = matgen(n:n)
let c = matmul(n:n, a:a, b:b)
print(c[n>>1][n>>1])
