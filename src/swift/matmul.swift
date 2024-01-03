struct Matrix {
	var storage: [Float64]
	let cols_no: Int

	init(n: Int) {
		storage = Array(repeating: 0, count: n*n)
		cols_no = n
	}

	subscript(_ index: (Int, Int)) -> Float64 {
		get {
			let (i, j) = index
			return self.storage[i * self.cols_no + j]
		}
		set(newValue) {
			let (i, j) = index
			self.storage[i * self.cols_no + j] = newValue
		}
	}
}

func matgen(n: Int) -> Matrix {
	var a = Matrix(n: n)
	let tmp = 1.0 / Float64(n) / Float64(n);
	for i in 0...n-1 {
		for j in 0...n-1 {
			a[(i, j)] = tmp * Float64(i - j) * Float64(i + j)
		}
	}
	return a
}

func matmul(n: Int, a: Matrix, b: Matrix) -> Matrix {
	var c = Matrix(n: n)
	for i in 0...n-1 {
		for k in 0...n-1 {
			for j in 0...n-1 {
				c[(i, j)] += a[(i, k)] * b[(k, j)]
			}
		}
	}
	return c
}

var n = 1500
let a = matgen(n: n)
let b = matgen(n: n)
let c = matmul(n: n, a: a, b: b)
print(c[(n>>1, n>>1)])
