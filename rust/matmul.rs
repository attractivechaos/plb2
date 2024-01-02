fn matgen(n: usize) -> Vec<Vec<f64>> {
	let mut a = vec![vec![0f64; n]; n];
	let tmp = 1.0 / (n as f64) / (n as f64);
	for i in 0..n {
		for j in 0..n {
			let ii = i as i64;
			let jj = j as i64;
			a[i][j] = tmp * ((ii - jj) as f64) * ((ii + jj) as f64);
		}
	}
	return a;
}

fn matmul(n: usize, a: Vec<Vec<f64>>, b: Vec<Vec<f64>>) -> Vec<Vec<f64>> {
	let mut c = vec![vec![0f64; n]; n];
	for i in 0..n {
		for k in 0..n {
			let aik = a[i][k]; // hoisting aik helps performance, but hoisting b[k] doesn't
			for j in 0..n {
				c[i][j] += aik * b[k][j];
			}
		}
	}
	return c;
}

fn main() {
	let n = 1500;
	let a = matgen(n);
	let b = matgen(n);
	let c = matmul(n, a, b);
	println!("{}", c[n>>1][n>>1]);
}
