fn matgen<const N: usize>() -> Box<[[f64; N]; N]> {
	let mut a = Box::new([[0f64; N]; N]);
	let tmp = 1.0 / (N as f64) / (N as f64);
	for i in 0..N {
		for j in 0..N {
			let ii = i as i64;
			let jj = j as i64;
			a[i][j] = tmp * ((ii - jj) as f64) * ((ii + jj) as f64);
		}
	}
	return a;
}

fn matmul<const N: usize>(a: &[[f64; N]; N], b: &[[f64; N]; N]) -> Box<[[f64; N]; N]> {
	let mut c = Box::new([[0f64; N]; N]);
	for i in 0..N {
		for k in 0..N {
			let aik = a[i][k]; // hoisting aik helps performance, but hoisting b[k] doesn't
			for j in 0..N {
				c[i][j] += aik * b[k][j];
			}
		}
	}
	return c;
}

fn main() {
	const N: usize = 1500;
	let a = matgen::<N>();
	let b = matgen::<N>();
	let c = matmul::<N>(&a, &b);
	println!("{}", c[N>>1][N>>1]);
}