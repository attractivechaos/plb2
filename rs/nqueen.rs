pub fn nq_solve(n: usize) -> usize {
	const MAX_N: usize = 32;
	let mut a: [i32; MAX_N] = [-1; MAX_N];
	let mut l: [i32; MAX_N] = [0; MAX_N];
	let mut c: [i32; MAX_N] = [0; MAX_N];
	let mut r: [i32; MAX_N] = [0; MAX_N];
	let mut m = 0;
	let y0 = (1<<n) - 1;
	let mut k = 0;
	loop {
		let y = (l[k] | c[k] | r[k]) & y0;
		if (y ^ y0) >> (a[k] + 1) != 0 {
			let mut i = a[k] + 1;
			while i < n as i32 && (y & (1<<i)) != 0 {
				i += 1;
			}
			if k < n - 1 {
				let z = 1<<i;
				a[k] = i;
				k += 1;
				l[k] = (l[k-1]|z)<<1;
				c[k] = c[k-1]|z;
				r[k] = (r[k-1]|z)>>1;
			} else {
				m += 1;
				if k == 0 {
					break;
				}
				k -= 1;
			}
		} else {
			a[k] = -1;
			if k == 0 {
				break;
			}
			k -= 1;
		}
	}
	return m;
}

fn main() {
	println!("{}", nq_solve(15));
}
