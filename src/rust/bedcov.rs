// splitmix32 RNG. Adapted from a ChatGPT4 implementation
struct SplitMix32 {
    state: u32,
}

impl SplitMix32 {
    fn new(seed: u32) -> SplitMix32 {
        SplitMix32 { state: seed }
    }

    fn next(&mut self) -> u32 {
        self.state = self.state.wrapping_add(0x9e3779b9);
        let mut z = self.state;
        z = (z ^ (z >> 16)).wrapping_mul(0x21f0aaad);
        z = (z ^ (z >> 15)).wrapping_mul(0x735a2d97);
        return z ^ (z >> 15);
    }
}

// Implicit interval tree
#[derive(Copy, Clone)]
#[allow(dead_code)]
struct Interval<SType, DType> {
	data: DType,
	st: SType,
	en: SType,
	max: SType,
}

type SType = i64;
type DType = i64;

fn iit_index(a: &mut Vec<Interval<SType, DType>>) -> i8 { // TODO: make this a generic function
	let n = a.len();
	if n == 0 {
		return -1;
	}
	let mut last_i: usize = 0;
	let mut last: SType = 0;
	for i in (0..n).step_by(2) {
		last_i = i;
		a[i].max = a[i].en;
		last = a[i].max;
	}
	let mut k = 1i8;
	while 1u64<<k <= n as u64 {
		let x = (1u64<<(k-1)) as usize;
		let i0 = ((x<<1) - 1) as usize;
		let step = (x<<2) as usize;
		for i in (i0..n).step_by(step) {
			let el = a[i - x].max;
			let er = if i + x < n { a[i + x].max } else { last };
			let mut e = a[i].en;
			if e < el { e = el }
			if e < er { e = er }
			a[i].max = e;
		}
		last_i = if last_i>>k&1 != 0 { last_i - x } else { last_i + x };
		if last_i < n && a[last_i].max > last {
			last = a[last_i].max;
		}
		k += 1;
	}
	return k - 1;
}

#[derive(Copy, Clone)]
struct StackCell {
	x: usize,
	k: i8,
	w: u8,
}

fn iit_overlap(a: &Vec<Interval<SType, DType>>, max_level: i8, st: SType, en: SType, b: &mut Vec<Interval<SType, DType>>) {
	let n = a.len();
	b.clear();
	let mut stack: [StackCell; 64] = [StackCell { x: ((1u64<<max_level) - 1) as usize, k: max_level, w: 0 }; 64];
	let mut t = 1;
	while t > 0 {
		t -= 1;
		let z = stack[t];
		if z.k <= 3 {
			let i0 = z.x >> z.k << z.k;
			let mut i1 = i0 + ((1u64<<(z.k+1)) - 1) as usize;
			if i1 >= n {
				i1 = n
			}
			let mut i = i0;
			while i < i1 && a[i].st < en {
				if st < a[i].en {
					b.push(a[i]);
				}
				i += 1;
			}
		} else if z.w == 0 {
			let y = z.x - (1u64<<(z.k-1)) as usize;
			stack[t] = StackCell { x: z.x, k: z.k, w: 1 };
			t += 1;
			if y >= n || a[y].max > st {
				stack[t] = StackCell { x: y, k: z.k - 1, w: 0 };
				t += 1;
			}
		} else if z.x < n && a[z.x].st < en {
			if st < a[z.x].en {
				b.push(a[z.x]);
			}
			stack[t] = StackCell { x: z.x + (1u64<<(z.k-1)) as usize, k: z.k - 1, w: 0 };
			t += 1;
		}
	}
}

// Generate random intervals
fn gen_intv(n: usize, rng: &mut SplitMix32, bit_st: u8, bit_len: u8) -> Vec<Interval<i64, i64>> {
	let mask_st = (1i64 << bit_st) - 1;
	let mask_len = (1i64 << bit_len) - 1;
	let mut a: Vec<Interval<i64, i64>> = Vec::with_capacity(n);
	for i in 1..n {
		let st = (rng.next() as i64) & mask_st;
		let en = st + ((rng.next() as i64) & mask_len);
		a.push(Interval { st: st, en: en, data: i as i64, max: 0 });
	}
	return a;
}

fn main() {
	let n = 1000000;
	let bit_st = 28u8;
	let bit_len = 14u8;
	let mut rng = SplitMix32::new(11);
	let mut a1 = gen_intv(n, &mut rng, bit_st, bit_len);
	let a2 = gen_intv(n, &mut rng, bit_st, bit_len);
	a1.sort_by(|a, b| a.st.partial_cmp(&b.st).unwrap());
	let max_level = iit_index(&mut a1);
	let mut b = Vec::new();
	let mut tot_cov: SType = 0;
	for a2j in a2.iter() {
		let st0 = a2j.st;
		let en0 = a2j.en;
		iit_overlap(&a1, max_level, st0, en0, &mut b);
		if b.len() == 0 {
			continue;
		}
		let mut cov_st = if b[0].st > st0 { b[0].st } else { st0 };
		let mut cov_en = if b[0].en < en0 { b[0].en } else { en0 };
		let mut cov: SType = 0;
		for bi in b.iter() {
			let st1 = if bi.st > st0 { bi.st } else { st0 };
			let en1 = if bi.en < en0 { bi.en } else { en0 };
			if st1 > cov_en {
				cov += cov_en - cov_st;
				cov_st = st1;
				cov_en = en1;
			} else {
				cov_en = if cov_en > en1 { cov_en } else { en1 };
			}
		}
		cov += cov_en - cov_st;
		tot_cov += cov;
	}
	println!("{}", tot_cov);
}
