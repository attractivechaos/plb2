func splitmix32(_ x: inout UInt32) -> UInt32 {
	x = x &+ 0x9e3779b9;
	var z = x;
	z = (z ^ (z >> 16)) &* 0x21f0aaad;
	z = (z ^ (z >> 15)) &* 0x735a2d97;
	return z ^ (z >> 15);
}

struct Interval {
	var st: Int64;
	var en: Int64;
	var max: Int64;
	var data: Int64;
	init(_ s: Int64, _ e: Int64, _ d: Int64) {
		st = s;
		en = e;
		max = 0;
		data = d;
	}
}

func gen_intv(_ n: Int, _ x: inout UInt32, _ bit_st: Int, _ bit_len: Int) -> [Interval] {
	let mask_st = (Int64(1)<<bit_st) - 1;
	let mask_len = (Int64(1)<<bit_len) - 1;
	var a: [Interval] = Array(repeating: Interval(0, 0, 0), count: n);
	for i in 0 ..< n {
		let st = Int64(splitmix32(&x)) & mask_st;
		let en = st + (Int64(splitmix32(&x)) & mask_len);
		a[i] = Interval(st, en, Int64(i));
	}
	return a;
}

func iit_index(_ a: inout [Interval]) -> Int {
	var last_i = -1;
	var last: Int64 = -1;
	for i in stride(from: 0, to: n, by: 2) {
		last_i = i;
		last = a[i].en;
		a[i].max = last;
	}
	var k = 1;
	while 1<<(k-1) <= n {
		let x = 1<<(k-1);
		let i0 = (x<<1) - 1;
		let step = x<<2;
		for i in stride(from: i0, to: n, by: step) {
			let el = a[i - x].max;
			let er = i + x < n ? a[i + x].max : last;
			var e = a[i].en;
			e = e > el ? e : el;
			e = e > er ? e : er;
			a[i].max = e;
		}
		last_i = last_i>>k&1 != 0 ? last_i - x : last_i + x;
		if last_i < n && a[last_i].max > last {
			last = a[last_i].max;
		}
		k += 1;
	}
	return k - 1;
}

struct StackCell {
	var x: Int;
	var k: Int;
	var w: Int;
	init(_ xx: Int, _ kk: Int, _ ww: Int) {
		x = xx;
		k = kk;
		w = ww;
	}
}

func iit_overlap(_ a: [Interval], _ max_level: Int, _ st: Int64, _ en: Int64, _ b: inout [Interval]) {
	b.removeAll(keepingCapacity: true);
	var stack = Array(repeating: StackCell(0, 0, 0), count: 64);
	stack[0] = StackCell((1<<max_level) - 1, max_level, 0);
	var t = 1;
	while t > 0 {
		t -= 1;
		let z = stack[t];
		if z.k <= 3 {
			let i0 = (z.x >> z.k) << z.k;
			var i1 = i0 + (1<<(z.k+1)) - 1;
			if i1 >= n {
				i1 = n;
			}
			var i = i0;
			while i < i1 && a[i].st < en {
				if st < a[i].en {
					b.append(a[i]);
				}
				i += 1;
			}
		} else if z.w == 0 {
			let y = z.x - (1<<(z.k-1));
			stack[t] = StackCell(z.x, z.k, 1);
			t += 1;
			if y >= n || a[y].max > st {
				stack[t] = StackCell(y, z.k - 1, 0);
				t += 1;
			}
		} else if z.x < n && a[z.x].st < en {
			if st < a[z.x].en {
				b.append(a[z.x]);
			}
			stack[t] = StackCell(z.x + (1<<(z.k-1)), z.k - 1, 0);
			t += 1;
		}
	}
}

var x: UInt32 = 11;
var n = 1000000;
let bit_st = 28;
let bit_len = 14
var a1 = gen_intv(n, &x, bit_st, bit_len).sorted { $0.st < $1.st };
let a2 = gen_intv(n, &x, bit_st, bit_len);
let max_level = iit_index(&a1);
var tot_cov: Int64 = 0;
var b = [Interval]();
for j in 0 ..< a2.count {
	let st0 = a2[j].st;
	let en0 = a2[j].en;
	var cov: Int64 = 0;
	iit_overlap(a1, max_level, st0, en0, &b);
	if b.count == 0 {
		continue;
	}
	var cov_st = b[0].st > st0 ? b[0].st : st0;
	var cov_en = b[0].en < en0 ? b[0].en : en0;
	for i in 1 ..< b.count {
		let st1 = b[i].st > st0 ? b[i].st : st0;
		let en1 = b[i].en < en0 ? b[i].en : en0;
		if st1 > cov_en {
			cov += cov_en - cov_st;
			cov_st = st1;
			cov_en = en1;
		} else {
			cov_en = cov_en > en1 ? cov_en : en1;
		}
	}
	cov += cov_en - cov_st;
	tot_cov += cov;
}
print(tot_cov);
