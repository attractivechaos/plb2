fn splitmix32(mut x &u32) u32 {
	x += 0x9e3779b9
	mut z := x
	z = (z ^ (z >> 16)) * 0x21f0aaad
	z = (z ^ (z >> 15)) * 0x735a2d97
	return z ^ (z >> 15)
}

type SType = i64
type DType = i64

struct Interval {
mut:
	st   SType
	en   SType
	max  SType
	data DType
}

fn gen_intv(n i64, mut x &u32, bit_st u8, bit_len u8) []Interval {
	mut a := []Interval{len: int(n), init: Interval{
		st: 0
		en: 0
		max: 0
		data: 0
	}}
	mask_st := i64((1 << bit_st) - 1)
	mask_len := i64((1 << bit_len) - 1)
	for i in 0 .. n {
		st := splitmix32(mut x) & mask_st
		en := st + (splitmix32(mut x) & mask_len)
		a[i] = Interval{
			st: st
			en: en
			max: 0
			data: i
		}
	}
	return a
}

@[direct_array_access]
fn iit_index(mut a []Interval) i8 {
	n := a.len
	if n == 0 {
		return -1
	}
	mut last_i := 0 // FIXME: ideally I would like to use u64 for array indices
	mut last := SType(0)
	for i := 0; i < n; i += 2 {
		last_i = i
		a[i].max = a[i].en
		last = a[i].max
	}
	mut k := i8(1)
	for u64(1) << k <= n {
		x := 1 << (k - 1)
		i0 := (x << 1) - 1
		step := x << 2
		for i := i0; i < n; i += step {
			el := a[i - x].max
			er := if i + x < n { a[i + x].max } else { last }
			mut e := a[i].en
			if e < el {
				e = el
			}
			if e < er {
				e = er
			}
			a[i].max = e
		}
		last_i = if last_i >> k & 1 != 0 { last_i - x } else { last_i + x }
		if last_i < n && a[last_i].max > last {
			last = a[last_i].max
		}
		k += 1
	}
	return k - 1
}

struct StackCell {
mut:
	x int
	k i8
	w u8
}

@[direct_array_access]
fn iit_overlap(a []Interval, max_level i8, st SType, en SType, mut b []Interval) {
	n := a.len
	b.clear()
	mut stack := []StackCell{len: 64, init: StackCell{
		x: (1 << max_level) - 1
		k: max_level
		w: 0
	}}
	mut t := 1
	for t > 0 {
		t -= 1
		z := stack[t]
		if z.k <= 3 {
			i0 := z.x >> z.k << z.k
			mut i1 := i0 + ((1 << (z.k + 1)) - 1)
			if i1 >= n {
				i1 = n
			}
			mut i := i0
			for i < i1 && a[i].st < en {
				if st < a[i].en {
					b.insert(b.len, a[i])
				}
				i += 1
			}
		} else if z.w == 0 {
			y := z.x - 1 << (z.k - 1)
			stack[t] = StackCell{
				x: z.x
				k: z.k
				w: 1
			}
			t += 1
			if y >= n || a[y].max > st {
				stack[t] = StackCell{
					x: y
					k: z.k - 1
					w: 0
				}
				t += 1
			}
		} else if z.x < n && a[z.x].st < en {
			if st < a[z.x].en {
				b.insert(b.len, a[z.x])
			}
			stack[t] = StackCell{
				x: z.x + (1 << (z.k - 1))
				k: z.k - 1
				w: 0
			}
			t += 1
		}
	}
}

fn main() {
	mut x := u32(11)
	mut n := 1_000_000
	bit_st := u8(28)
	bit_len := u8(14)
	mut a1 := gen_intv(n, mut &x, bit_st, bit_len)
	a2 := gen_intv(n, mut &x, bit_st, bit_len)
	a1.sort_with_compare(fn (x &Interval, y &Interval) i64 {
		return x.st - y.st
	})
	max_level := iit_index(mut a1)
	mut b := []Interval{cap: 100}
	mut tot_cov := SType(0)
	for y2 in a2 {
		st0 := y2.st
		en0 := y2.en
		iit_overlap(a1, max_level, st0, en0, mut b)
		if b.len == 0 {
			continue
		}
		mut cov_st := if b[0].st > st0 { b[0].st } else { st0 }
		mut cov_en := if b[0].en < en0 { b[0].en } else { en0 }
		mut cov := SType(0)
		for bi in b {
			st1 := if bi.st > st0 { bi.st } else { st0 }
			en1 := if bi.en < en0 { bi.en } else { en0 }
			if st1 > cov_en {
				cov += cov_en - cov_st
				cov_st = st1
				cov_en = en1
			} else {
				cov_en = if cov_en > en1 { cov_en } else { en1 }
			}
		}
		cov += cov_en - cov_st
		tot_cov += cov
	}
	println(tot_cov)
}
