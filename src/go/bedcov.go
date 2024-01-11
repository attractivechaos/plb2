package main

import (
	"fmt"
	"sort"
)

type SplitMix32 struct {
	state uint32
}

func splitmix32(rng *SplitMix32) uint32 {
	(*rng).state += 0x9e3779b9
	z := (*rng).state
	z = (z ^ (z >> 16)) * 0x21f0aaad
	z = (z ^ (z >> 15)) * 0x735a2d97
	return z ^ (z >> 15)
}

type SType int64
type DType int64

type Interval struct {
	st SType
	en SType
	max SType
	data DType
}

func gen_intv(n int, rng *SplitMix32, bit_st int, bit_len int) []Interval {
	a := make([]Interval, n)
	mask_st := (SType(1) << bit_st) - 1
	mask_len := (SType(1) << bit_len) - 1
	for i := 0; i < n; i++ {
		st := SType(splitmix32(rng)) & mask_st
		en := st + (SType(splitmix32(rng)) & mask_len)
		a[i] = Interval{ SType(st), SType(en), 0, DType(i) }
	}
	return a
}

func iit_index(a []Interval) int {
	sort.SliceStable(a, func(i, j int) bool {
		return a[i].st < a[j].st
	})
	n := len(a)
	if n == 0 {
		return -1
	}
	last_i := 0
	last := SType(0)
	for i := 0; i < n; i += 2 {
		last_i = i
		a[i].max = a[i].en
		last = a[i].max
	}
	k := 1
	for 1 << k <= n {
		x := 1 << (k - 1)
		i0 := (x << 1) - 1
		step := x << 2
		for i := i0; i < n; i += step {
			el := a[i - x].max
			er := last
			if i + x < n {
				er = a[i + x].max
			}
			e := a[i].en
			if e < el {
				e = el
			}
			if e < er {
				e = er
			}
			a[i].max = e
		}
		last_i = last_i + x
		if last_i >> k & 1 != 0 {
			last_i = last_i - x
		}
		if last_i < n && a[last_i].max > last {
			last = a[last_i].max
		}
		k += 1
	}
	return k - 1
}

type StackCell struct {
	x int
	k int8
	w int8
}

func iit_overlap(a []Interval, max_level int, st SType, en SType, b []Interval) []Interval {
	n := len(a)
	b = b[:0]
	stack := make([]StackCell, 64);
	stack[0] = StackCell{ (1 << max_level) - 1, int8(max_level), 0 };
	t := 1
	for t > 0 {
		t -= 1
		z := stack[t]
		if z.k <= 3 {
			i0 := z.x >> z.k << z.k
			i1 := i0 + ((1 << (z.k + 1)) - 1)
			if i1 >= n {
				i1 = n
			}
			i := i0
			for i < i1 && a[i].st < en {
				if st < a[i].en {
					b = append(b, a[i])
				}
				i += 1
			}
		} else if z.w == 0 {
			y := z.x - 1 << (z.k - 1)
			stack[t] = StackCell{ z.x, z.k, 1 }
			t += 1
			if y >= n || a[y].max > st {
				stack[t] = StackCell{ y, z.k - 1, 0 }
				t += 1
			}
		} else if z.x < n && a[z.x].st < en {
			if st < a[z.x].en {
				b = append(b, a[z.x])
			}
			stack[t] = StackCell{ z.x + (1 << (z.k - 1)), z.k - 1, 0 }
			t += 1
		}
	}
	return b
}

func main() {
	n := 1000000
	bit_st := 28
	bit_len := 14
	rng := new(SplitMix32)
	rng.state = 11
	a1 := gen_intv(n, rng, bit_st, bit_len)
	max_level := iit_index(a1)
	b := []Interval{}
	a2 := gen_intv(n, rng, bit_st, bit_len)
	tot_cov := SType(0)
	for j := 0; j < len(a2); j++ {
		st0 := a2[j].st
		en0 := a2[j].en
		b = iit_overlap(a1, max_level, st0, en0, b)
		if len(b) == 0 {
			continue
		}
		cov_st := st0
		if b[0].st > st0 {
			cov_st = b[0].st
		}
		cov_en := en0
		if b[0].en < en0 {
			cov_en = b[0].en
		}
		cov := SType(0)
		for i := 0; i < len(b); i++ {
			st1 := st0
			if b[i].st > st0 {
				st1 = b[i].st
			}
			en1 := en0
			if b[i].en < en0 {
				en1 = b[i].en
			}
			if st1 > cov_en {
				cov += cov_en - cov_st
				cov_st = st1
				cov_en = en1
			} else {
				if cov_en < en1 {
					cov_en = en1
				}
			}
		}
		cov += cov_en - cov_st
		tot_cov += cov
	}
	fmt.Println(tot_cov)
}
