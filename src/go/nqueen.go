package main

import (
	"fmt"
)

func nq_solve(n int) int {
	const NQ_MAX = 32
	var a [NQ_MAX]int
	var l [NQ_MAX]int
	var c [NQ_MAX]int
	var r [NQ_MAX]int
	for i := 0; i < n; i++ {
		a[i] = -1
	}
	m := 0
	k := 0
	y0 := (1<<n) - 1
	for k >= 0 {
		y := (l[k] | c[k] | r[k]) & y0
		if (y ^ y0) >> (a[k] + 1) != 0 {
			i := a[k] + 1
			for i < n && (y & (1<<i)) != 0 {
				i++
			}
			if k < n - 1 {
				z := 1<<i
				a[k] = i
				k += 1
				l[k] = (l[k-1]|z)<<1
				c[k] = c[k-1]|z
				r[k] = (r[k-1]|z)>>1
			} else {
				m++
				k--
			}
		} else {
			a[k] = -1
			k--
		}
	}
	return m
}

func main() {
	n := 15
	fmt.Printf("%d\n", nq_solve(n))
}
