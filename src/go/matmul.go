package main

import (
	"fmt"
)

type SqMatrix struct {
	values []float64
	dim    int
}

func New(dim int) *SqMatrix {
	return &SqMatrix{
		values: make([]float64, dim*dim),
		dim:    dim,
	}
}

func matgen(n int) *SqMatrix {
	a := New(n)
	tmp := float64(1.0) / float64(n) / float64(n)
	aIdx := 0
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			a.values[aIdx] = tmp * float64(i-j) * float64(i+j)
			aIdx++
		}
	}
	return a
}

func matmul(n int, a, b *SqMatrix) *SqMatrix {
	c := New(n)
	for i := 0; i < n; i++ {
		aRow := a.values[i*n : (i+1)*n]
		cRow := c.values[i*n : (i+1)*n]
		for k, aValue := range aRow {
			bRow := b.values[k*n : (k+1)*n]
			for j := 0; j < n; j++ {
				cRow[j] += aValue * bRow[j]
			}
		}
	}
	return c
}

func main() {
	n := 1500
	a := matgen(n)
	b := matgen(n)
	c := matmul(n, a, b)
	fmt.Println(c.values[n>>1*n+n>>1])
}
