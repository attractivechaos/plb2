package main

import (
	"fmt"
)

func matgen(n int) [][]float64 {
	a := make([][]float64, n);
	tmp := float64(1.0) / float64(n) / float64(n);
	for i := 0; i < n; i++ {
		a[i] = make([]float64, n);
		for j := 0; j < n; j++ {
			a[i][j] = tmp * float64(i - j) * float64(i + j);
		}
	}
	return a;
}

func matmul(n int, a [][]float64, b [][]float64) [][]float64 {
	c := make([][]float64, n);
	for i := 0; i < n; i++ {
		c[i] = make([]float64, n);
	}
	for i := 0; i < n; i++ {
		for k := 0; k < n; k++ {
			t := a[i][k];
			for j := 0; j < n; j++ {
				c[i][j] += t * b[k][j];
			}
		}
	}
	return c;
}

func main() {
	n := 1500;
	a := matgen(n);
	b := matgen(n);
	c := matmul(n, a, b);
	fmt.Println(c[n>>1][n>>1]);
}
