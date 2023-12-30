#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define NQ_MAX 31

int nq_solve(int n) // inspired the 2nd C implementation from Rossetta Code
{
	int i, k, a[NQ_MAX+1], m = 0;
	uint32_t l[NQ_MAX+1], c[NQ_MAX+1], r[NQ_MAX+1], y, y0 = (1U<<n) - 1;
	for (k = 0; k < n; ++k) a[k] = -1, l[k] = c[k] = r[k] = 0;
	for (k = 0; k >= 0;) {
		y = (l[k] | c[k] | r[k]) & y0; // bit array for possible solutions
		if ((y ^ y0) >> (a[k] + 1)) { // solution found
			for (i = a[k] + 1; i < n; ++i) // find the first solution
				if ((y & 1<<i) == 0) break;
			if (k < n - 1) {
				a[k++] = i, y = 1<<i; // keep the solution
				l[k] = (l[k-1]|y)<<1, c[k] = c[k-1]|y, r[k] = (r[k-1]|y)>>1;
			} else ++m, --k;
		} else a[k--] = -1; // no solution
	}
	return m;
}

int main(int argc, char *argv[])
{
	int n = 15, m;
	if (argc > 1) n = atoi(argv[1]);
	if (n > NQ_MAX) abort();
	m = nq_solve(n);
	printf("%d\n", m);
	return 0;
}
