#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define NQ_MAX 31

int nq_solve(int n) // inspired the 2nd C implementation from Rossetta Code
{
	int k, a[NQ_MAX+1], m = 0;
	uint32_t l[NQ_MAX+1], c[NQ_MAX+1], r[NQ_MAX+1], y0 = (1U<<n) - 1;
	for (k = 0; k < n; ++k) a[k] = -1, l[k] = c[k] = r[k] = 0;
	for (k = 0; k >= 0;) {
		uint32_t y = (l[k] | c[k] | r[k]) & y0; // bit array for possible choices at row k
		if ((y ^ y0) >> (a[k] + 1)) { // possible to make a choice
			int i;
			for (i = a[k] + 1; i < n; ++i) // look for the first choice
				if ((y & 1<<i) == 0) break;
			if (k < n - 1) { // store the choice
				uint32_t z = 1<<i;
				a[k++] = i;
				l[k] = (l[k-1]|z)<<1;
				c[k] =  c[k-1]|z;
				r[k] = (r[k-1]|z)>>1;
			} else ++m, --k; // solution found
		} else a[k--] = -1; // no choice; backtrack
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
