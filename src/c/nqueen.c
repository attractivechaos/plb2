#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define NQ_MAX 15

static int nq_solve(int n) // inspired the 2nd C implementation from Rossetta Code
{
	int k, a[NQ_MAX], m = 0;
	const uint32_t y0 = (1U<<n) - 1;
	uint32_t l[NQ_MAX], c[NQ_MAX], r[NQ_MAX];
	for (k = 0; k < n; ++k) a[k] = -1, l[k] = c[k] = r[k] = 0;
	for (k = 0; k >= 0;) {
		uint32_t y = (l[k] | c[k] | r[k]) & y0; // bit array for possible choices at row k
		if ((y ^ y0) >> (a[k] + 1)) { // possible to make a choice
			int i = a[k] + 1;
			while (i < n) {
				// look for the first choice
				if ((y & 1 << i) == 0) break;
				i++;
			}
			if (k < n - 1) { // store the choice
				uint32_t z = 1U<<i;
				a[k++] = i;
				l[k] = (l[k-1]|z)<<1U;
				c[k] =  c[k-1]|z;
				r[k] = (r[k-1]|z)>>1;
			} else ++m, --k; // solution found
		} else a[k--] = -1; // no choice; backtrack
	}
	return m;
}

int main()
{
	int n = 15;
	printf("%d\n", nq_solve(n));
	return 0;
}
