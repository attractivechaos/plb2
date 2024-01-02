import std.stdio;

int nq_solve(int n) // inspired the 2nd C implementation from Rossetta Code
{
	const int NQ_MAX = 31;
	int[NQ_MAX+1] a;
	uint[NQ_MAX+1] l, c, r;
	uint m = 0, y0 = (1U<<n) - 1;
	for (int k = 0; k < n; ++k) a[k] = -1, l[k] = c[k] = r[k] = 0;
	for (int k = 0; k >= 0;) {
		auto y = (l[k] | c[k] | r[k]) & y0;
		if ((y ^ y0) >> (a[k] + 1)) {
			int i = a[k] + 1;
			for (; i < n; ++i)
				if ((y & 1<<i) == 0) break;
			if (k < n - 1) {
				a[k++] = i, y = 1<<i;
				l[k] = (l[k-1]|y)<<1, c[k] = c[k-1]|y, r[k] = (r[k-1]|y)>>1;
			} else ++m, --k;
		} else a[k--] = -1;
	}
	return m;
}

void main()
{
	int n = 15;
	writeln(nq_solve(n));
}
