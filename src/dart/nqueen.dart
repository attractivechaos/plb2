int nq_solve(int n) {
	var a = List<int>.filled(n, -1);
	var l = List<int>.filled(n, 0);
	var c = List<int>.filled(n, 0);
	var r = List<int>.filled(n, 0);
	var m = 0;
	final y0 = (1<<n) - 1;
	for (var k = 0; k >= 0;) {
		final y = (l[k] | c[k] | r[k]) & y0;
		if ((y ^ y0) >> (a[k] + 1) != 0) {
			var i;
			for (i = a[k] + 1; i < n; ++i)
				if ((y & 1<<i) == 0) break;
			if (k < n - 1) {
				final z = 1<<i;
				a[k++] = i;
				l[k] = (l[k-1]|z)<<1;
				c[k] =  c[k-1]|z;
				r[k] = (r[k-1]|z)>>1;
			} else {
				++m;
				--k;
			}
		} else a[k--] = -1;
	}
	return m;
}

main() {
	var n = 15;
	print(nq_solve(n));
}
