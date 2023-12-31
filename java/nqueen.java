import java.io.*;

class nqueen {
	static int nq_solve(int n) {
		int[] a = new int[n];
		int[] l = new int[n];
		int[] c = new int[n];
		int[] r = new int[n];
		int k, m = 0, y0 = (1<<n) - 1;
		for (k = 0; k < n; ++k) {
			a[k] = -1;
			l[k] = c[k] = r[k] = 0;
		}
		for (k = 0; k >= 0;) {
			int y = (l[k] | c[k] | r[k]) & y0;
			if ((y ^ y0) >> (a[k] + 1) != 0) {
				int i;
				for (i = a[k] + 1; i < n; ++i)
					if ((y & 1<<i) == 0) break;
				if (k < n - 1) {
					int z = 1<<i;
					a[k++] = i;
					l[k] = (l[k-1]|z)<<1;
					c[k] =  c[k-1]|z;
					r[k] = (r[k-1]|z)>>1;
				} else {
					++m;
					--k;
				}
			} else a[k--] = -1; // no solution
		}
		return m;
	}
	public static void main(String[] args) throws Exception {
		int n = 15;
		System.out.println(nqueen.nq_solve(n));
	}
}
