class matmul {
	public double[][] matgen(int n) {
		double[][] a = new double[n][n];
		double tmp = 1. / n / n;
		for (int i = 0; i < n; ++i)
			for (int j = 0; j < n; ++j)
				a[i][j] = tmp * (i - j) * (i + j);
		return a;
	}
	public double[][] matmul(double[][] a, double[][] b) {
		int m = a.length, n = a[0].length, p = b[0].length;
		double[][] c = new double[m][p];
		for (int i = 0; i < m; ++i) {
			double[] ci = c[i]; // much faster by hoisting this there
			for (int k = 0; k < n; ++k) {
				double aik = a[i][k];
				double[] bk = b[k];
				for (int j = 0; j < p; ++j)
					ci[j] += aik * bk[j];
			}
		}
		return c;
	}
	public static void main(String[] args) {
		int n = 1000;
		if (args.length >= 1) n = Integer.parseInt(args[0]);
		n = n / 2 * 2;
		matmul m = new matmul();
		double[][] a, b, c;
		a = m.matgen(n);
		b = m.matgen(n);
		c = m.matmul(a, b);
		System.out.println(c[n/2][n/2]);
	}
}
