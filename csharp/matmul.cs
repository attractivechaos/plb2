class Matmul {
	public static double[,] matgen(int n) {
		double[,] a = new double[n,n];
		double tmp = 1.0 / n / n;
		for (int i = 0; i < n; ++i)
			for (int j = 0; j < n; ++j)
				a[i,j] = tmp * (i - j) * (i + j);
		return a;
	}
	public static double[,] matmul(double[,] a, double[,] b) {
		int m = a.GetLength(0), n = a.GetLength(1), p = b.GetLength(0);
		double[,] c = new double[p,n];
		for (int i = 0; i < m; ++i)
			for (int k = 0; k < n; ++k) {
				double t = a[i,k]; // this improves the performance
				for (int j = 0; j < p; ++j)
					c[i,j] += t * b[k,j];
			}
		return c;
	}
	public static void Main(String[] args) {
		int n = 1000;
		if (args.GetLength(0) >= 1) n = int.Parse(args[0]);
		n = n / 2 * 2;
		double[,] a, b, x;
		a = Matmul.matgen(n);
		b = Matmul.matgen(n);
		x = Matmul.matmul(a, b);
		Console.WriteLine(x[n/2,n/2]);
	}
}
