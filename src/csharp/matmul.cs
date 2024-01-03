class Matmul {
        public static double[][] matgen(int n) {
                double[][] a = new double[n][];
                double tmp = 1.0 / n / n;
                for (int i = 0; i < n; ++i) {
                        a[i] = new double[n];
                        for (int j = 0; j < n; ++j)
                                a[i][j] = tmp * (i - j) * (i + j);
                }
                return a;
        }
        public static double[][] matmul(double[][] a, double[][] b) {
                int m = a.Length, n = a[0].Length, p = b[0].Length;
                double[][] c = new double[m][];
		for (int i = 0; i < m; ++i) {
                        c[i] = new double[n];
                        double[] ci = c[i]; 
                        for (int k = 0; k < n; ++k) {
                                double aik = a[i][k];
                                double[] bk = b[k];
                                for (int j = 0; j < p; ++j)
                                        ci[j] += aik * bk[j];
                        }
                }
                return c;
        }
        public static void Main(string[] args) {
                int n = 1500;
                if (args.Length >= 1) n = int.Parse(args[0]);
                n = n / 2 * 2;
                double[][] a, b, x;
                a = matgen(n);
                b = matgen(n);
                x = matmul(a, b);
                Console.WriteLine(x[n/2][n/2]);
        }
}
