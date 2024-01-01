import std.stdio;

double[][] matgen(in int n) {
	double tmp = 1.0 / n / n;
	auto a = new double[][](n, n);
	for (int i = 0; i < n; ++i)
		for (int j = 0; j < n; ++j)
			a[i][j] = tmp * (i - j) * (i + j);
	return a;
}

double[][] matmul(in double[][] a, in double[][] b) {
	auto m = a.length, n = a[0].length, p = b[0].length;
	auto c = new double[][](m, p);
	for (int i = 0; i < m; ++i)
		for (int j = 0; j < p; ++j)
			c[i][j] = 0.0;
	for (int i = 0; i < m; ++i) {
		for (int k = 0; k < n; ++k) {
			auto aik = a[i][k]; // faster this way
			for (int j = 0; j < p; ++j)
				c[i][j] += aik * b[k][j];
		}
	}
	return c;
}

void main(in string[] args) {
	int n = 1000;
	auto a = matgen(n);
	auto b = matgen(n);
	auto c = matmul(a, b);
	writeln(c[n / 2][n / 2]);
}
