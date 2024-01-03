import std.stdio;

double[][] matgen(in int n) {
	double tmp = 1.0 / n / n;
	auto a = new double[][](n, n);
	for (int i = 0; i < n; ++i)
		for (int j = 0; j < n; ++j)
			a[i][j] = tmp * (i - j) * (i + j);
	return a;
}

double[][] matmul(int n, in double[][] a, in double[][] b) {
	auto c = new double[][](n, n);
	for (int i = 0; i < n; ++i)
		for (int j = 0; j < n; ++j)
			c[i][j] = 0.0;
	for (int i = 0; i < n; ++i) {
		auto ci = c[i];
		for (int k = 0; k < n; ++k) {
			auto aik = a[i][k]; // faster this way
			auto bk = b[k];
			for (int j = 0; j < n; ++j)
				ci[j] += aik * bk[j];
		}
	}
	return c;
}

void main(in string[] args) {
	int n = 1500;
	auto a = matgen(n);
	auto b = matgen(n);
	auto c = matmul(n, a, b);
	writeln(c[n / 2][n / 2]);
}
