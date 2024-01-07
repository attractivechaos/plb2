import 'dart:typed_data';

List<Float64List> matgen(int n) {
	var a = List<Float64List>.generate(n, (i)=>Float64List(n), growable: false);
	final tmp = 1.0 / n / n;
	for (var i = 0; i < n; ++i) {
		for (var j = 0; j < n; ++j) {
			a[i][j] = tmp * (i - j) * (i + j);
		}
	}
	return a;
}

List<Float64List> matmul(a, b) {
	final n = a.length, p = a[0].length, m = b[0].length;
	var c = List<Float64List>.generate(n, (i)=>Float64List(m), growable: false);
	for (var i = 0; i < n; ++i) {
		var ci = c[i];
		for (var k = 0; k < p; ++k) {
			final t = a[i][k], bk = b[k];
			for (var j = 0; j < m; ++j)
				ci[j] += t * bk[j];
		}
	}
	return c;
}

main() {
	var n = 1500;
	final a = matgen(n);
	final b = matgen(n);
	final c = matmul(a, b);
	print(c[n>>1][n>>1]);
}
