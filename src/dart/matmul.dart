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

List<Float64List> matmul(int n, a, b) {
	var c = List<Float64List>.generate(n, (i)=>Float64List(n), growable: false);
	for (var i = 0; i < a.length; ++i)
		for (var k = 0; k < b.length; ++k) {
			final t = a[i][k], l = b[0].length;
			for (var j = 0; j < l; ++j)
				c[i][j] += t * b[k][j];
		}
	return c;
}

main() {
	var n = 1500;
	final a = matgen(n);
	final b = matgen(n);
	final c = matmul(n, a, b);
	print(c[n>>1][n>>1]);
}
