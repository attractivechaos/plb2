#!/usr/bin/env k8

function matgen(n) {
	let a = [], tmp = 1. / n / n;
	for (let i = 0; i < n; ++i) {
		a[i] = new Float64Array(n);
		for (let j = 0; j < n; ++j)
			a[i][j] = tmp * (i - j) * (i + j);
	}
	return a;
}

function matmul(a, b) {
	if (a[0].length != b.length) return;
	let c = [];
	for (let i = 0; i < a.length; ++i)
		c[i] = new Float64Array(b[0].length);
	for (let i = 0; i < a.length; ++i)
		for (let k = 0; k < b.length; ++k) {
			const t = a[i][k], l = b[0].length;
			for (let j = 0; j < l; ++j)
				c[i][j] += t * b[k][j];
		}
	return c;
}

var ccc = {
	print: typeof print == "function"? print : console.log,
	argv: typeof k8_version == "function"? arguments.slice(0) : typeof Deno == "object"? Deno.args.slice(0) : typeof Bun == "function"? Bun.argv.slice(2) : process.argv.splice(2)
};

let n = ccc.argv.length > 0? parseInt(ccc.argv[0]) : 1000;
const a = matgen(n), b = matgen(n);
const c = matmul(a, b);
ccc.print(c[n>>1][n>>1]);
