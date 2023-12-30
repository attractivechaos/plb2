#!/usr/bin/env k8

function nq_solve(n) {
	let a = [], l = [], c = [], r = [], m = 0;
	const y0 = (1<<n) - 1;
	for (let k = 0; k < n; ++k)
		a[k] = -1, l[k] = c[k] = r[k] = 0;
	for (let k = 0; k >= 0;) {
		const y = (l[k] | c[k] | r[k]) & y0;
		if ((y ^ y0) >> (a[k] + 1)) {
			let i;
			for (i = a[k] + 1; i < n; ++i)
				if ((y & 1<<i) == 0) break;
			if (k < n - 1) {
				const z = 1<<i;
				a[k++] = i;
				l[k] = (l[k-1]|z)<<1, c[k] = c[k-1]|z, r[k] = (r[k-1]|z)>>1;
			} else ++m, --k;
		} else a[k--] = -1;
	}
	return m;
}

var ccc = {
	print: typeof print == "function"? print : console.log,
	argv: typeof k8_version == "function"? arguments.slice(0) : typeof Deno == "object"? Deno.args.slice(0) : typeof Bun == "function"? Bun.argv.slice(2) : process.argv.splice(2)
};

function main(args) {
	let n = 15;
	if (args.length > 0) n = parseInt(args[0]);
	print(nq_solve(n));
}

main(ccc.argv);
