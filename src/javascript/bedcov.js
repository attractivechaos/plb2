#!/usr/bin/env -S k8 --single-threaded

function iit_sort_copy(a) {
	a.sort((x, y) => (x.st - y.st));
	const b = [];
	for (let i = 0; i < a.length; ++i)
		b.push({ st: a[i].st, en: a[i].en, max: 0, data: a[i].data });
	return b;
}

function iit_index(a) {
	if (a.length == 0) return -1;
	let last, last_i, k;
	for (let i = 0; i < a.length; i += 2) last = a[i].max = a[i].en, last_i = i;
	for (k = 1; 1<<k <= a.length; ++k) {
		const i0 = (1<<k) - 1, step = 1<<(k+1), x = 1<<(k-1);
		for (let i = i0; i < a.length; i += step) {
			a[i].max = a[i].en;
			if (a[i].max < a[i-x].max) a[i].max = a[i-x].max;
			const e = i + x < a.length? a[i+x].max : last;
			if (a[i].max < e) a[i].max = e;
		}
		last_i = last_i>>k&1? last_i - x : last_i + x;
		if (last_i < a.length) last = last > a[last_i].max? last : a[last_i].max;
	}
	return k - 1;
}

function iit_overlap(a, st, en) {
	let h = 0;
	const stack = [], b = [];
	for (h = 0; 1<<h <= a.length; ++h);
	--h;
	stack.push([(1<<h) - 1, h, 0]);
	while (stack.length) {
		const t = stack.pop();
		const x = t[0], h = t[1], w = t[2];
		if (h <= 3) {
			const i0 = x >> h << h;
			let i1 = i0 + (1<<(h+1)) - 1;
			if (i1 >= a.length) i1 = a.length;
			for (let i = i0; i < i1 && a[i].st < en; ++i)
				if (st < a[i].en) b.push(a[i]);
		} else if (w == 0) { // if left child not processed
			stack.push([x, h, 1]);
			const y = x - (1<<(h-1));
			if (y >= a.length || a[y].max > st)
				stack.push([y, h - 1, 0]);
		} else if (x < a.length && a[x].st < en) {
			if (st < a[x].en) b.push(a[x]);
			stack.push([x + (1<<(h-1)), h - 1, 0]);
		}
	}
	return b;
}

function splitmix32(a) { // https://github.com/bryc/code/blob/master/jshash/PRNGs.md
	return function() {
		a |= 0; a = a + 0x9e3779b9 | 0;
		let t = a ^ a >>> 16;
		t = Math.imul(t, 0x21f0aaad);
		t = t ^ t >>> 15;
		t = Math.imul(t, 0x735a2d97);
		return (t = t ^ t >>> 15) >>> 0;
	}
}

function gen_intv(n, rng, bit_st, bit_len) {
	const mask_st = (1<<bit_st) - 1;
	const mask_len = (1<<bit_len) - 1;
	const a = [];
	for (let i = 0; i < n; ++i) {
		const st = rng() & mask_st;
		const len = rng() & mask_len;
		a.push({ st: st, en: st + len, max: 0, data: i });
	}
	return a;
}

const ccc = {
	print: typeof print == "function"? print : console.log,
	argv: typeof k8_version == "function"? arguments.slice(0) // k8
		: typeof scriptArgs == "object"? scriptArgs.slice(1)  // quickjs
		: typeof Deno == "object"? Deno.args.slice(0) // Deno
		: typeof Bun == "function"? Bun.argv.slice(2) // Bun
		: process.argv.splice(2) // Node
};

function main(args)
{
	const bit_st = 28, bit_len = 14, seed = 11;
	let n = 1000000;
	if (args.length >= 1) n = parseInt(args[0]);
	const rng = splitmix32(seed)
	const a1 = iit_sort_copy(gen_intv(n, rng, bit_st, bit_len));
	const a2 = gen_intv(n, rng, bit_st, bit_len);
	iit_index(a1);
	let tot_cov = 0;
	for (let j = 0; j < a2.length; ++j) {
		const st0 = a2[j].st, en0 = a2[j].en;
		const a = iit_overlap(a1, st0, en0);
		let cov_st = 0, cov_en = 0, cov = 0;
		for (let i = 0; i < a.length; ++i) {
			const st1 = a[i].st > st0? a[i].st : st0;
			const en1 = a[i].en < en0? a[i].en : en0;
			if (st1 > cov_en) {
				cov += cov_en - cov_st;
				cov_st = st1, cov_en = en1;
			} else cov_en = cov_en > en1? cov_en : en1;
		}
		cov += cov_en - cov_st;
		tot_cov += cov;
	}
	ccc.print(tot_cov);
}

main(ccc.argv);
