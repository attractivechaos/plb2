class Interval {
	int st = 0;
	int en = 0;
	int max = 0;
	int data = 0;
	Interval(this.st, this.en, this.max, this.data);
}

List<Interval> iit_sort_copy(List<Interval> a) {
	a.sort((x, y) => (x.st - y.st));
	List<Interval> b = [];
	for (var i = 0; i < a.length; ++i)
		b.add(Interval(a[i].st, a[i].en, a[i].max, a[i].data));
	return b;
}

int iit_index(List<Interval> a) {
	if (a.length == 0) return -1;
	int last = 0, last_i = 0, k;
	for (int i = 0; i < a.length; i += 2) {
		last = a[i].max = a[i].en;
		last_i = i;
	}
	for (k = 1; 1<<k <= a.length; ++k) {
		final int i0 = (1<<k) - 1, step = 1<<(k+1), x = 1<<(k-1);
		for (int i = i0; i < a.length; i += step) {
			a[i].max = a[i].en;
			if (a[i].max < a[i-x].max) a[i].max = a[i-x].max;
			final int e = i + x < a.length? a[i+x].max : last;
			if (a[i].max < e) a[i].max = e;
		}
		last_i = (last_i>>k&1) != 0? last_i - x : last_i + x;
		if (last_i < a.length) last = last > a[last_i].max? last : a[last_i].max;
	}
	return k - 1;
}

class StackCell {
	int x = 0;
	int k = 0;
	int w = 0;
	StackCell(this.x, this.k, this.w);
}

List<Interval> iit_overlap(List<Interval> a, int st, int en) {
	int h = 0;
	List<Interval> b = [];
	List<StackCell> stack = [];
	for (h = 0; 1<<h <= a.length; ++h);
	--h;
	stack.add(StackCell((1<<h) - 1, h, 0));
	while (stack.length > 0) {
		final t = stack.removeLast();
		final x = t.x, h = t.k, w = t.w;
		if (h <= 3) {
			final i0 = x >> h << h;
			int i1 = i0 + (1<<(h+1)) - 1;
			if (i1 >= a.length) i1 = a.length;
			for (var i = i0; i < i1 && a[i].st < en; ++i)
				if (st < a[i].en) b.add(a[i]);
		} else if (w == 0) { // if left child not processed
			stack.add(StackCell(x, h, 1));
			int y = x - (1<<(h-1));
			if (y >= a.length || a[y].max > st)
				stack.add(StackCell(y, h - 1, 0));
		} else if (x < a.length && a[x].st < en) {
			if (st < a[x].en) b.add(a[x]);
			stack.add(StackCell(x + (1<<(h-1)), h - 1, 0));
		}
	}
	return b;
}

splitmix32(int a) {
	return () {
		a |= 0; a = (a + 0x9e3779b9) & 0xffffffff;
		int t = a ^ a >>> 16;
		t = (t * 0x21f0aaad) & 0xffffffff;
		t ^= t >>> 15;
		t = (t * 0x735a2d97) & 0xffffffff;
		t ^= t >>> 15;
		return t & 0xffffffff;
	};
}

List<Interval> gen_intv(int n, rng, int bit_st, int bit_len) {
	final mask_st = (1<<bit_st) - 1;
	final mask_len = (1<<bit_len) - 1;
	List<Interval> a = [];
	for (var i = 0; i < n; ++i) {
		final st = rng() & mask_st;
		final len = rng() & mask_len;
		a.add(Interval(st, st + len, 0, i));
	}
	return a;
}

main() {
	int n = 1000000, bit_st = 28, bit_len = 14;
	var rng = splitmix32(11);
	var a1 = iit_sort_copy(gen_intv(n, rng, bit_st, bit_len));
	final a2 = gen_intv(n, rng, bit_st, bit_len);
	iit_index(a1);
	int tot_cov = 0;
	for (var j = 0; j < a2.length; ++j) {
		final st0 = a2[j].st, en0 = a2[j].en;
		final a = iit_overlap(a1, st0, en0);
		int cov_st = 0, cov_en = 0, cov = 0;
		for (var i = 0; i < a.length; ++i) {
			final st1 = a[i].st > st0? a[i].st : st0;
			final en1 = a[i].en < en0? a[i].en : en0;
			if (st1 > cov_en) {
				cov += cov_en - cov_st;
				cov_st = st1;
				cov_en = en1;
			} else cov_en = cov_en > en1? cov_en : en1;
		}
		cov += cov_en - cov_st;
		tot_cov += cov;
	}
	print(tot_cov);
}
