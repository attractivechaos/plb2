import java.io.*;
import java.util.*;

class Interval implements Comparable<Interval> {
	long st, en, max, data;
	public int compareTo(Interval x) {
		return st > x.st? 1 : st < x.st? -1 : 0;
	}
	Interval(long st_, long en_, long data_) {
		st = st_; en = en_; max = 0; data = data_;
	}
};

class StackCell {
	int x, k, w;
	StackCell(int x_, int k_, int w_) {
		x = x_; k = k_; w = w_;
	}
};

class bedcov {
	static long splitmix32(long[] x) {
		x[0] = (x[0] + 0x9e3779b9) & 0xffffffffl;
		long z = x[0];
		z = ((z ^ (z >> 16)) * 0x21f0aaad) & 0xffffffffl;
		z = ((z ^ (z >> 15)) * 0x735a2d97) & 0xffffffffl;
		return z ^ (z >> 15);
	}
	static Interval[] gen_intv(int n, long[] x, int bit_st, int bit_len) {
		Interval[] a = new Interval[n];
		long mask_st = (1l<<bit_st) - 1;
		long mask_len = (1l<<bit_len) - 1;
		for (int i = 0; i < n; ++i) {
			long st = splitmix32(x) & mask_st;
			long en = st + (splitmix32(x) & mask_len);
			a[i] = new Interval(st, en, i);
		}
		return a;
	}
	static int iit_index(int n, Interval[] a) {
		int last_i = -1;
		long last = -1;
		if (n == 0) return -1;
		Arrays.sort(a);
		for (int i = 0; i < n; i += 2) {
			last_i = i;
			last = a[i].max = a[i].en;
		}
		int k;
		for (k = 1; 1<<k <= n; ++k) {
			int x = 1<<(k-1), i0 = (x<<1) - 1, step = x<<2;
			for (int i = i0; i < n; i += step) {
				long el = a[i - x].max;
				long er = i + x < n? a[i + x].max : last;
				long e = a[i].en;
				e = e > el? e : el;
				e = e > er? e : er;
				a[i].max = e;
			}
			last_i = (last_i>>k&1) != 0? last_i - x : last_i + x;
			if (last_i < n && a[last_i].max > last)
				last = a[last_i].max;
		}
		return k - 1;
	}
	static void iit_overlap(int n, Interval[] a, int max_level, long st, long en, ArrayList<Interval> b) {
		b.clear();
		int t = 0;
		StackCell[] stack = new StackCell[64];
		stack[t++] = new StackCell((1<<max_level) - 1, max_level, 0);
		while (t > 0) {
			StackCell z = stack[--t];
			if (z.k <= 3) {
				int i, i0 = z.x >> z.k << z.k, i1 = i0 + (1<<(z.k+1)) - 1;
				if (i1 >= n) i1 = n;
				for (i = i0; i < i1 && a[i].st < en; ++i)
					if (st < a[i].en)
						b.add(a[i]);
			} else if (z.w == 0) {
				int y = z.x - (1<<(z.k-1));
				stack[t++] = new StackCell(z.x, z.k, 1);
				if (y >= n || a[y].max > st)
					stack[t++] = new StackCell(y, z.k - 1, 0);
			} else if (z.x < n && a[z.x].st < en) {
				if (st < a[z.x].en) b.add(a[z.x]);
				stack[t++] = new StackCell(z.x + (1<<(z.k-1)), z.k - 1, 0);
			}
		}
	}
	public static void main(String[] args) throws Exception {
		long[] x = { 11 };
		int n = 1000000, bit_st = 28, bit_len = 14;
		Interval[] a1 = gen_intv(n, x, bit_st, bit_len);
		Interval[] a2 = gen_intv(n, x, bit_st, bit_len);
		int max_level = iit_index(n, a1);
		ArrayList<Interval> b = new ArrayList<Interval>();
		long tot_cov = 0;
		for (int j = 0; j < n; ++j) {
			long st0 = a2[j].st, en0 = a2[j].en, cov_st, cov_en, cov = 0;
			int n_b, i;
			iit_overlap(n, a1, max_level, st0, en0, b);
			if (b.size() == 0) continue;
			cov_st = b.get(0).st > st0? b.get(0).st : st0;
			cov_en = b.get(0).en < en0? b.get(0).en : en0;
			for (i = 1; i < b.size(); ++i) {
				long st1 = b.get(i).st > st0? b.get(i).st : st0;
				long en1 = b.get(i).en < en0? b.get(i).en : en0;
				if (st1 > cov_en) {
					cov += cov_en - cov_st;
					cov_st = st1;
					cov_en = en1;
				} else cov_en = cov_en > en1? cov_en : en1;
			}
			cov += cov_en - cov_st;
			tot_cov += cov;
		}
		System.out.println(tot_cov);
	}
}
