#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

/**************
 * Radix sort *
 **************/

#define RS_MIN_SIZE 64
#define RS_MAX_BITS 8

#define KRADIX_SORT_INIT(name, rstype_t, rskey, sizeof_key) \
	typedef struct { \
		rstype_t *b, *e; \
	} rsbucket_##name##_t; \
	void rs_insertsort_##name(rstype_t *beg, rstype_t *end) \
	{ \
		rstype_t *i; \
		for (i = beg + 1; i < end; ++i) \
			if (rskey(*i) < rskey(*(i - 1))) { \
				rstype_t *j, tmp = *i; \
				for (j = i; j > beg && rskey(tmp) < rskey(*(j-1)); --j) \
					*j = *(j - 1); \
				*j = tmp; \
			} \
	} \
	void rs_sort_##name(rstype_t *beg, rstype_t *end, int n_bits, int s) \
	{ \
		rstype_t *i; \
		int size = 1<<n_bits, m = size - 1; \
		rsbucket_##name##_t *k, b[1<<RS_MAX_BITS], *be = b + size; \
		assert(n_bits <= RS_MAX_BITS); \
		for (k = b; k != be; ++k) k->b = k->e = beg; \
		for (i = beg; i != end; ++i) ++b[rskey(*i)>>s&m].e; \
		for (k = b + 1; k != be; ++k) \
			k->e += (k-1)->e - beg, k->b = (k-1)->e; \
		for (k = b; k != be;) { \
			if (k->b != k->e) { \
				rsbucket_##name##_t *l; \
				if ((l = b + (rskey(*k->b)>>s&m)) != k) { \
					rstype_t tmp = *k->b, swap; \
					do { \
						swap = tmp; tmp = *l->b; *l->b++ = swap; \
						l = b + (rskey(tmp)>>s&m); \
					} while (l != k); \
					*k->b++ = tmp; \
				} else ++k->b; \
			} else ++k; \
		} \
		for (b->b = beg, k = b + 1; k != be; ++k) k->b = (k-1)->e; \
		if (s) { \
			s = s > n_bits? s - n_bits : 0; \
			for (k = b; k != be; ++k) \
				if (k->e - k->b > RS_MIN_SIZE) rs_sort_##name(k->b, k->e, n_bits, s); \
				else if (k->e - k->b > 1) rs_insertsort_##name(k->b, k->e); \
		} \
	} \
	void radix_sort_##name(rstype_t *beg, rstype_t *end) \
	{ \
		if (end - beg <= RS_MIN_SIZE) rs_insertsort_##name(beg, end); \
		else rs_sort_##name(beg, end, RS_MAX_BITS, (sizeof_key - 1) * RS_MAX_BITS); \
	}

/**************
 * splitmix32 *
 **************/

static inline uint32_t splitmix32(uint32_t *x)
{
	uint32_t z = (*x += 0x9e3779b9);
	z = (z ^ (z >> 16)) * 0x21f0aaad;
	z = (z ^ (z >> 15)) * 0x735a2d97;
	return z ^ (z >> 15);
}

/**********
 * IITree *
 **********/

typedef int64_t stype_t; // scalar type

/* Suppose there are N=2^(K+1)-1 sorted numbers in an array a[]. They
 * implicitly form a complete binary tree of height K+1. We consider leaves to
 * be at level 0. The binary tree has the following properties:
 *
 * 1. The lowest k-1 bits of nodes at level k are all 1. The k-th bit is 0.
 *    The first node at level k is indexed by 2^k-1. The root of the tree is
 *    indexed by 2^K-1.
 *
 * 2. For a node x at level k, its left child is x-2^(k-1) and the right child
 *    is x+2^(k-1).
 *
 * 3. For a node x at level k, it is a left child if its (k+1)-th bit is 0. Its
 *    parent node is x+2^k. Similarly, if the (k+1)-th bit is 1, x is a right
 *    child and its parent is x-2^k.
 *
 * 4. For a node x at level k, there are 2^(k+1)-1 nodes in the subtree
 *    descending from x, including x. The left-most leaf is x&~(2^k-1) (masking
 *    the lowest k bits to 0).
 *
 * When numbers can't fill a complete binary tree, the parent of a node may not
 * be present in the array. The implementation here still mimics a complete
 * tree, though getting the special casing right is a little complex. There may
 * be alternative solutions.
 *
 * As a sorted array can be considered as a binary search tree, we can
 * implement an interval tree on top of the idea. We only need to record, for
 * each node, the maximum value in the subtree descending from the node.
 */

typedef struct {
	stype_t st, en, max;
} intv_t;

#define intv_key(x) ((x).st)
KRADIX_SORT_INIT(intv, intv_t, intv_key, 8)

int iit_index(size_t n, intv_t *a)
{
	size_t i, last_i; // last_i points to the rightmost node in the tree
	stype_t last; // last is the max value at node last_i
	int k;
	if (n == 0) return -1;
	radix_sort_intv(a, a + n);
	for (i = 0; i < n; i += 2) last_i = i, last = a[i].max = a[i].en; // leaves (i.e. at level 0)
	for (k = 1; 1ULL<<k <= n; ++k) { // process internal nodes in the bottom-up order
		size_t x = 1ULL<<(k-1), i0 = (x<<1) - 1, step = x<<2; // i0 is the first node
		for (i = i0; i < n; i += step) { // traverse all nodes at level k
			stype_t el = a[i - x].max;                          // max value of the left child
			stype_t er = i + x < n? a[i + x].max : last; // of the right child
			stype_t e = a[i].en;
			e = e > el? e : el;
			e = e > er? e : er;
			a[i].max = e; // set the max value for node i
		}
		last_i = last_i>>k&1? last_i - x : last_i + x; // last_i now points to the parent of the original last_i
		if (last_i < n && a[last_i].max > last) // update last accordingly
			last = a[last_i].max;
	}
	return k - 1;
}

typedef struct {
	size_t x; // node
	int k, w; // k: level; w: 0 if left child hasn't been processed
} stackcell_t;

static inline intv_t *iit_push_back(intv_t **b, size_t *n_b, size_t *m_b)
{
	if (*n_b == *m_b) {
		*m_b += (*m_b >> 1) + 16;
		*b = (intv_t*)realloc(*b, *m_b * sizeof(intv_t));
	}
	return &(*b)[(*n_b)++];
}

size_t iit_overlap(size_t n, const intv_t *a, int max_level, const stype_t st, const stype_t en, intv_t **b, size_t *m_b)
{
	size_t n_b = 0;
	int t = 0;
	stackcell_t stack[64];
	stack[t++] = (stackcell_t) { (1ULL<<max_level) - 1, max_level, 0 }; // push the root; this is a top down traversal
	while (t) { // the following guarantees that numbers in out[] are always sorted
		stackcell_t z = stack[--t];
		if (z.k <= 3) { // we are in a small subtree; traverse every node in this subtree
			size_t i, i0 = z.x >> z.k << z.k, i1 = i0 + (1LL<<(z.k+1)) - 1;
			if (i1 >= n) i1 = n;
			for (i = i0; i < i1 && a[i].st < en; ++i)
				if (st < a[i].en) // if overlap, append to out[]
					*iit_push_back(b, &n_b, m_b) = a[i];
		} else if (z.w == 0) { // if left child not processed
			size_t y = z.x - (1LL<<(z.k-1)); // the left child of z.x; NB: y may be out of range (i.e. y>=n)
			stack[t++] = (stackcell_t) { z.x, z.k, 1 }; // re-add node z.x, but mark the left child having been processed
			if (y >= n || a[y].max > st) // push the left child if y is out of range or may overlap with the query
				stack[t++] = (stackcell_t) { y, z.k - 1, 0 };
		} else if (z.x < n && a[z.x].st < en) { // need to push the right child
			if (st < a[z.x].en) *iit_push_back(b, &n_b, m_b) = a[z.x]; // test if z.x overlaps the query; if yes, append to out[]
			stack[t++] = (stackcell_t) { z.x + (1ULL<<(z.k-1)), z.k - 1, 0 }; // push the right child
		}
	}
	return n_b;
}

/*****************
 * main function *
 *****************/

intv_t *gen_intv(size_t n, uint32_t *x, int bit_st, int bit_len)
{
	stype_t mask_st = (1<<bit_st) - 1;
	stype_t mask_len = (1<<bit_len) - 1;
	intv_t *a;
	size_t i;
	a = (intv_t*)malloc(n * sizeof(*a));
	for (i = 0; i < n; ++i) {
		a[i].st = splitmix32(x) & mask_st;
		a[i].en = a[i].st + (splitmix32(x) & mask_len);
	}
	return a;
}

int main(int argc, char *argv[])
{
	int bit_st = 28, bit_len = 14, max_level;
	uint32_t x = 11;
	size_t j, n = 500000, m_b = 0;
	intv_t *a1, *a2, *b = 0;
	stype_t tot_cov = 0;

	if (argc > 1) n = atoi(argv[1]);
	a1 = gen_intv(n, &x, bit_st, bit_len);
	a2 = gen_intv(n, &x, bit_st, bit_len);
	max_level = iit_index(n, a1);
	for (j = 0; j < n; ++j) {
		stype_t st0 = a2[j].st, en0 = a2[j].en, cov_st, cov_en, cov = 0;
		size_t n_b, i;
		n_b = iit_overlap(n, a1, max_level, st0, en0, &b, &m_b);
		if (n_b == 0) continue;
		cov_st = b[0].st > st0? b[0].st : st0;
		cov_en = b[0].en < en0? b[0].en : en0;
		for (i = 1; i < n_b; ++i) {
			stype_t st1 = b[i].st > st0? b[i].st : st0;
			stype_t en1 = b[i].en < en0? b[i].en : en0;
			if (st1 > cov_en) {
				cov += cov_en - cov_st;
				cov_st = st1, cov_en = en1;
			} else cov_en = cov_en > en1? cov_en : en1;
		}
		cov += cov_en - cov_st;
		tot_cov += cov;
	}
	free(a1); free(a2);
	printf("%llu\n", (unsigned long long)tot_cov);
	return 0;
}
