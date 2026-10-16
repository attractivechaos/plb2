// plb2 bedcov: for each of 1,000,000 random intervals, the length of the
// part covered by another 1,000,000, found with an implicit interval tree:
// the intervals sorted by start (by radix sort here) form a complete binary
// search tree in place, each node holding the largest end in its subtree.

import std;
import os;

// --- splitmix32 -----------------------------------------------------------

struct SplitMix32 { x: u32 }

fn next(r: SplitMix32&) -> u32 {
    r.x += 0x9e3779b9;
    var z = r.x;
    z = (z ^ (z >> 16)) * 0x21f0aaad;
    z = (z ^ (z >> 15)) * 0x735a2d97;
    return z ^ (z >> 15);
}

// --- radix sort -------------------------------------------------------------

struct Intv { st: i64, en: i64, max: i64, data: i64 }

let RS_MIN_SIZE = 64;
let RS_BITS = 8;
let RS_SIZE = 1 << RS_BITS;

fn rs_insertsort(a: Intv[:]) {
    for i in 1..a.len {
        if a[i].st < a[i - 1].st {
            let tmp = a[i];
            var j = i;
            while j > 0 && tmp.st < a[j - 1].st {
                a[j] = a[j - 1];
                j--;
            }
            a[j] = tmp;
        }
    }
}

struct Bucket { b: i64, e: i64 }

// An in-place MSD radix sort on bits s and up of the start, RS_BITS at a
// time: each element is swapped straight into its bucket, along a cycle of
// displaced elements.
recursive fn rs_sort(a: Intv[:], s: i64) {
    let m = RS_SIZE - 1;
    var b: Bucket[RS_SIZE] = [Bucket { b: 0, e: 0 }; RS_SIZE];
    for i in a.len { b[(a[i].st >> s) & m].e++; }
    for k in 1..RS_SIZE {
        b[k].e += b[k - 1].e;
        b[k].b = b[k - 1].e;
    }
    var k = 0;
    while k < RS_SIZE {
        if b[k].b != b[k].e {
            let l = (a[b[k].b].st >> s) & m;
            if l != k {
                var tmp = a[b[k].b];
                var ll = l;
                loop {
                    let swap = tmp;
                    tmp = a[b[ll].b];
                    a[b[ll].b] = swap;
                    b[ll].b++;
                    ll = (tmp.st >> s) & m;
                    if ll == k { break; }
                }
                a[b[k].b] = tmp;
                b[k].b++;
            } else {
                b[k].b++;
            }
        } else {
            k++;
        }
    }
    b[0].b = 0;
    for k2 in 1..RS_SIZE { b[k2].b = b[k2 - 1].e; }
    if s != 0 {
        let s2 = if s > RS_BITS { s - RS_BITS } else { 0 };
        for k2 in RS_SIZE {
            let len = b[k2].e - b[k2].b;
            if len > RS_MIN_SIZE {
                rs_sort(a[b[k2].b..b[k2].e], s2);
            } else if len > 1 {
                rs_insertsort(a[b[k2].b..b[k2].e]);
            }
        }
    }
}

fn radix_sort(a: Intv[:]) {
    if a.len <= RS_MIN_SIZE {
        rs_insertsort(a);
    } else {
        rs_sort(a, 7 * RS_BITS);             // a start is 8 bytes
    }
}

// --- the implicit interval tree ---------------------------------------------

// In N = 2^(K+1)-1 sorted intervals, node x at level k (leaves at 0) has its
// lowest k-1 bits set and bit k clear; its children are x -/+ 2^(k-1), and
// its subtree is the 2^(k+1)-1 intervals from x & ~(2^k-1). A tree that is
// not complete is treated as the complete one with its right end missing.
// Sorts a and sets every node's max; returns the root's level.
fn iit_index(a: Intv[:]) -> i64 {
    let n = a.len;
    if n == 0 { return -1; }
    radix_sort(a);
    var last_i = 0;                          // the rightmost node
    var last = 0;                            // and its max
    var i = 0;
    while i < n {                            // the leaves
        last_i = i;
        a[i].max = a[i].en;
        last = a[i].max;
        i += 2;
    }
    var k = 1;
    while 1 << k <= n {                      // the inner nodes, bottom up
        let x = 1 << (k - 1);
        let step = x << 2;
        i = (x << 1) - 1;                    // the level's first node
        while i < n {
            let el = a[i - x].max;                               // the left child's max
            let er = if i + x < n { a[i + x].max } else { last }; // the right one's
            a[i].max = max(a[i].en, max(el, er));
            i += step;
        }
        last_i = if (last_i >> k) & 1 != 0 { last_i - x } else { last_i + x };  // its parent
        if last_i < n && a[last_i].max > last { last = a[last_i].max; }
        k++;
    }
    return k - 1;
}

struct StackCell { x: i64, k: i32, w: i32 }  // w: whether the left child is done

// Appends the intervals of a that overlap [st, en) to b, in sorted order.
fn iit_overlap(a: const Intv[:], max_level: i64, st: i64, en: i64, b: Intv[>..]&) {
    let n = a.len;
    b.clear();
    var stack: StackCell[64] = [StackCell { x: 0, k: 0, w: 0 }; 64];
    stack[0] = StackCell { x: (1 << max_level) - 1, k: max_level as! i32, w: 0 };  // the root
    var t = 1;
    while t > 0 {
        t--;
        let z = stack[t];
        if z.k <= 3 {                        // a small subtree: scan it
            let i0 = z.x >> z.k << z.k;
            let i1 = min(i0 + (1 << (z.k + 1)) - 1, n);
            var i = i0;
            while i < i1 && a[i].st < en {
                if st < a[i].en { b.push(a[i]); }
                i++;
            }
        } else if z.w == 0 {                 // the left child next, then x again
            let y = z.x - (1 << (z.k - 1));  // which may be past the end
            stack[t] = StackCell { x: z.x, k: z.k, w: 1 };
            t++;
            if y >= n || a[y].max > st {
                stack[t] = StackCell { x: y, k: z.k - 1, w: 0 };
                t++;
            }
        } else if z.x < n && a[z.x].st < en {   // x itself, then the right child
            if st < a[z.x].en { b.push(a[z.x]); }
            stack[t] = StackCell { x: z.x + (1 << (z.k - 1)), k: z.k - 1, w: 0 };
            t++;
        }
    }
}

// --- main -------------------------------------------------------------------

fn gen_intv(n: i64, r: SplitMix32&, bit_st: i64, bit_len: i64) -> Intv[>..] {
    let mask_st = (1 << bit_st) - 1;
    let mask_len = (1 << bit_len) - 1;
    var a: Intv[>..] = [];
    for i in n {
        let st = r.next() & mask_st;
        let en = st + (r.next() & mask_len);
        a.push(Intv { st: st, en: en, max: 0, data: i });
    }
    return a;
}

fn main() {
    var n = 1000000;
    let argv = args();
    if argv.len > 1 {
        let v, ok = parse_int(argv[1]);
        if ok { n = v; }
    }
    var r = SplitMix32 { x: 11 };
    var a1 = gen_intv(n, r, 28, 14);
    let a2 = gen_intv(n, r, 28, 14);
    let max_level = iit_index(a1);
    var b: Intv[>..] = [];
    var tot_cov = 0;
    for q in a2 {
        let st0 = q.st;
        let en0 = q.en;
        iit_overlap(a1, max_level, st0, en0, b);
        if b.len == 0 { continue; }
        var cov_st = max(b[0].st, st0);
        var cov_en = min(b[0].en, en0);
        var cov = 0;
        for bi in b[1..] {
            let st1 = max(bi.st, st0);
            let en1 = min(bi.en, en0);
            if st1 > cov_en {
                cov += cov_en - cov_st;
                cov_st = st1;
                cov_en = en1;
            } else {
                cov_en = max(cov_en, en1);
            }
        }
        cov += cov_en - cov_st;
        tot_cov += cov;
    }
    print(tot_cov);
}
