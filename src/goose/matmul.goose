// plb2 matmul: the product of two n x n matrices (1500 by default) by the
// i-k-j triple loop. A matrix is its rows back to back in one array, and
// each loop takes the rows it works on as slices. Asserting that each row
// slice is n long, which the compiler cannot work out from the bounds of
// `lo..lo + n` for an n read at run time, lets it drop every bounds check
// from the inner loop, which then vectorizes.

import std;
import os;

fn mat_gen(n: i64) -> f64[>..] {
    var a: f64[>..] = [];
    let tmp = 1.0 / n / n;
    for i in n {
        for j in n { a.push(tmp * (i - j) * (i + j)); }
    }
    return a;
}

// `simd`: the inner loop vectorizes, and the CPU's widest vectors run it.
simd fn mat_mul(n: i64, a: f64[:], b: f64[:], c: f64[:]) {
    for i in n {
        let lo = i * n;
        let ai = a[lo..lo + n];
        let ci = c[lo..lo + n];
        assert(ai.len == n && ci.len == n);
        for k in n {
            let aik = ai[k];
            let bk = b[k * n..k * n + n];
            assert(bk.len == n);
            for j in n { ci[j] += aik * bk[j]; }
        }
    }
}

fn main() {
    var n = 1500;
    let argv = args();
    if argv.len > 1 {
        let v, ok = parse_int(argv[1]);
        if ok { n = v; }
    }
    let a = mat_gen(n);
    let b = mat_gen(n);
    var c: f64[>..] = [];
    c.resize(n * n, 0.0);
    mat_mul(n, a, b, c);
    var line: u8[>..] = [];
    format_flt(line, c[(n >> 1) * n + (n >> 1)], 6);
    print(line);
}
