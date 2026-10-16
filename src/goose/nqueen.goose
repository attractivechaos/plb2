// plb2 nqueen: counts the solutions of the n-queens problem (15 by default)
// with an iterative backtracking search over bit masks, after the second C
// implementation on Rosetta Code.

import std;
import os;

let NQ_MAX = 31;

fn nq_solve(n: i32) -> i32 {
    var a: i32[NQ_MAX] = [-1; NQ_MAX];       // the column chosen at each row
    var l: u32[NQ_MAX] = [0; NQ_MAX];        // per row, the columns the queens above
    var c: u32[NQ_MAX] = [0; NQ_MAX];        // attack along left diagonals, columns
    var r: u32[NQ_MAX] = [0; NQ_MAX];        // and right diagonals
    let y0: u32 = (1 << n) - 1;
    var m: i32 = 0;
    var k: i32 = 0;
    while k >= 0 {
        let y = (l[k] | c[k] | r[k]) & y0;   // the columns row k cannot take
        if (y ^ y0) >> (a[k] + 1) != 0 {     // a column past the last one tried is free
            var i = a[k] + 1;
            while i < n {                    // the first of them
                if y & (1 << i) == 0 { break; }
                i++;
            }
            if k < n - 1 {
                let z: u32 = 1 << i;
                a[k] = i;
                k++;
                l[k] = (l[k - 1] | z) << 1;
                c[k] = c[k - 1] | z;
                r[k] = (r[k - 1] | z) >> 1;
            } else {
                m++;                         // a solution
                k--;
            }
        } else {
            a[k] = -1;                       // no choice left: backtrack
            k--;
        }
    }
    return m;
}

fn main() {
    var n = 15;
    let argv = args();
    if argv.len > 1 {
        let v, ok = parse_int(argv[1]);
        if ok { n = v; }
    }
    if n > NQ_MAX || n <= 0 { abort("n out of range"); }
    print(nq_solve(n as i32));
}
