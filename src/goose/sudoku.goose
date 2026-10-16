// plb2 sudoku: solves 20 hard sudokus 200 times over with the kudoku
// algorithm, an improved version of Guenter Stertenbrink's suexco.c, and
// prints every solution.
//
// A sudoku has 729 choices (9 numbers for each of 81 cells) and 324
// constraints, each a set of choices of which exactly one is made: a cell
// holds one number, and each number appears once per 3x3 box, per row and
// per column. Solving is picking choices that hit every constraint once, by
// backtracking. Every choice is in 4 constraints and every constraint has 9
// choices, so the 729x324 matrix is stored as the coordinates of its
// non-zero elements, and the search keeps, per constraint, how many of its
// choices are still allowed, so that it can branch on the most constrained
// one.

import std;
import os;

// The sparse form of the choice/constraint matrix.
struct SdAux {
    r: u16[9][324],   // the choices of constraint c are r[c][0..9]
    c: u16[4][729],   // the constraints of choice r are c[r][0..4]
}

fn sd_genmat() -> SdAux {
    var a = SdAux { .. };
    var r = 0;
    for i: u16 in 9 {
        for j: u16 in 9 {
            for k: u16 in 9 {
                a.c[r][0] = 9 * i + j;                      // row-column
                a.c[r][1] = (i / 3 * 3 + j / 3) * 9 + k + 81; // box-number
                a.c[r][2] = 9 * i + k + 162;                // row-number
                a.c[r][3] = 9 * j + k + 243;                // column-number
                r++;
            }
        }
    }
    var nr: u8[324] = [0; 324];
    for r2: u16 in 729 {
        for c2 in 4 {
            let k = a.c[r2][c2];
            a.r[k][nr[k]] = r2;
            nr[k]++;
        }
    }
    return a;
}

// Makes choice r: uses up its 4 constraints and forbids the other choices in
// them. sr[r] counts how many made choices forbid choice r, and sc[c] how
// many choices of constraint c are allowed, with bit 7 set once c is used.
// Returns the constraint left with the fewest allowed choices among those
// it touched, and that number, packed as min << 16 | constraint.
fn sd_update_for(aux: const SdAux&, sr: i8[729]&, sc: u8[324]&, r: u16) -> i32 {
    var min: u8 = 10;
    var min_c: u16 = 0;
    let cs .= aux.c[r];          // `.=` names the rows in place: copying them costs 13%
    for c2 in 4 { sc[cs[c2]] |= 1 << 7; }
    for c2 in 4 {
        let rs .= aux.r[cs[c2]];
        for r2 in 9 {
            let rr = rs[r2];
            let forbidden = sr[rr];
            sr[rr] = forbidden + 1;
            if forbidden != 0 { continue; }
            let ccs .= aux.c[rr];
            for cc2 in 4 {
                let cc = ccs[cc2];
                let n = sc[cc] - 1;
                sc[cc] = n;
                if n < min {
                    min = n;
                    min_c = cc;
                }
            }
        }
    }
    return (min as i32) << 16 | min_c;
}

// Takes choice r back.
fn sd_update_rev(aux: const SdAux&, sr: i8[729]&, sc: u8[324]&, r: u16) {
    let cs .= aux.c[r];
    for c2 in 4 { sc[cs[c2]] &= 0x7f; }
    for c2 in 4 {
        let rs .= aux.r[cs[c2]];
        for r2 in 9 {
            let rr = rs[r2];
            let forbidden = sr[rr] - 1;
            sr[rr] = forbidden;
            if forbidden != 0 { continue; }
            let p .= aux.c[rr];
            sc[p[0]] += 1;
            sc[p[1]] += 1;
            sc[p[2]] += 1;
            sc[p[3]] += 1;
        }
    }
}

// Solves the sudoku in s, '.' for an empty cell, and appends each solution
// and a newline to out. Returns the number of solutions.
fn sd_solve(aux: const SdAux&, s: const u8[:], out: u8[>..]&) -> i64 {
    var sr: i8[729] = [0; 729];      // no choice is forbidden
    var sc: u8[324] = [9; 324];      // 9 allowed choices; no constraint used
    var cr: i8[81] = [-1; 81];       // cr[i]: the choice made at step i, of the 9 of cc[i]
    var cc: i16[81] = [-1; 81];      // cc[i]: the constraint branched on at step i
    var grid: u8[81] = [0; 81];
    var hints = 0;
    for i in 81 {
        let ch = s[i];
        if ch >= '1' && ch <= '9' {
            sd_update_for(aux, sr, sc, (i * 9 + (ch - '1')) as! u16);
            hints++;
        }
        grid[i] = ch;
    }
    var n = 0;
    var dir = 1;                     // 1: forward; -1: backtrack
    var cand: i32 = 10 << 16;
    var i = 0;
    loop {
        while i >= 0 && i < 81 - hints {   // at most 81 - hints steps
            if dir == 1 {
                var min = cand >> 16;
                cc[i] = (cand & 0xffff) as! i16;
                if min > 1 {
                    for c: i16 in 324 {
                        if sc[c] < min {
                            min = sc[c];
                            cc[i] = c;
                            if min <= 1 { break; }   // no constraint does better
                        }
                    }
                }
                if min == 0 || min == 10 {
                    cr[i] = -1;
                    i--;
                    dir = -1;
                }
            }
            let c = cc[i];
            if dir == -1 && cr[i] >= 0 { sd_update_rev(aux, sr, sc, aux.r[c][cr[i]]); }
            var r2 = cr[i] + 1;          // the next choice that is not forbidden
            while r2 < 9 {
                if sr[aux.r[c][r2]] == 0 { break; }
                r2++;
            }
            if r2 < 9 {
                cand = sd_update_for(aux, sr, sc, aux.r[c][r2]);
                cr[i] = r2;
                i++;
                dir = 1;
            } else {
                cr[i] = -1;
                i--;
                dir = -1;
            }
        }
        if i < 0 { break; }
        for j in i {
            let r = aux.r[cc[j]][cr[j]];
            grid[r / 9] = (r % 9) as! u8 + '1';
        }
        out.append(grid);
        out.push('\n');
        n++;
        i--;
        dir = -1;
    }
    return n;
}

let hard20 = [
    "..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9",
    ".......12........3..23..4....18....5.6..7.8.......9.....85.....9...4.5..47...6...",
    ".2..5.7..4..1....68....3...2....8..3.4..2.5.....6...1...2.9.....9......57.4...9..",
    "........3..1..56...9..4..7......9.5.7.......8.5.4.2....8..2..9...35..1..6........",
    "12.3....435....1....4........54..2..6...7.........8.9...31..5.......9.7.....6...8",
    "1.......2.9.4...5...6...7...5.9.3.......7.......85..4.7.....6...3...9.8...2.....1",
    ".......39.....1..5..3.5.8....8.9...6.7...2...1..4.......9.8..5..2....6..4..7.....",
    "12.3.....4.....3....3.5......42..5......8...9.6...5.7...15..2......9..6......7..8",
    "..3..6.8....1..2......7...4..9..8.6..3..4...1.7.2.....3....5.....5...6..98.....5.",
    "1.......9..67...2..8....4......75.3...5..2....6.3......9....8..6...4...1..25...6.",
    "..9...4...7.3...2.8...6...71..8....6....1..7.....56...3....5..1.4.....9...2...7..",
    "....9..5..1.....3...23..7....45...7.8.....2.......64...9..1.....8..6......54....7",
    "4...3.......6..8..........1....5..9..8....6...7.2........1.27..5.3....4.9........",
    "7.8...3.....2.1...5.........4.....263...8.......1...9..9.6....4....7.5...........",
    "3.7.4...........918........4.....7.....16.......25..........38..9....5...2.6.....",
    "........8..3...4...9..2..6.....79.......612...6.5.2.7...8...5...1.....2.4.5.....3",
    ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6...",
    ".......12....35......6...7.7.....3.....4..8..1...........12.....8.....4..5....6..",
    "1.......2.9.4...5...6...7...5.3.4.......6........58.4...2...6...3...9.8.7.......1",
    ".....1.2.3...4.5.....6....7..2.....1.8..9..3.4.....8..5....2....9..3.4....67.....",
];

fn main() {
    var n = 200;
    let argv = args();
    if argv.len > 1 {
        let v, ok = parse_int(argv[1]);
        if ok { n = v; }
    }
    let aux = sd_genmat();
    // stdout is unbuffered, so the output collects here and goes out in
    // blocks, as C's stdio buffer sends it.
    var out: u8[>..] = [];
    for i in n {
        for s in hard20 {
            sd_solve(aux, s, out);
            out.push('\n');
            if out.len >= 1 << 16 {
                write_stdout(out);
                out.clear();
            }
        }
    }
    write_stdout(out);
}
