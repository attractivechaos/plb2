#!/usr/bin/env k8

class SudokuSolver {
	#R;
	#C;
	constructor() {
		this.C = [], this.R = [];
		let r = 0;
		for (let i = 0; i < 9; ++i)
			for (let j = 0; j < 9; ++j)
				for (let k = 0; k < 9; ++k)
					this.C[r++] = [ 9 * i + j, (Math.floor(i/3)*3 + Math.floor(j/3)) * 9 + k + 81, 9 * i + k + 162, 9 * j + k + 243 ];
		for (let c = 0; c < 324; ++c) this.R[c] = [];
		for (let r = 0; r < 729; ++r)
			for (let c2 = 0; c2 < 4; ++c2)
				this.R[this.C[r][c2]].push(r);
	}
	#update(sr, sc, r, v) {
		let min = 10, min_c = 0;
		for (let c2 = 0; c2 < 4; ++c2) sc[this.C[r][c2]] += v<<7;
		for (let c2 = 0; c2 < 4; ++c2) {
			let rr, c = this.C[r][c2];
			if (v > 0) {
				for (let r2 = 0; r2 < 9; ++r2) {
					if (sr[rr = this.R[c][r2]]++ != 0) continue;
					for (let cc2 = 0; cc2 < 4; ++cc2) {
						let cc = this.C[rr][cc2];
						if (--sc[cc] < min)
							min = sc[cc], min_c = cc;
					}
				}
			} else { // revert
				for (let r2 = 0; r2 < 9; ++r2) {
					if (--sr[rr = this.R[c][r2]] != 0) continue;
					const p = this.C[rr];
					++sc[p[0]]; ++sc[p[1]]; ++sc[p[2]]; ++sc[p[3]];
				}
			}
		}
		return min<<16 | min_c;
	}
	solve(_s) {
		let hints = 0, sr = [], sc = [], cr = [], cc = [], out = [], ret = [];
		for (let r = 0; r < 729; ++r) sr[r] = 0;
		for (let c = 0; c < 324; ++c) sc[c] = 9;
		for (let i = 0; i < 81; ++i) {
			const a = _s[i] >= '1' && _s[i] <= '9'? _s.charCodeAt(i) - 49 : -1;
			if (a >= 0) this.#update(sr, sc, i * 9 + a, 1);
			if (a >= 0) ++hints;
			cr[i] = cc[i] = -1, out[i] = a + 1;
		}
		let i = 0, dir = 1, cand = 10<<16|0;
		for (;;) {
			while (i >= 0 && i < 81 - hints) {
				if (dir == 1) {
					let min = cand>>16;
					cc[i] = cand&0xffff;
					if (min > 1) {
						for (let c = 0; c < 324; ++c) {
							if (sc[c] < min) {
								min = sc[c], cc[i] = c;
								if (min <= 1) break;
							}
						}
					}
					if (min == 0 || min == 10) cr[i--] = dir = -1;
				}
				const c = cc[i];
				if (dir == -1 && cr[i] >= 0)
					this.#update(sr, sc, this.R[c][cr[i]], -1);
				let r2;
				for (r2 = cr[i] + 1; r2 < 9; ++r2)
					if (sr[this.R[c][r2]] == 0) break;
				if (r2 < 9) {
					cand = this.#update(sr, sc, this.R[c][r2], 1);
					cr[i++] = r2; dir = 1;
				} else cr[i--] = dir = -1;
			}
			if (i < 0) break;
			let y = [];
			for (let j = 0; j < 81; ++j) y[j] = out[j];
			for (let j = 0; j < i; ++j) {
				const r = this.R[cc[j]][cr[j]];
				y[Math.floor(r/9)] = r%9 + 1;
			}
			ret.push(y);
			--i; dir = -1;
		}
		return ret;
	}
}

const hard20 = `
..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9
.......12........3..23..4....18....5.6..7.8.......9.....85.....9...4.5..47...6...
.2..5.7..4..1....68....3...2....8..3.4..2.5.....6...1...2.9.....9......57.4...9..
........3..1..56...9..4..7......9.5.7.......8.5.4.2....8..2..9...35..1..6........
12.3....435....1....4........54..2..6...7.........8.9...31..5.......9.7.....6...8
1.......2.9.4...5...6...7...5.9.3.......7.......85..4.7.....6...3...9.8...2.....1
.......39.....1..5..3.5.8....8.9...6.7...2...1..4.......9.8..5..2....6..4..7.....
12.3.....4.....3....3.5......42..5......8...9.6...5.7...15..2......9..6......7..8
..3..6.8....1..2......7...4..9..8.6..3..4...1.7.2.....3....5.....5...6..98.....5.
1.......9..67...2..8....4......75.3...5..2....6.3......9....8..6...4...1..25...6.
..9...4...7.3...2.8...6...71..8....6....1..7.....56...3....5..1.4.....9...2...7..
....9..5..1.....3...23..7....45...7.8.....2.......64...9..1.....8..6......54....7
4...3.......6..8..........1....5..9..8....6...7.2........1.27..5.3....4.9........
7.8...3.....2.1...5.........4.....263...8.......1...9..9.6....4....7.5...........
3.7.4...........918........4.....7.....16.......25..........38..9....5...2.6.....
........8..3...4...9..2..6.....79.......612...6.5.2.7...8...5...1.....2.4.5.....3
.......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6...
.......12....35......6...7.7.....3.....4..8..1...........12.....8.....4..5....6..
1.......2.9.4...5...6...7...5.3.4.......6........58.4...2...6...3...9.8.7.......1
.....1.2.3...4.5.....6....7..2.....1.8..9..3.4.....8..5....2....9..3.4....67.....`;

var ccc = {
	print: typeof print == "function"? print : console.log,
	argv: typeof k8_version == "function"? arguments.slice(0) : typeof Deno == "object"? Deno.args.slice(0) : typeof Bun == "function"? Bun.argv.slice(2) : process.argv.splice(2)
};

function main(args) {
	const n = args && args.length > 0? parseInt(args[0]) : 100;
	const a = hard20.split("\n");
	const solver = new SudokuSolver();
	for (let i = 0; i < n; ++i) {
		for (let j = 0; j < a.length; ++j) {
			if (a[j].length >= 81) {
				const r = solver.solve(a[j]);
				for (let i = 0; i < r.length; ++i)
					ccc.print(r[i].join(''));
				ccc.print();
			}
		}
	}
}

main(ccc.argv);
