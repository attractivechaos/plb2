class Sudoku {
	var R: [[Int]]
	var C: [[Int]]
	init() {
		self.R = Array(repeating: Array(repeating: 0, count: 9), count: 324);
		self.C = Array(repeating: Array(repeating: 0, count: 4), count: 729);
		var r = 0
		for i in 0...8 {
			for j in 0...8 {
				for k in 0...8 {
					self.C[r][0] = 9 * i + j;
					self.C[r][1] = (i/3*3 + j/3) * 9 + k + 81;
					self.C[r][2] = 9 * i + k + 162;
					self.C[r][3] = 9 * j + k + 243;
					r += 1;
				}
			}
		}
		var nr = Array(repeating: 0, count: 324);
		for r in 0...728 {
			for c2 in 0...3 {
				let k = self.C[r][c2];
				self.R[k][nr[k]] = r;
				nr[k] += 1;
			}
		}
	}
	func update(_ sr: inout [Int], _ sc: inout [Int], _ r: Int, _ v: Int) -> Int {
		for c2 in 0...3 {
			sc[self.C[r][c2]] ^= 1<<7;
		}
		var min = 10;
		var min_c = 0;
		for c2 in 0...3 {
			let c = self.C[r][c2];
			if v > 0 { // move forward
				for r2 in 0...8 {
					let rr = self.R[c][r2];
					let tmp = sr[rr];
					sr[rr] += 1;
					if tmp != 0 {
						continue;
					}
					for cc2 in 0...3 {
						let cc = self.C[rr][cc2];
						sc[cc] -= 1;
						if (sc[cc] < min) {
							min = sc[cc];
							min_c = cc;
						}
					}
				}
			} else { // revert
				for r2 in 0...8 {
					let rr = self.R[c][r2];
					sr[rr] -= 1;
					if sr[rr] != 0 {
						continue;
					}
					sc[self.C[rr][0]] += 1;
					sc[self.C[rr][1]] += 1;
					sc[self.C[rr][2]] += 1;
					sc[self.C[rr][3]] += 1;
				}
			}
		}
		return min<<16 | min_c;
	}
	// solve a Sudoku; _s is the standard dot/number representation
	func solve(_ s: String) {
		var sr = Array(repeating: 0, count: 729);
		var cr = Array(repeating: -1, count: 81);
		var sc = Array(repeating: 9, count: 324);
		var cc = Array(repeating: -1, count: 81);
		var out = Array(s);
		var hints = 0;
		for i in 0..<81 {
			let a = out[i].asciiValue ?? 48;
			if a >= 49 && a <= 57 {
				_ = self.update(&sr, &sc, i * 9 + Int(a - 49), 1);
				hints += 1;
			}
		}
		var dir = 1;
		var cand = 10<<16 | 0;
		var n = 0;
		var i = 0;
		while true {
			while i >= 0 && i < 81 - hints {
				if dir == 1 {
					var min = cand>>16;
					cc[i] = cand&0xffff;
					if min > 1 {
						for c in 0...323 {
							if sc[c] < min {
								min = sc[c];
								cc[i] = c;
								if min <= 1 {
									break;
								}
							}
						}
					}
					if min == 0 || min == 10 {
						cr[i] = -1;
						dir = -1;
						i -= 1;
					}
				}
				let c = cc[i];
				if dir == -1 && cr[i] >= 0 {
					_ = self.update(&sr, &sc, self.R[c][cr[i]], -1);
				}
				var r2 = cr[i] + 1;
				while r2 < 9 && sr[self.R[c][r2]] != 0 {
					r2 += 1;
				}
				if r2 < 9 {
					cand = self.update(&sr, &sc, self.R[c][r2], 1);
					cr[i] = r2;
					i += 1;
					dir = 1;
				} else {
					cr[i] = -1;
					i -= 1;
					dir = -1;
				}
			}
			if i < 0 {
				break;
			}
			for j in 0..<i {
				let r = self.R[cc[j]][cr[j]];
				out[r/9] = Character(String(r%9 + 1));
			}
			print(String(out))
			n -= 1;
			i -= 1;
			dir = -1;
		}
	}
}

var hard20: [String] = [
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
	".....1.2.3...4.5.....6....7..2.....1.8..9..3.4.....8..5....2....9..3.4....67....."
];

var n = 200;
var a = Sudoku();
for _ in 0...n-1 {
	for j in 0...19 {
		a.solve(hard20[j]);
		print();
	}
}
