struct Sudoku {
	r: [[u16; 9]; 324],
	c: [[u16; 4]; 729]
}

impl Sudoku {
	pub fn new() -> Sudoku {
		let mut s = Sudoku { r: [[0u16; 9]; 324], c: [[0u16; 4]; 729] };
		let mut nr = [0; 324];
		let mut r = 0;
		for i in 0..9 {
			for j in 0..9 {
				for k in 0..9 {
					s.c[r][0] = 9 * i + j;
					s.c[r][1] = (i/3*3 + j/3) * 9 + k + 81;
					s.c[r][2] = 9 * i + k + 162;
					s.c[r][3] = 9 * j + k + 243;
					r += 1;
				}
			}
		}
		for r in 0..729 {
			for c2 in 0..4 {
				let k = s.c[r][c2] as usize;
				s.r[k][nr[k]] = r as u16;
				nr[k] += 1;
			}
		}
		return s;
	}
	#[inline(always)]
	fn forward(&self, sr: &mut [i8], sc: &mut [u8], c: u16, min: &mut u8, min_c: &mut u16) {
		for rr in self.r[c as usize] {
			let srrr = &mut sr[rr as usize];
			*srrr += 1;
			if *srrr == 1 {
				for cc in self.c[rr as usize] {
					let sccc = &mut sc[cc as usize];
					*sccc -= 1;
					if *sccc < *min {
						*min = *sccc;
						*min_c = cc;
					}
				}
			}
		}
	}
	#[inline(always)]
	fn revert(&self, sr: &mut [i8], sc: &mut [u8], c: u16) {
		for rr in self.r[c as usize] {
			let srrr = &mut sr[rr as usize];
			*srrr -= 1;
			if *srrr == 0 {
				for i in self.c[rr as usize] {
					sc[i as usize] += 1;
				}
			}
		}
	}
	#[inline(always)]
	fn update(&self, sr: &mut [i8], sc: &mut [u8], r: u16, v: i32) -> i32 {
		let mut min = 10;
		let mut min_c = 0;
		for i in self.c[r as usize] {
			sc[i as usize] += (v<<7) as u8;
		}
		for c in self.c[r as usize] {
			if v > 0 {
				self.forward(sr, sc, c, &mut min, &mut min_c);
			} else {
				self.revert(sr, sc, c);
			}
		}
		return (min as i32)<<16 | min_c as i32;
	}
	pub fn solve(&self, inp: &str) {
		let mut sc = [9u8; 324];
		let mut sr = [0i8; 729];
		let mut cr = [-1i8; 81];
		let mut cc = [-1i16; 81];
		let mut s = [-1i8; 81];
		let mut s8 = [48u8; 81];
		let mut hints = 0;
		for i in 0..81 {
			let c = inp.as_bytes()[i] as i8;
			if c >= '1' as i8 && c <= '9' as i8 {
				s[i] = c - '1' as i8;
				self.update(&mut sr, &mut sc, (i * 9 + s[i] as usize) as u16, 1);
				hints += 1;
				s8[i] = c as u8;
			}
		}
		let mut i = 0i32;
		let mut dir = 1;
		let mut cand: i32 = 10<<16|0;
		loop {
			while i >= 0 && i < 81 - hints {
				if dir == 1 {
					let mut min = (cand>>16) as u8;
					cc[i as usize] = (cand & 0xffff) as i16;
					if min > 1 {
						for c in 0..324 {
							if sc[c] < min {
								min = sc[c];
								cc[i as usize] = c as i16;
								if min <= 1 {
									break;
								}
							}
						}
					}
					if min == 0 || min == 10 {
						cr[i as usize] = -1;
						i -= 1;
						dir = -1;
					}
				}
				let c = cc[i as usize] as usize;
				if dir == -1 && cr[i as usize] >= 0 {
					self.update(&mut sr, &mut sc, self.r[c][cr[i as usize] as usize], -1);
				}
				let mut r2 = (cr[i as usize] as usize) + 1;
				while r2 < 9 && sr[self.r[c][r2] as usize] != 0 {
					r2 += 1;
				}
				if r2 < 9 {
					cand = self.update(&mut sr, &mut sc, self.r[c][r2], 1);
					cr[i as usize] = r2 as i8;
					i += 1;
					dir = 1;
				} else {
					cr[i as usize] = -1;
					i -= 1;
					dir = -1;
				}
			}
			if i < 0 {
				break;
			}
			for j in 0 .. (i as usize) {
				let r = self.r[cc[j] as usize][cr[j] as usize];
				s8[(r/9) as usize] = (r%9 + '1' as u16) as u8;
			}
			println!("{}", std::str::from_utf8(&s8).unwrap());
			i -= 1; dir = -1;
		}
	}
}

fn main() {
	let hard20: [&str; 20] = [
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
	let s = Sudoku::new();
	let n = 200;
	for _ in 0..n {
		for j in 0..20 {
			s.solve(hard20[j]);
			println!();
		}
	}
}
