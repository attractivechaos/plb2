const std = @import("std");

fn nq_solve(n: u5) i32 {
	const NQ_MAX: u32 = 31;
	var a = [_]i32{-1} ** NQ_MAX;
	var l = [_]u32{0} ** NQ_MAX;
	var c = [_]u32{0} ** NQ_MAX;
	var r = [_]u32{0} ** NQ_MAX;
	var m: i32 = 0;
	const y0: u32 = (@as(u32, 1) << n) - 1;
	var k: u32 = 0;
	while (true) {
		const y = (l[k] | c[k] | r[k]) & y0;
		const t: u32 = @bitCast(a[k] + 1);
		if ((y ^ y0) >> @truncate(t) != 0) {
			var i: u32 = @bitCast(a[k] + 1);
			while (i < n and (y & (@as(u32, 1) << @truncate(i))) != 0) {
				i += 1;
			}
			if (k < n - 1) {
				const z: u32 = @as(u32, 1) << @truncate(i);
				a[k] = @bitCast(i);
				k += 1;
				l[k] = (l[k-1]|z)<<1;
				c[k] = c[k-1]|z;
				r[k] = (r[k-1]|z)>>1;
			} else {
				m += 1;
				if (k == 0) {
					break;
				}
				k -= 1;
			}
		} else {
			a[k] = -1;
			if (k == 0) {
				break;
			}
			k -= 1;
		}
	}
	return m;
}

pub fn main() !void {
	var n: u5 = 15;
	std.debug.print("{d}\n", .{nq_solve(n)});
}
