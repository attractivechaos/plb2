const std = @import("std");

fn mat_alloc(n: u32) [][]f64 {
	var a: [][]f64 = undefined;
	a = std.heap.page_allocator.alloc([]f64, n) catch unreachable;
	for (a) |*row| {
		row.* = std.heap.page_allocator.alloc(f64, n) catch unreachable;
	}
	return a;
}

fn matgen(n: u32) [][]f64 {
	var a = mat_alloc(n);
	const f: f64 = @floatFromInt(n);
	const tmp  = 1.0 / f / f;
	for (0..n) |i| {
		const ii: i64 = @bitCast(i);
		for (0..n) |j| {
			const jj: i64 = @bitCast(j);
			a[i][j] = tmp * @as(f64, @floatFromInt(ii - jj)) * @as(f64, @floatFromInt(ii + jj));
		}
	}
	return a;
}

fn matmul(n: u32, a: [][]f64, b: [][]f64) [][]f64 {
	var c = mat_alloc(n);
	for (0..n) |i| {
		for (0..n) |j| {
			c[i][j] = 0.0;
		}
	}
	for (0..n) |i| {
		var ci = c[i];
		for (0..n) |k| {
			const aik = a[i][k];
			const bk = b[k];
			for (0..n) |j| {
				ci[j] += aik * bk[j];
			}
		}
	}
	return c;
}

pub fn main() !void {
	const n = 1500;
	const a = matgen(n);
	const b = matgen(n);
	const c = matmul(n, a, b);
	std.debug.print("{}\n", .{c[n>>1][n>>1]});
	// FIXME: deallocate!
}
