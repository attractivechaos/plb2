# This sample demonstrates how various systems optimizations can be applied to a
# naive matmul implementation in Mojo to gain significant performance speedups

import benchmark
from memory import memset_zero
from algorithm import (
    vectorize_unroll,
    parallelize,
    Static2DTileUnitFunc as Tile2DFunc,
)

alias n = 1500  # cols of A, B, and C
alias type = DType.float32

# simdwidth of = amount of `type` elements that fit into a single SIMD register
# 2x multiplier will use multiple SIMD registers in parallel where possible
alias nelts = simdwidthof[type]() * 2
alias tile_n = 64  # N must be a multiple of this
alias tile_k = 4  # K must be a multiple of this


struct Matrix[rows: Int, cols: Int]:
    var data: DTypePointer[type]

    # Initialize zeroeing all values
    fn __init__(inout self):
        self.data = DTypePointer[type].alloc(rows * cols)
        memset_zero(self.data, rows * cols)

    fn __getitem__(self, y: Int, x: Int) -> SIMD[type, 1]:
        return self.load[1](y, x)

    fn __setitem__(inout self, y: Int, x: Int, val: SIMD[type, 1]):
        self.store[1](y, x, val)

    fn load[nelts: Int](self, y: Int, x: Int) -> SIMD[type, nelts]:
        return self.data.simd_load[nelts](y * self.cols + x)

    fn store[nelts: Int](self, y: Int, x: Int, val: SIMD[type, nelts]):
        return self.data.simd_store[nelts](y * self.cols + x, val)

fn matgen(inout a: Matrix):
	let tmp = 1.0 / a.rows / a.cols
	for i in range(a.rows):
		for j in range(a.cols):
			a[i, j] = tmp * (i - j) * (i + j)

# Perform 2D tiling on the iteration space defined by end_x and end_y
fn tile[tiled_fn: Tile2DFunc, tile_x: Int, tile_y: Int](end_x: Int, end_y: Int):
    for y in range(0, end_y, tile_y):
        for x in range(0, end_x, tile_x):
            tiled_fn[tile_x, tile_y](x, y)

# Unroll the vectorized loop by a constant factor
fn matmul_unrolled(inout C: Matrix, A: Matrix, B: Matrix):
    @parameter
    fn calc_row(m: Int):
        @parameter
        fn calc_tile[tile_x: Int, tile_y: Int](x: Int, y: Int):
            @unroll(tile_y)
            for k in range(y, y + tile_y):

                @parameter
                fn dot[nelts: Int](n: Int):
                    C.store(
                        m,
                        n + x,
                        C.load[nelts](m, n + x)
                        + A[m, k] * B.load[nelts](k, n + x),
                    )

                alias unroll_factor = tile_x // nelts
                vectorize_unroll[nelts, tile_x, unroll_factor, dot]()

        tile[calc_tile, tile_n, tile_k](C.cols, B.rows)

    parallelize[calc_row](C.rows, C.rows)

fn main() raises:   
    var a = Matrix[n, n]()
    matgen(a)
    var b = Matrix[n, n]()
    matgen(b)
    var c = Matrix[n, n]()

    @always_inline
    @parameter
    fn test_fn():
        _ = matmul_unrolled(c, a, b)

    let secs = benchmark.run[test_fn](max_runtime_secs=0.5).mean()

    a.data.free()
    b.data.free()
    print(c[n>>1,n>>1])
    c.data.free()
