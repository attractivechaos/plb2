# ===----------------------------------------------------------------------=== #
# Copyright (c) 2023, Modular Inc. All rights reserved.
#
# Licensed under the Apache License v2.0 with LLVM Exceptions:
# https://llvm.org/LICENSE.txt
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# ===----------------------------------------------------------------------=== #

from memory import memset_zero

alias type = DType.float64

struct Matrix: # this struct comes from matmul.mojo in mojo/examples
	var data: DTypePointer[type]
	var rows: Int
	var cols: Int

	# Initialize zeroeing all values
	fn __init__(inout self, rows: Int, cols: Int):
		self.data = DTypePointer[type].alloc(rows * cols)
		memset_zero(self.data, rows * cols)
		self.rows = rows
		self.cols = cols

	# Initialize taking a pointer, don't set any elements
	fn __init__(
		inout self, rows: Int, cols: Int, data: DTypePointer[type]
	):
		self.data = data
		self.rows = rows
		self.cols = cols

	fn __getitem__(self, y: Int, x: Int) -> Float64:
		return self.load[1](y, x)

	fn __setitem__(inout self, y: Int, x: Int, val: Float64):
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

fn matmul(inout c: Matrix, a: Matrix, b: Matrix):
	for i in range(a.rows):
		for k in range(b.cols):
			let aik = a[i, k] # hoisting aik is faster
			for j in range(a.cols):
				c[i, j] += aik * b[k, j]

fn main():
	let n = 1000
	var a = Matrix(n, n)
	var b = Matrix(n, n)
	var c = Matrix(n, n)
	matgen(a)
	matgen(b)
	matmul(c, a, b)
	print(c[n>>1,n>>1])
