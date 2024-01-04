type
  Matrix* = ptr UncheckedArray[ptr UncheckedArray[float64]]

proc allocMat(n: int): Matrix =
  let rows = cast[ptr UncheckedArray[ptr UncheckedArray[float64]]](alloc(n *
      sizeof(ptr UncheckedArray[float64])))
  for i in 0..<n:
    rows[i] = cast[ptr UncheckedArray[float64]](alloc(n * sizeof(float64)))
  return rows

proc deallocMat(mat: Matrix, n: int) =
  for i in 0..<n:
    for j in 0..<n:
      `=destroy`(mat[i][j])
    dealloc(mat[i])
  dealloc(mat)
  mat = nil

proc matgen(n: int): Matrix =
  var tmp = 1.0 / float64(n) / float64(n)
  var a = allocMat(n)
  for i in 0..n-1:
    var ai = a[i]
    for j in 0..n-1:
      ai[j] = tmp * float64(i - j) * float64(i + j)
  return a

proc matmul(n: int, a: Matrix, b: Matrix): Matrix =
  var c = allocMat(n)
  for i in 0..n-1:
    for k in 0..n-1:
      var aik = a[i][k]
      var bk = b[k]
      var ci = c[i]
      for j in 0..n-1:
        ci[j] += aik * bk[j]
  return c

var n = 1500
var a = matgen(n)
var b = matgen(n)
var c = matmul(n, a, b)
echo c[n shr 1][n shr 1]
deallocMat(a, n)
deallocMat(b, n)
deallocMat(c, n)
