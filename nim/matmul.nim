import sequtils

proc matgen(n: int): seq[seq[float64]] =
  var tmp = 1.0 / float64(n) / float64(n)
  var a = newSeqWith(n, newSeq[float64](n))
  for i in 0..n-1:
    for j in 0..n-1:
      a[i][j] = tmp * float64(i - j) * float64(i + j)
  return a

proc matmul(n: int, a: seq[seq[float64]], b: seq[seq[float64]]): seq[seq[float64]] =
  var c = newSeqWith(n, newSeq[float64](n))
  for i in 0..n-1:
    for k in 0..n-1:
      var aik = a[i][k]
      var bk = b[k]
      for j in 0..n-1:
        c[i][j] += aik * bk[j]
  return c

var n = 1000
var a = matgen(n)
var b = matgen(n)
var c = matmul(n, a, b)
echo c[n shr 1][n shr 1]
