proc nq_solve(n: int): int =
  const
    NQ_MAX = 31
  var
    a: array[0..NQ_MAX, int]
    l: array[0..NQ_MAX, int]
    c: array[0..NQ_MAX, int]
    r: array[0..NQ_MAX, int]
    m = 0
    y0 = (1 shl n) - 1
  for i in 0 .. n-1:
    a[i] = -1
  var k = 0
  while k >= 0:
    var y = (l[k] or c[k] or r[k]) and y0
    if ((y xor y0) shr (a[k] + 1)) != 0:
      var i = a[k] + 1
      while i < n and (y and (1 shl i)) != 0:
        i += 1
      if k < n - 1:
        var z = 1 shl i
        a[k] = i
        k += 1
        l[k] = (l[k-1] or z) shl 1
        c[k] =  c[k-1] or z
        r[k] = (r[k-1] or z) shr 1
      else:
        m += 1
        k -= 1
    else:
      a[k] = -1
      k -= 1
  return m

var n = 15
echo nq_solve(n)
