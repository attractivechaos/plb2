type
  CArray*[T] = object
    len: int
    data: ptr UncheckedArray[T]

proc `=destroy`*[T](x: CArray[T]) =
  if x.data != nil:
    for i in 0..<x.len: `=destroy`(x.data[i])
    dealloc(x.data)
    x.data = nil

proc `[]=`*[T](arr: CArray[T], i: int, x: T) =
  arr.data[i] = x

proc `[]`*[T](arr: CArray[T], i: int): T =
  return arr.data[i]

proc newArray*[T](size: int): CArray[T] =
  result.len = size
  result.data = cast[ptr UncheckedArray[T]](alloc(size * sizeof(T)))

proc nq_solve(n: int): int =
  var
    a = newArray[int](n)
    l = newArray[int](n)
    c = newArray[int](n)
    r = newArray[int](n)
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
        c[k] = c[k-1] or z
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
