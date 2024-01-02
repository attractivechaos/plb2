import std/algorithm

type
  Interval*[S,T] = tuple[st, en: S, data: T, max: S]

proc intvcmp[S,T](x, y: Interval[S,T]): int =
  cmp(x.st, y.st)

proc index*[S,T](a: var seq[Interval[S,T]]): int {.discardable.} =
  if a.len == 0: return 0
  var is_srt = true
  for i in 1..<a.len:
    if a[i-1].st > a[i].st:
      is_srt = false; break
  if not is_srt: a.sort(intvcmp)
  var last_i: int
  var last: S
  for i in countup(0, a.len-1, 2): # leaves (i.e. at level 0)
    (last_i, last, a[i].max) = (i, a[i].en, a[i].en)
  var k = 1
  while 1 shl k <= a.len: # process internal nodes in the bottom-up order
    let x = 1 shl (k - 1)
    let i0 = (x shl 1) - 1 # the first node at level k
    let step = x shl 2
    for i in countup(i0, a.len - 1, step): # traverse nodes at level k
      let el = a[i - x].max  # max value of the left child
      let er = if i + x < a.len: a[i + x].max else: last # of the right child
      var e = a[i].en
      if e < el: e = el
      if e < er: e = er
      a[i].max = e
    # point last_i to the parent of the original last_i
    last_i = if ((last_i shr k) and 1) != 0: last_i - x else: last_i + x
    if last_i < a.len and a[last_i].max > last: # update last accordingly
      last = a[last_i].max
    k += 1
  return k - 1

iterator overlap*[S,T](a: seq[Interval[S,T]], st: S, en: S): Interval[S,T] {.noSideEffect.} =
  var h: int = 0
  while 1 shl h <= a.len: h += 1
  h -= 1 # h is the height of the tree
  var stack: array[64, tuple[k, x, w:int]] # 64 is the max possible tree height
  var t: int = 0
  stack[t] = (h, (1 shl h) - 1, 0); t += 1 # push the root
  while t > 0: # the following guarantees sorted "yield"
    t -= 1
    let (k, x, w) = stack[t] # pop from the stack
    if k <= 3: # in a small subtree, traverse everything
      let i0 = (x shr k) shl k
      var i1 = i0 + (1 shl (k + 1)) - 1
      if i1 >= a.len: i1 = a.len
      for i in countup(i0, i1 - 1):
        if a[i].st >= en: break  # out of range; no need to proceed
        if st < a[i].en: yield a[i] # overlap! yield
    elif w == 0: # the left child not processed
      let y = x - (1 shl (k - 1)) # the left child of z.x; y may >=a.len
      stack[t] = (k, x, 1); t += 1
      if y >= a.len or a[y].max > st:
        stack[t] = (k-1, y, 0); t += 1 # add left child
    elif x < a.len and a[x].st < en: # need to push the right child
      if st < a[x].en: yield a[x] # test if x overlaps the query
      stack[t] = (k - 1, x + (1 shl (k - 1)), 0); t += 1

type
  IntervalList64 = seq[Interval[int64,int64]]

type
  Splitmix32 = object
    x*: uint32

proc splitmix32(rng: var Splitmix32): uint32 =
  rng.x = rng.x + 0x9e3779b9'u32
  var z = rng.x
  z = (z xor (z shr 16)) * 0x21f0aaad'u32
  z = (z xor (z shr 15)) * 0x735a2d97'u32
  return z xor (z shr 15)

proc gen_intv(n: int64, rng: var Splitmix32, bit_st: int64, bit_len: int64): IntervalList64 =
  result = @[]
  let mask_st  = (1'i64 shl bit_st)  - 1
  let mask_len = (1'i64 shl bit_len) - 1
  for i in 0..<n:
    let st: int64 = int64(splitmix32(rng)) and mask_st
    let en: int64 = st + (int64(splitmix32(rng)) and mask_len)
    result.add((st, en, int64(i), 0'i64))

var rng = Splitmix32(x:11)
var n = 1000000'i64
var bit_st = 28
var bit_len = 14
var a1 = gen_intv(n, rng, bit_st, bit_len)
var a2 = gen_intv(n, rng, bit_st, bit_len)
a1.index()
var tot_cov: int64 = 0
for i in 0..<n:
  let st0 = a2[i].st
  let en0 = a2[i].en
  var cov_st, cov_en, cov, cnt: int64
  for x in a1.overlap(st0, en0):
    let st1 = if x.st > st0: x.st else: st0
    let en1 = if x.en < en0: x.en else: en0
    if st1 > cov_en:
      cov += cov_en - cov_st
      (cov_st, cov_en) = (st1, en1)
    else:
      cov_en = if cov_en > en1: cov_en else: en1
  cov += cov_en - cov_st
  tot_cov += cov
echo tot_cov
