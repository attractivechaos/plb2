def matgen(n)
  tmp = 1.0 / n / n
  return Array.new(n * n) do |i|
    k = i / n
    j = i % n
    tmp * (k - j) * (k + j)
  end
end

def matmul(a, ah, aw, b, _bh, bw)
  c = Array.new(ah * bw, 0)
  (0...ah).each do |ay|
    # a-y-offset
    ayo = ay * aw
    cyo = ay * bw
    (0...aw).each do |ax|
      # a-cell
      ac = a[ayo + ax]
      # b-y-offset
      byo = ax * bw
      (0...bw).each do |bx|
        c[cyo + bx] += ac * b[byo + bx]
      end
    end
  end
  return c
end

n = 1500
if ARGV.length >= 1
  n = ARGV[0].to_i
end
n = n / 2 * 2
a = matgen(n)
b = matgen(n)
c = matmul(a, n, n, b, n, n)
puts c[n/2 * n + n/2]
