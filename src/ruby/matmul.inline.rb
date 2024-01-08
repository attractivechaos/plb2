def matgen(n)
  tmp = 1.0 / n / n
  # initializes the array inline
  return Array.new(n) do |i|
    Array.new(n) do |j|
      tmp * (i - j) * (i + j)
    end
  end
end

def matmul(a, b)
  m = a.length
  n = a[0].length
  p = b[0].length
  return Array.new(m) do |i|
    ai = a[i]
    Array.new(p) do |j|
      acc = 0
      (0...n).each do |k|
        # the column access here is killing it
        acc += ai[k] * b[k][j]
      end
      acc
    end
  end
end

n = 1500
if ARGV.length >= 1
  n = ARGV[0].to_i
end
n = n / 2 * 2
a = matgen(n)
b = matgen(n)
c = matmul(a, b)
puts c[n/2][n/2]
