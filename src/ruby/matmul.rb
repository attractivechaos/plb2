# frozen_string_literal: true

def matgen(n)
  tmp = 1.0 / n / n
  Array.new(n) { |i|
    Array.new(n) { |j|
      tmp * (i - j) * (i + j)
    }
  }
end

def matmul(a, b)
  m = a.length
  n = a[0].length
  p = b[0].length
  c = Array.new(m) { Array.new(p, 0.0) }
  for i in 0...m
    ci = c[i]
    ai = a[i]
    k = 0
    while k < n
      aik = ai[k]
      bk = b[k]
      j = 0
      while j < p
        ci[j] += aik * bk[j]
        j += 1
      end
      k += 1
    end
  end
  c
end

n = 1500
if ARGV.length > 0
  n = ARGV[0].to_i
end
n = n / 2 * 2
a = matgen(n)
b = matgen(n)
c = matmul(a, b)
puts c[n / 2][n / 2]
