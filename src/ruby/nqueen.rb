# frozen_string_literal: true

def nq_solve(n)
  a = Array.new(n, -1)
  l = Array.new(n, 0)
  c = Array.new(n, 0)
  r = Array.new(n, 0)
  y0 = (1 << n) - 1
  m = 0
  k = 0

  while k >= 0
    y = (l[k] | c[k] | r[k]) & y0
    if (y ^ y0) >> (a[k] + 1) != 0
      i = a[k] + 1
      while i < n && (y & 1 << i) != 0
        i += 1
      end
      if k < n - 1
        z = 1 << i
        a[k] = i
        k += 1
        l[k] = (l[k-1] | z) << 1
        c[k] = c[k-1] | z
        r[k] = (r[k-1] | z) >> 1
      else
        m += 1
        k -= 1
      end
    else
      a[k] = -1
      k -= 1
    end
  end

  m
end

n = 15
if ARGV.length > 0
  n = ARGV[0].to_i
end

puts nq_solve(n)
