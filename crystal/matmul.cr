def matgen(n)
	a = Array.new(n) { Array.new(n) { 0_f64 } }
	tmp = 1.0 / n / n
	(0..n-1).each do |i|
		(0..n-1).each do |j|
			a[i][j] = tmp * (i - j) * (i + j)
		end
	end
	return a
end

def matmul(n, a, b)
	c = Array.new(n) { Array.new(n) { 0_f64 } }
	(0..n-1).each do |i|
		ci = c[i]
		(0..n-1).each do |k|
			t = a[i][k]
			bk = b[k]
			(0..n-1).each do |j|
				ci[j] += t * bk[j]
			end
		end
	end
	return c
end

n = 1000
a = matgen(n)
b = matgen(n)
c = matmul(n, a, b)
puts c[n>>1][n>>1]
