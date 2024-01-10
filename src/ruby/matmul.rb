def matgen(n)
	tmp = 1.0 / n / n
  	a = Array.new(n) { Array.new(n) { 0 } }
	for i in 0 .. n-1
		for j in 0 .. n-1
			a[i][j] = tmp * (i - j) * (i + j)
		end
	end
	return a
end

def matmul(a, b)
	m = a.length
	n = a[0].length
	p = b[0].length
  	c = Array.new(m) { Array.new(p) { 0 } }
	for i in 0 .. m-1
		ci = c[i]
		for k in 0 .. n-1
			aik = a[i][k]
			bk = b[k]
			for j in 0 .. p-1
				ci[j] += aik * bk[j]
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
c = matmul(a, b)
puts c[n/2][n/2]
