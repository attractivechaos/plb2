function nq_solve(n)
	a = [ 0 for i=1:n ]
	l = [ 0 for i=1:n ]
	c = [ 0 for i=1:n ]
	r = [ 0 for i=1:n ]
	m = 0
	y0 = (1<<n) - 1
	k = 1
	while k >= 1
		y = (l[k] | c[k] | r[k]) & y0
		if xor(y, y0) >> a[k] != 0
			i = a[k]
			while i < n && (y & (1<<i)) != 0
				i += 1
			end
			if k < n
				z = 1<<i
				a[k] = i + 1
				k += 1
				l[k] = (l[k-1] | z) << 1
				c[k] = c[k-1] | z
				r[k] = (r[k-1] | z) >> 1
			else
				m += 1
				k -= 1
			end
		else
			a[k] = 0
			k -= 1
		end
	end
	return m
end

println(nq_solve(15))
