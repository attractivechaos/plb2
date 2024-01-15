def splitmix32(x)
	x = (x + 0x9e3779b9) & 0xffffffff
	z = x
	z = (z ^ (z >> 16)) * 0x21f0aaad & 0xffffffff
	z = (z ^ (z >> 15)) * 0x735a2d97 & 0xffffffff
	return z ^ (z >> 15), x
end

Interval = Struct.new(:st, :en, :max, :data) do
end

def gen_intv(n, x, bit_st, bit_len)
	a = Array.new(n)
	mask_st = (1<<bit_st) - 1
	mask_len = (1<<bit_len) - 1
	for i in 0 .. n-1 do
		r, x = splitmix32(x)
		st = r & mask_st
		r, x = splitmix32(x)
		en = st + (r & mask_len)
		a[i] = Interval.new(st, en, i, 0)
	end
	return a, x
end

def iit_index(a)
	if a.size == 0
		return
	end
	a.sort_by!{|x| x.st}
	last, last_i, i = a[0].en, 1, 0
	while i < a.size
		last, last_i = a[i].en, i
		a[i] = Interval.new(a[i].st, a[i].en, a[i].en, a[i].data)
		i += 2
	end
	k = 1
	while 1<<k <= a.size
		i0, step = (1<<k) - 1, 1<<(k+1)
		i = i0
		while i < a.size
			x = 1 << (k - 1)
			max = a[i].en > a[i-x].max ? a[i].en : a[i-x].max
			e = i + x < a.size ? a[i+x].max : last
			max = e if max < e
			a[i] = Interval.new(a[i].st, a[i].en, max, a[i].data)
			i += step
		end
		last_i = (last_i>>k&1) != 0 ? last_i - (1<<(k-1)) : last_i + (1<<(k-1))
		if last_i < a.size
			last = last > a[last_i].max ? last : a[last_i].max
		end
		k += 1
	end
end

def iit_overlap(a, st, en)
	h = 0
	while 1<<h <= a.size
		h += 1
	end
	h -= 1
	stack, n = Array.new(64), 0
	stack[n], n = [(1<<h)-1, h, 0], n + 1
	while n > 0
		n -= 1
		(x, h, w) = stack[n]
		if h <= 3
			i0 = x >> h << h
			i1 = i0 + (1<<(h+1)) - 1
			i1 = a.size if i1 >= a.size
			i = i0
			while i < i1 && a[i].st < en
				yield a[i] if st < a[i].en
				i += 1
			end
		elsif w == 0
			stack[n], n = [x, h, 1], n + 1
			y = x - (1<<(h-1))
			if y >= a.size || a[y].max > st
				stack[n], n = [y, h - 1, 0], n + 1
			end
		elsif x < a.size && a[x].st < en
			yield a[x] if st < a[x].en
			stack[n], n = [x + (1<<(h-1)), h - 1, 0], n + 1
		end
	end
end

n = 1000000
x, bit_st, bit_len = 11, 28, 14
a1, x = gen_intv(n, x, bit_st, bit_len)
a2, x = gen_intv(n, x, bit_st, bit_len)
iit_index(a1)
tot_cov = 0
for i in 0 .. a2.size-1
	st0, en0 = a2[i].st, a2[i].en
	cov_st, cov_en, cov = 0, 0, 0
	iit_overlap(a1, st0, en0) do |x|
		st1 = x.st > st0 ? x.st : st0
		en1 = x.en < en0 ? x.en : en0
		if st1 > cov_en
			cov += cov_en - cov_st
			cov_st, cov_en = st1, en1
		else
			cov_en = en1 if cov_en < en1
		end
	end
	cov += cov_en - cov_st
	tot_cov += cov
end
puts tot_cov
