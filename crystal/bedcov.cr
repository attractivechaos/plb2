module IITree(SType, DType)
	struct Interval(SType, DType)
		property st, en, max, data
		def initialize(@st : SType, @en : SType, @max : SType, @data : DType)
		end
	end

	def self.index(a : Array(Interval(SType, DType)))
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
	
	def self.overlap(a : Array(Interval(SType, DType)), st : SType, en : SType)
		h = 0
		while 1<<h <= a.size
			h += 1
		end
		h -= 1
		stack, n = StaticArray(Tuple(Int32, Int32, Int32), 64).new({0,0,0}), 0
		stack[n], n = { (1<<h)-1, h, 0 }, n + 1
		while n > 0
			n -= 1
			x, h, w = stack[n]
			if h <= 3
				i0 = x >> h << h
				i1 = i0 + (1<<(h+1)) - 1
				i1 = a.size if i1 >= a.size
				i = i0
				while i < i1 && a.unsafe_fetch(i).st < en
					yield a.unsafe_fetch(i) if st < a.unsafe_fetch(i).en
					i += 1
				end
			elsif w == 0
				stack[n], n = { x, h, 1 }, n + 1
				y = x - (1<<(h-1))
				if y >= a.size || a.unsafe_fetch(y).max > st
					stack[n], n = { y, h - 1, 0 }, n + 1
				end
			elsif x < a.size && a.unsafe_fetch(x).st < en
				yield a.unsafe_fetch(x) if st < a.unsafe_fetch(x).en
				stack[n], n = { x + (1<<(h-1)), h - 1, 0 }, n + 1
			end
		end
	end
end # module IITree

def splitmix32(x : UInt32)
	x = x &+ 0x9e3779b9
	z = x
	z = (z ^ (z >> 16)) &* 0x21f0aaad
	z = (z ^ (z >> 15)) &* 0x735a2d97
	z = (z ^ (z >> 15))
	return z, x
end

def gen_intv(n, x : UInt32, bit_st, bit_len)
	mask_st  = (1<<bit_st)  - 1
	mask_len = (1<<bit_len) - 1
	a = Array(IITree::Interval(Int64, Int64)).new
	(0...n).each do |i|
		s, x = splitmix32(x)
		l, x = splitmix32(x)
		st = (s & mask_st).to_i64
		a.push(IITree::Interval.new(st, st + (l & mask_len).to_i64, 0_i64, i.to_i64))
	end
	return a, x
end

x = 11_u32
n, bit_st, bit_len = 500000, 28, 14
a1, x = gen_intv(n, x, bit_st, bit_len)
a2, x = gen_intv(n, x, bit_st, bit_len)
IITree.index(a1)

tot_cov = 0_i64
(0 ... a2.size).each do |i|
	st0, en0 = a2[i].st, a2[i].en
	cov_st, cov_en, cov = 0_i64, 0_i64, 0_i64
	IITree.overlap(a1, st0, en0) do |x|
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
