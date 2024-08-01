struct Interval{S,T}
	data::T
	st::S
	en::S
	max::S
end

function it_index!(a::Memory{Interval{S,T}}) where {S,T}
	sort!(a, by = x -> x.st)
	last_i = 1
	last::S = 0
	@inbounds for i = 1:2:length(a)
		last, last_i = a[i].en, i;
		a[i] = Interval{S,T}(a[i].data, a[i].st, a[i].en, a[i].en)
	end
	k::Int = 1
	@inbounds while 1<<k <= length(a)
		i0, step = (1<<k), 1<<(k+1)
		for i = i0:step:length(a)
			x = 1<<(k-1)
			max_end = max(a[i].en, a[i-x].max)
			e = if (i + x <= length(a)) a[i+x].max else last end
			max_end = max(max_end, e)
			a[i] = Interval{S,T}(a[i].data, a[i].st, a[i].en, max_end)
		end
		last_i = if ((last_i>>k&1) != 0) last_i - (1<<(k-1)) else last_i + (1<<(k-1)) end
		if (last_i <= length(a)) last = max(last, a[last_i].max) end
		k += 1
	end
end

function it_overlap!(
	a::Memory{Interval{S,T}},
	stack::Memory{NTuple{3, Int}},
	st::S,
	en::S,
	b::Vector{Interval{S,T}}
) where {S,T}
	empty!(b)
	h = 0
	while (1<<h <= length(a)) h += 1 end
	h -= 1
	@inbounds stack[1] = ((1<<h), h, 0)
	stack_len = 1
	@inbounds while stack_len > 0
		x, h, w = stack[stack_len]
		stack_len -= 1
		if h <= 3
			i0 = ((x-1) >> h << h) + 1
			i1 = i0 + (1 << (h+1)) - 2
			i1 = min(i1, length(a))
			i = i0
			while i <= i1 && a[i].st < en
				if (st < a[i].en) push!(b, a[i]) end
				i += 1
			end
		elseif w == 0
			stack[stack_len += 1] = (x, h, 1)
			y = x - (1<<(h-1))
			if y > length(a) || a[y].max > st
				stack[stack_len += 1] = (y, h - 1, 0)
			end
		elseif x <= length(a) && a[x].st < en
			if (st < a[x].en) push!(b, a[x]) end
			stack[stack_len += 1] = (x + (1<<(h-1)), h - 1, 0)
		end
	end
end

mutable struct Splitmix32
	x::UInt32
end

function splitmix32!(rng::Splitmix32)::UInt32
	rng.x += 0x9e3779b9
	z = rng.x
	z = xor(z, z >> 16) * 0x21f0aaad
	z = xor(z, z >> 15) * 0x735a2d97
	z = xor(z, z >> 15)
	return z
end

function gen_intv(n, x::Splitmix32, bit_st, bit_len)
	mask_st  = (1<<bit_st)  - 1
	mask_len = (1<<bit_len) - 1
	a = Memory{Interval{Int64,Int64}}(undef, n)
	for i = 1:n
		s = splitmix32!(x)
		l = splitmix32!(x)
		st = s & mask_st
		en = st + (l & mask_len)
		a[i] = Interval{Int64,Int64}(i, st, en, 0)
	end
	return a, x
end

function main(args)
	bit_st, bit_len = 28, 14
	x = Splitmix32(11)
	n = 1000000
	a1, x  = gen_intv(n, x, bit_st, bit_len)
	a2, x  = gen_intv(n, x, bit_st, bit_len)
	it_index!(a1)
	tot_cov = 0
	b = Vector{Interval{Int64,Int64}}()
	stack = Memory{NTuple{3, Int}}(undef, 64)
	for k = 1:n
		st0, en0 = a2[k].st, a2[k].en
		it_overlap!(a1, stack, st0, en0, b)
		cov_st, cov_en, cov = 0, 0, 0
		for i = 1:length(b)
			st1 = max(b[i].st, st0)
			en1 = min(b[i].en, en0)
			if st1 > cov_en
				cov += cov_en - cov_st
				cov_st, cov_en = st1, en1
			else
				cov_en = max(cov_en, en1)
			end
		end
		cov += cov_en - cov_st
		tot_cov += cov
	end
	println(tot_cov)
end

main(ARGS)
