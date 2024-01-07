local bit = require("bit")

function it_sort_copy(a)
	table.sort(a, function(x,y) return x[1] < y[1] end)
	local b = {}
	for i = 1, #a do
		local t = {}
		for k, v in pairs(a[i]) do
			t[k] = v
		end
		table.insert(b, t)
	end
	return b
end

function it_index(a)
	--table.sort(a, function(x,y) return x[1] < y[1] end)
	local last, last_i
	for i = 1, #a, 2 do
		a[i][3], last, last_i = a[i][2], a[i][2], i
	end
	k = 1
	while bit.lshift(1, k) <= #a do
		local i0, step = bit.lshift(1, k), bit.lshift(1, k + 1)
		for i = i0, #a, step do
			local x = bit.lshift(1, k - 1)
			a[i][3] = a[i][2] > a[i-x][3] and a[i][2] or a[i-x][3]
			local e = i + x <= #a and a[i+x][3] or last
			a[i][3] = a[i][3] > e and a[i][3] or e
		end
		last_i = bit.band(bit.rshift(last_i, k), 1) ~= 0 and last_i + bit.lshift(1, k-1) or last_i - bit.lshift(1, k-1)
		if last_i <= #a then
			last = last > a[last_i][3] and last or a[last_i][3]
		end
		k = k + 1
	end
end

function it_overlap(a, st, en)
	local stack, h0, b = {}, 0, {}
	while bit.lshift(1, h0) <= #a do h0 = h0 + 1 end
	h0 = h0 - 1
	table.insert(stack, {bit.lshift(1, h0), h0, 0})
	while #stack > 0 do
		local z = table.remove(stack)
		local x, h, w = z[1], z[2], z[3]
		if h <= 3 then
			local i0 = bit.lshift(bit.rshift(x - 1, h), h) + 1
			local i1 = i0 + bit.lshift(1, h + 1) - 2
			i1 = i1 < #a and i1 or #a
			for i = i0, i1 do
				if a[i][1] >= en then break end
				if st < a[i][2] then table.insert(b, a[i]) end
			end
		elseif w == 0 then
			table.insert(stack, {x, h, 1})
			local y = x - bit.lshift(1, h - 1)
			if y > #a or a[y][3] > st then
				table.insert(stack, {y, h - 1, 0})
			end
		elseif x <= #a and a[x][1] < en then
			if st < a[x][2] then table.insert(b, a[x]) end
			table.insert(stack, {x + bit.lshift(1, h - 1), h - 1, 0})
		end
	end
	return b
end

function mul32(a, b)
	local ah, al = bit.rshift(a, 16), bit.band(a, 0xffff)
	local bh, bl = bit.rshift(b, 16), bit.band(b, 0xffff)
	local high = bit.band(ah * bl + al * bh, 0xffff) 
	return bit.lshift(high, 16) + al * bl
end

function splitmix32(a)
	return function()
		a = bit.band(a + 0x9e3779b9, 0xffffffff)
		local t = a
		t = bit.bxor(t, bit.rshift(t, 16))
		t = mul32(t, 0x21f0aaad)
		t = bit.bxor(t, bit.rshift(t, 15))
		t = mul32(t, 0x735a2d97);
		t = bit.bxor(t, bit.rshift(t, 15))
		if t < 0 then t = 4294967296 + t end
		return t
	end
end

function gen_intv(n, rng, bit_st, bit_len)
	local mask_st  = bit.lshift(1, bit_st)  - 1
	local mask_len = bit.lshift(1, bit_len) - 1
	local a = {}
	for i = 1, n do
		st = bit.band(rng(), mask_st)
		en = st + bit.band(rng(), mask_len)
		table.insert(a, { st, en, 0, i - 1 })
	end
	return a
end

local n = 1000000
local rng = splitmix32(11)
local bit_st, bit_len = 28, 14
local a1 = it_sort_copy(gen_intv(n, rng, bit_st, bit_len))
local a2 = gen_intv(n, rng, bit_st, bit_len)
it_index(a1)
local tot_cov = 0
for i = 1, #a2 do
	local st0, en0 = a2[i][1], a2[i][2]
	local cov_st, cov_en, cov = 0, 0, 0
	local a = it_overlap(a1, st0, en0)
	for i = 1, #a do
		local st1 = st0 > a[i][1] and st0 or a[i][1]
		local en1 = en0 < a[i][2] and en0 or a[i][2]
		if st1 > cov_en then
			cov = cov + (cov_en - cov_st)
			cov_st, cov_en = st1, en1
		else
			cov_en = cov_en > en1 and cov_en or en1
		end
	end
	cov = cov + (cov_en - cov_st)
	tot_cov = tot_cov + cov
end
print(tot_cov)
