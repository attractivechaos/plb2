local bit = require("bit")

function nq_solve(n)
	local a, l, c, r, m = {}, {}, {}, {}, 0
	local y0 = bit.lshift(1, n) - 1
	for i = 1, n do
		a[i], l[i], c[i], r[i] = 0, 0, 0, 0
	end
	local k = 1
	while k >= 1 do
		local y = bit.band(bit.bor(bit.bor(l[k], c[k]), r[k]), y0)
		if bit.rshift(bit.bxor(y, y0), a[k]) ~= 0 then
			local i = a[k]
			while i < n and bit.band(y, bit.lshift(1, i)) ~= 0 do
				i = i + 1
			end
			if k < n then
				local z = bit.lshift(1, i)
				a[k] = i + 1
				k = k + 1
				l[k] = bit.lshift(bit.bor(l[k-1], z), 1)
				c[k] = bit.bor(c[k-1], z)
				r[k] = bit.rshift(bit.bor(r[k-1], z), 1)
			else
				m = m + 1
				k = k - 1
			end
		else
			a[k] = 0
			k = k - 1
		end
	end
	return m
end

local n = 15
if #arg > 0 then
	n = tonumber(arg[1])
end
print(nq_solve(n))
