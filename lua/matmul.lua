function matgen(n)
	local tmp = 1.0 / n / n
	local a = {}
	for i = 1, n do
		a[i] = {}
		for j = 1, n do
			a[i][j] = tmp * (i - j) * (i + j - 2)
		end
	end
	return a
end

function matmul(n, a, b)
	local c = {}
	for i = 1, n do
		local ci = {}
		for j = 1, n do ci[j] = 0 end
		for k = 1, n do
			local t, bk = a[i][k], b[k]
			for j = 1, n do
				ci[j] = ci[j] + t * bk[j]
			end
		end
		c[i] = ci
	end
	return c
end

local n = tonumber(arg[1]) or 1000
local a = matgen(n)
local b = matgen(n)
local c = matmul(n, a, b)
print(c[math.floor(n/2)+1][math.floor(n/2)+1]);
