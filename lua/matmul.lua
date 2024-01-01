function matgen(n)
	local tmp = 1.0 / n / n
	local a = {}
	for i = 0, n-1 do
		a[i] = {}
		for j = 0, n-1 do
			a[i][j] = tmp * (i - j) * (i + j)
		end
	end
	return a
end

function matmul(n, a, b)
	local c = {}
	for i = 0, n-1 do
		c[i] = {}
		for j = 0, n-1 do c[i][j] = 0 end
		for k = 0, n-1 do
			for j = 0, n-1 do
				c[i][j] = c[i][j] + a[i][k] * b[k][j]
			end
		end
	end
	return c
end

local n = 1000
local a = matgen(n)
local b = matgen(n)
local c = matmul(n, a, b)
print(c[math.floor(n/2)][math.floor(n/2)]);
