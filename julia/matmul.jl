function matgen(n)
	a = Array{Float64}(undef, n, n)
	tmp = 1.0 / n / n
	for i = 1:n, j = 1:n
		a[i,j] = tmp * (i - j) * (i + j - 2)
	end
	return a
end

function matmul(n, a, b)
	c = zeros(Float64, n, n)
	for i = 1:n
		for k = 1:n
			aik = a[i,k]
			for j = 1:n
				c[i,j] += aik * b[k,j]
			end
		end
	end
	return c
end

n = 1500
a = matgen(n)
b = matgen(n)
c = matmul(n, a, b)
println(c[(n>>1)+1,(n>>1)+1])
