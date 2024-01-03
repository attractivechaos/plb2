#!/usr/bin/env -S mix run --no-mix-exs
# Implementation of nqueens based on the ruby version using tuples
import Bitwise

new_tuple = fn
  _new_tuple, 0, _val, acc ->
    List.to_tuple(acc)

  new_tuple, n, val, acc when n > 0 ->
    new_tuple.(new_tuple, n - 1, val, [val | acc])
end

inc = fn inc, i, n, y ->
  if i < n and band(y, 1<<<i) != 0 do
    inc.(inc, i + 1, n, y)
  else
    i
  end
end

nq_solve = fn
  nq_solve, n, a, l, c, r, y0, m, k when k >= 0 ->
    y =
      elem(l, k)
      |> bor(elem(c, k))
      |> bor(elem(r, k))
      |> band(y0)

    if bxor(y, y0) >>> (elem(a, k) + 1) != 0 do
      i = elem(a, k) + 1
      i = inc.(inc, i, n, y)
      if k < n - 1 do
        z = 1<<<i
        a = put_elem(a, k, i)
        k = k + 1
        l = put_elem(l, k, bor(elem(l, k-1), z)<<<1)
        c = put_elem(c, k, bor(elem(c, k-1), z))
        r = put_elem(r, k, bor(elem(r, k-1), z)>>>1)
        nq_solve.(nq_solve, n, a, l, c, r, y0, m, k)
      else
        nq_solve.(nq_solve, n, a, l, c, r, y0, m + 1, k - 1)
      end
    else
      a = put_elem(a, k, -1)
      nq_solve.(nq_solve, n, a, l, c, r, y0, m, k - 1)
    end

  _nq_solve, _n, _a, _l, _c, _r, _y0, m, _k ->
    m
end

nq_solve_start = fn n ->
  a = new_tuple.(new_tuple, n, -1, [])
  l = new_tuple.(new_tuple, n, 0, [])
  c = new_tuple.(new_tuple, n, 0, [])
  r = new_tuple.(new_tuple, n, 0, [])
  y0 = (1<<<n) - 1
  m = 0
  k = 0

  nq_solve.(nq_solve, n, a, l, c, r, y0, m, k)
end

n = 15
{elapsed, m} = :timer.tc(fn ->
  nq_solve_start.(n)
end)

# tc is in microseconds by default
IO.inspect {elapsed / 1_000_000, m}
