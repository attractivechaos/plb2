#!/usr/bin/env -S mix run --no-mix-exs
# Implementation of nqueens based on the ruby version using maps
# Is 2x slower the tuples, but provided for reference, in short: don't use maps for this kind of
# thing
import Bitwise

new_map = fn
  _new_map, 0, _val, acc ->
    acc

  new_map, n, val, acc when n > 0 ->
    new_map.(new_map, n - 1, val, Map.put(acc, n - 1, val))
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
      Map.get(l, k)
      |> bor(Map.get(c, k))
      |> bor(Map.get(r, k))
      |> band(y0)

    if bxor(y, y0) >>> (Map.get(a, k) + 1) != 0 do
      i = Map.get(a, k) + 1
      i = inc.(inc, i, n, y)
      if k < n - 1 do
        z = 1<<<i
        a = Map.put(a, k, i)
        k = k + 1
        l = Map.put(l, k, bor(Map.get(l, k-1), z)<<<1)
        c = Map.put(c, k, bor(Map.get(c, k-1), z))
        r = Map.put(r, k, bor(Map.get(r, k-1), z)>>>1)
        nq_solve.(nq_solve, n, a, l, c, r, y0, m, k)
      else
        nq_solve.(nq_solve, n, a, l, c, r, y0, m + 1, k - 1)
      end
    else
      a = Map.put(a, k, -1)
      nq_solve.(nq_solve, n, a, l, c, r, y0, m, k - 1)
    end

  _nq_solve, _n, _a, _l, _c, _r, _y0, m, _k ->
    m
end

nq_solve_start = fn n ->
  a = new_map.(new_map, n, -1, %{})
  l = new_map.(new_map, n, 0, %{})
  c = new_map.(new_map, n, 0, %{})
  r = new_map.(new_map, n, 0, %{})
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
