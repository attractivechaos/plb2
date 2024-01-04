#!/usr/bin/env -S mix run --no-mix-exs
new_tuple = fn
  new_tuple, y, x, cb, acc when x >= 0 ->
    new_tuple.(new_tuple, y, x - 1, cb, [cb.(y, x) | acc])

  _new_tuple, _y, x, _cb, acc when x < 0 ->
    # important note here, this needs to be reversed since the tuple
    # creation counts down instead of up
    List.to_tuple(Enum.reverse(acc))
end

matgen = fn h, w, cb_or_val ->
  List.to_tuple(for y <- 0..(h-1) do
    new_tuple.(new_tuple, y, w-1, cb_or_val, [])
  end)
end

matmul = fn a, ah, aw, b, _bh, bw ->
  Enum.reduce(0..(ah-1), [], fn ay, acc ->
    # So what we're doing here is constructing a single row of the result (c)
    # since we only need to operate on a single row at a time here
    cyr = List.to_tuple(Enum.map(0..(bw - 1), fn _ ->
      0
    end))

    [Enum.reduce(0..(aw-1), cyr, fn ax, cyr ->
      ac = elem(elem(a, ay), ax)
      byr = elem(b, ax)
      Enum.reduce(0..(bw-1), cyr, fn bx, cyr ->
        put_elem(cyr, bx, ac * elem(byr, bx))
      end)
    end) | acc]
  end)
  # We built the matrix in reverse, so we need to reverse the list again
  |> Enum.reverse()
  # and then we turn it into a tuple
  |> List.to_tuple()
end

n = 1500
# n = 400 # for quicker testing
tmp = 1.0 / n / n
init_mat = fn y, x ->
  tmp * (y - x) * (y + x)
end

n = floor(n / 2) * 2
{elapsed, x} = :timer.tc(fn ->
  a = matgen.(n, n, init_mat)
  b = matgen.(n, n, init_mat)
  c = matmul.(a, n, n, b, n, n)

  elem(elem(c, floor(n/2)), floor(n/2))
end)

IO.inspect {elapsed / 1_000_000, x}
