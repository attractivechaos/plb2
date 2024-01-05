#!/usr/bin/env -S mix run --no-mix-exs
matgen = fn h, w, cb ->
  List.to_tuple(Enum.map(0..(h-1), fn y ->
    List.to_tuple(Enum.map(0..(w-1), fn x ->
      cb.(y, x)
    end))
  end))
end

zipmul = fn
  _zipmul, _ac, [], [], acc ->
    Enum.reverse(acc)

  zipmul, ac, [c | cr], [b | br], acc ->
    zipmul.(zipmul, ac, cr, br, [c + ac * b | acc])
end

matmul = fn a, ah, aw, b, _bh, bw ->
  Enum.reduce(0..(ah-1), [], fn ay, acc ->
    ayr = elem(a, ay)
    # So what we're doing here is constructing a single row of the result (c)
    # since we only need to operate on a single row at a time here
    cyr = Enum.map(0..(bw-1), fn _ ->
      0
    end)

    cyr =
      Enum.reduce(0..(aw-1), cyr, fn ax, cyr ->
        ac = elem(ayr, ax)
        byr = Tuple.to_list(elem(b, ax))
        zipmul.(zipmul, ac, cyr, byr, [])
      end)

    [List.to_tuple(cyr) | acc]
  end)
  # We built the matrix in reverse, so we need to reverse the list again
  |> Enum.reverse()
  # and then we turn it into a tuple
  |> List.to_tuple()
end

n = 1500
#n = 400 # for quicker testing
tmp = 1.0 / n / n
init_mat = fn y, x ->
  tmp * (y - x) * (y + x)
end

n = floor(n / 2) * 2
{_elapsed, x} = :timer.tc(fn ->
  a = matgen.(n, n, init_mat)
  b = matgen.(n, n, init_mat)
  c = matmul.(a, n, n, b, n, n)

  elem(elem(c, floor(n/2)), floor(n/2))
end)

IO.inspect x
