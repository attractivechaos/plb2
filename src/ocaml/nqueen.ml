let nqueen (n: int) =
  let a = Array.make n (-1) in
  let l = Array.make n 0 in
  let c = Array.make n 0 in
  let r = Array.make n 0 in
  let k = ref 0 in
  let m = ref 0 in
  let y0 = (1 lsl n) - 1 in
  while !k >= 0 do
    let y = (l.(!k) lor c.(!k) lor r.(!k)) land y0 in
    if (y lxor y0) asr (a.(!k) + 1) != 0 then begin
      let i = ref (a.(!k) + 1) in
      while !i < n && (y land (1 lsl !i) != 0) do
        incr i
      done;
      if !k < n - 1 then begin
        let z = 1 lsl !i in
        a.(!k) <- !i;
        incr k;
        l.(!k) <- (l.(!k-1) lor z) lsl 1;
        c.(!k) <- c.(!k-1) lor z;
        r.(!k) <- (r.(!k-1) lor z) asr 1
      end else begin
        incr m;
        decr k
      end;
    end else begin
      a.(!k) <- -1;
      decr k
    end
  done;
  !m
;;

let () =
  let m = nqueen 15 in
  print_int m;
  print_newline ()
;;