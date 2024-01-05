type mat = float array array

let matgen (n: int) =
  let tmp = 1.0 /. (float_of_int n) /. (float_of_int n) in
  let a = Array.make_matrix n n 0. in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      a.(i).(j) <- tmp *. (float_of_int ((i - j) * (i + j)))
    done
  done;
  a
;;

let matmul (n: int) (a: mat) (b: mat) =
  let c = Array.make_matrix n n 0. in
  for i = 0 to n - 1 do
    let ai = a.(i) in
    let ci = c.(i) in
    for k = 0 to n - 1 do
      let aik = ai.(k) in
      let bk = b.(k) in
      for j = 0 to n - 1 do
        ci.(j) <- ci.(j) +. aik *. bk.(j)
      done
    done
  done;
  c
;; 

let () =
  let n = 1500 in
  let a = matgen n in
  let b = matgen n in
  let c = matmul n a b in
  print_float c.(n/2).(n/2);
  print_newline ()
;;
