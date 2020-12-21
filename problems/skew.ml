open! Core

let new_skew old_skew nuc =
  match nuc with
  | 'C' -> old_skew -. 1.0
  | 'G' -> old_skew +. 1.0
  | _ -> old_skew
;;

let gen_skews genome =
  let array = Array.create ~len:(String.length genome) 0.0 in
  let rec loop old_skew i =
    if i = String.length genome
    then ()
    else (
      let new_skew = new_skew old_skew genome.[i] in
      array.(i) <- new_skew;
      loop new_skew (i + 1))
  in
  loop 0.0 0;
  array
;;

let%expect_test "" =
  print_s [%sexp (gen_skews "ACGCG" : float array)];
  [%expect {| (0 -1 0 -1 0) |}]
;;
