open! Core

(* http://rosalind.info/problems/ba1f/ *)

let new_skew old_skew n =
  match n with
  | 'C' -> old_skew - 1
  | 'G' -> old_skew + 1
  | _ -> old_skew
;;

let main genome =
  let rec loop skew (min_skew, positions) i =
    if String.length genome = i + 1
    then List.rev positions
    else (
      let new_skew = new_skew skew genome.[i] in
      if new_skew > min_skew
      then loop new_skew (min_skew, positions) (i + 1)
      else if new_skew = min_skew
      then loop new_skew (min_skew, (i + 1) :: positions) (i + 1)
      else loop new_skew (new_skew, [ i + 1 ]) (i + 1))
  in
  loop 0 (2, []) 0
;;

let%expect_test "" =
  print_s [%sexp (main "AGCTAGTCAGTC" : int list)];
  [%expect {| (1 3 4 5 8 9) |}]
;;
