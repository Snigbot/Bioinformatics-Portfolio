open! Core

(* http://rosalind.info/problems/ba1m/ *)

let number_to_nucleotide num =
  match num with
  | 0 -> 'A'
  | 1 -> 'C'
  | 2 -> 'G'
  | 3 -> 'T'
  | _ -> assert false
;;

let gen_nucs_in_num_form num k =
  let rec loop k_ num_ list =
    if k_ = 0
    then list
    else (
      let nuc_in_num_form = num_ % 4 in
      loop (k_ - 1) ((num_ - nuc_in_num_form) / 4) (nuc_in_num_form :: list))
  in
  loop k num []
;;

let%expect_test "" =
  print_s [%sexp (gen_nucs_in_num_form 15 2 : int list)];
  [%expect {| (3 3) |}];
  print_s [%sexp (15 % 4 : int)];
  [%expect {| 3 |}]
;;

let number_to_pattern num k =
  let rec loop nucs_in_num_form kmer =
    match nucs_in_num_form with
    | [] -> kmer
    | head :: tail ->
      let nuc = Char.to_string (number_to_nucleotide head) in
      loop tail (kmer ^ nuc)
  in
  loop (gen_nucs_in_num_form num k) ""
;;

let%expect_test "" =
  print_s [%sexp (number_to_pattern 7693 10 : string)];
  [%expect {| AAACTGAATC |}]
;;
