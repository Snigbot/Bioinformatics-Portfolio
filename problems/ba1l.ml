open! Core

(* http://rosalind.info/problems/ba1l/ *)

let nucleotide_to_number nuc =
  match nuc with
  | 'A' -> 0
  | 'C' -> 1
  | 'G' -> 2
  | 'T' -> 3
  | _ -> assert false
;;

let pattern_to_number kmer =
  let rec loop i (n : int) =
    if i = String.length kmer
    then n
    else (
      let num = nucleotide_to_number kmer.[i] in
      loop (i + 1) (num + (4 * n)))
  in
  loop 0 0
;;

let%expect_test "" =
  let kmer = "CCTGTCGCGACCTATAAACACGTTGG" in
  print_s [%sexp (pattern_to_number kmer : int)];
  [%expect {| 1668613530470138 |}]
;;
