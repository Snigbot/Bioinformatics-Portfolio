open! Core

(* http://rosalind.info/problems/ba1j/ *)

let gen_d_dist_kmers_h positions offset kmer =
  List.fold positions ~init:[] ~f:(fun acc pos ->
      Ba1i.modify_kmer kmer pos offset :: acc)
;;

let gen_d_dist_kmers kmer d =
  let positions = Ba1i.gen_choices (String.length kmer) d in
  let offsets = Ba1i.gen_tuples 1 3 d in
  List.fold offsets ~init:[] ~f:(fun accum offset ->
      List.append (gen_d_dist_kmers_h positions offset kmer) accum)
;;

let%expect_test "" =
  print_s [%sexp (gen_d_dist_kmers "AA" 1 : string list)];
  [%expect {| (TA AT GA AG CA AC) |}]
;;

let find_close_kmers kmer d =
  let rec loop kmers current_d =
    if current_d = 0
    then kmer :: kmers
    else (
      let close_kmers = List.append (gen_d_dist_kmers kmer current_d) kmers in
      loop close_kmers (current_d - 1))
  in
  loop [] d
;;

let%expect_test "" =
  print_s [%sexp (find_close_kmers "AA" 1 : string list)];
  [%expect {| (AA TA AT GA AG CA AC) |}]
;;

let find_close_kmers_with_rev_comps kmer d =
  let close_kmers = find_close_kmers kmer d in
  let rev_comps_of_close_kmers =
    List.map close_kmers ~f:(fun kmer -> Ba1c.rev_comp kmer)
  in
  List.append close_kmers rev_comps_of_close_kmers
;;

let%expect_test "" =
  let kmer = "AC" in
  let d = 1 in
  let close_kmers = find_close_kmers kmer d in
  print_s [%sexp (find_close_kmers_with_rev_comps kmer d : string list)];
  [%expect {| (AC TC AA GC AT CC AG GT GA TT GC AT GG CT) |}];
  print_s [%sexp (close_kmers : string list)];
  print_s
    [%sexp
      (List.map close_kmers ~f:(fun kmer -> Ba1c.rev_comp kmer) : string list)];
  [%expect {|
    (AC TC AA GC AT CC AG)
    (GT GA TT GC AT GG CT) |}]
;;

let tally kmer map d =
  let kmers = find_close_kmers_with_rev_comps kmer d in
  let rec loop kmers_ map =
    match kmers_ with
    | [] -> map
    | head :: tail ->
      let r = Map.find map head in
      (match r with
      | None -> loop tail (Map.set map ~key:head ~data:1)
      | Some x -> loop tail (Map.set map ~key:head ~data:(x + 1)))
  in
  loop kmers map
;;

let%expect_test "" =
  let empty_map = Map.empty (module String) in
  let kmer = "AA" in
  let kmer2 = "AC" in
  let d = 1 in
  let tallied_map = tally kmer empty_map d in
  let twice_tallied_map = tally kmer2 tallied_map d in
  print_s [%sexp (Ba1i.map_to_list twice_tallied_map : (string * int) list)];
  [%expect
    {|
    ((TT 2) (TG 1) (TC 2) (TA 2) (GT 2) (GG 1) (GC 2) (GA 2) (CT 2) (CC 1)
     (CA 1) (AT 4) (AG 2) (AC 2) (AA 2)) |}]
;;

let main text k d =
  let rec loop i map =
    if i > String.length text - k
    then map
    else loop (i + 1) (tally (String.sub ~pos:i ~len:k text) map d)
  in
  snd (Ba1i.most_frequent (loop 0 (Map.empty (module String))))
;;

(*snd (Ba1i.most_frequent ( *)
let%expect_test "" =
  print_s
    [%sexp
      (main
         "TATGGCTCCCGAACCGTCGTATGGCTCCCGGGCTGACCTATGGCTCCCGGGCTGACCGGGCTGACCGAACCGTCGCAGAGCGTACATCATAACACATCATAACTATGGCTCCCACATCATAACTATGGCTCCCTATGGCTCCCGAACCGTCGGGGCTGACCTATGGCTCCCACATCATAACCAGAGCGTCAGAGCGTGAACCGTCGGAACCGTCGCAGAGCGTCAGAGCGTACATCATAACGGGCTGACCGAACCGTCGACATCATAACTATGGCTCCCGAACCGTCGACATCATAACGAACCGTCGTATGGCTCCCTATGGCTCCCCAGAGCGTTATGGCTCCCTATGGCTCCCCAGAGCGTACATCATAACTATGGCTCCCCAGAGCGTTATGGCTCCCCAGAGCGTACATCATAACACATCATAACACATCATAACACATCATAACCAGAGCGTACATCATAACTATGGCTCCCACATCATAACTATGGCTCCCCAGAGCGTTATGGCTCCCCAGAGCGTACATCATAACCAGAGCGTTATGGCTCCCCAGAGCGTACATCATAACGAACCGTCGGAACCGTCGCAGAGCGTCAGAGCGTCAGAGCGTGGGCTGACCGAACCGTCGTATGGCTCCCCAGAGCGTACATCATAACCAGAGCGTCAGAGCGTACATCATAACTATGGCTCCCCAGAGCGTGGGCTGACCTATGGCTCCCGGGCTGACCCAGAGCGTTATGGCTCCCGGGCTGACCGAACCGTCGACATCATAACGGGCTGACCTATGGCTCCCACATCATAACGAACCGTCGGAACCGTCGCAGAGCGTACATCATAACACATCATAACCAGAGCGTGGGCTGACCGAACCGTCGGGGCTGACCCAGAGCGTTATGGCTCCCGAACCGTCGTATGGCTCCCGGGCTGACCACATCATAACTATGGCTCCCACATCATAACCAGAGCGTTATGGCTCCC"
         6
         3
        : string list)];
  [%expect {|
    (GGGCCG CGGCCC) |}];
  print_s [%sexp (Ba1c.rev_comp "AA" : string)];
  [%expect {| TT |}]
;;
