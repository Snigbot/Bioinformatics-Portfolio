open! Core

(* http://rosalind.info/problems/ba1i/ *)

let gen_tuples min max len =
  let rec loop len_ =
    if len_ = 0
    then [ [] ]
    else (
      let (shorter : int list list) = loop (len_ - 1) in
      let rec loop2 i list x =
        if i = max
        then List.append [ i :: x ] list
        else loop2 (i + 1) (List.append [ i :: x ] list) x
      in
      List.concat (List.map shorter ~f:(fun x -> loop2 min [] x)))
  in
  loop len
;;

let rec gen_choices n k =
  if k = 0
  then [ [] ]
  else if n = 0
  then []
  else (
    let last_chosen =
      List.map
        (gen_choices (n - 1) (k - 1))
        ~f:(fun choices -> (n - 1) :: choices)
    in
    let last_not_chosen = gen_choices (n - 1) k in
    List.append last_chosen last_not_chosen)
;;

let modify_kmer_h kmer_ position change =
  let nuc_to_num x =
    match x with
    | 'A' -> 0
    | 'T' -> 1
    | 'G' -> 2
    | 'C' -> 3
    | _ -> assert false
  in
  let num_to_nuc n =
    match n % 4 with
    | 0 -> 'A'
    | 1 -> 'T'
    | 2 -> 'G'
    | 3 -> 'C'
    | _ -> assert false
  in
  Bytes.set
    kmer_
    position
    (num_to_nuc (nuc_to_num (Bytes.get kmer_ position) + change))
;;

let modify_kmer kmer positions (offset : int list) =
  let kmer_ = Bytes.of_string kmer in
  let rec loop pos offset_ =
    match pos with
    | [] -> ()
    | head :: tail ->
      modify_kmer_h kmer_ head (List.hd_exn offset_);
      loop tail (List.tl_exn offset_)
  in
  loop positions offset;
  Bytes.to_string kmer_
;;

let find_close_kmers kmer d =
  let rec loop kmers dist =
    if dist = 0
    then kmer :: kmers
    else (
      let positions = gen_choices (String.length kmer) dist in
      let offsets = gen_tuples 1 3 dist in
      loop
        (List.append
           (List.fold offsets ~init:[] ~f:(fun accum offset ->
                List.append
                  (List.fold positions ~init:[] ~f:(fun acc pos ->
                       modify_kmer kmer pos offset :: acc))
                  accum))
           kmers)
        (dist - 1))
  in
  loop [] d
;;

let tally kmer map d =
  let kmers = find_close_kmers kmer d in
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

let most_frequent map =
  Map.fold
    map
    ~init:(0, [])
    ~f:(fun ~key:kmer ~data:count (old_count, old_kmers) ->
      if count > old_count
      then count, [ kmer ]
      else if count = old_count
      then old_count, kmer :: old_kmers
      else old_count, old_kmers)
;;

let main text k d =
  let rec loop i map =
    if i > String.length text - k
    then map
    else loop (i + 1) (tally (String.sub ~pos:i ~len:k text) map d)
  in
  snd (most_frequent (loop 0 (Map.empty (module String))))
;;

let map_to_list map =
  Map.fold map ~init:[] ~f:(fun ~key:kmer ~data:count list ->
      List.append [ kmer, count ] list)
;;

let%expect_test "" =
  print_s [%sexp (gen_choices 2 2 : int list list)];
  [%expect {|
    ((1 0)) |}];
  print_s [%sexp (gen_choices 5 2 : int list list)];
  [%expect {| ((4 3) (4 2) (4 1) (4 0) (3 2) (3 1) (3 0) (2 1) (2 0) (1 0)) |}];
  print_s
    [%sexp
      (main
         "GCCCAGGAGGCCCAGGAGTATACGATATTTAGTAAGGCCCAGGAGTTAGTAAGTTAGTAAGTATACGATATTTAGTAAGATACTACCTTAGTAAGTTAGTAAGTTAGTAAGTTAGTAAGTATACGATATTTAGTAAGTTAGTAAGGCCCAGGAGTTAGTAAGTTAGTAAGGCCCAGGAGTATCTCCTAATACTACCGCCCAGGAGTATACGATATTATCTCCTATATCTCCTATATACGATATTATACGATATATACTACCTTAGTAAGTTAGTAAGTATACGATATTATCTCCTATTAGTAAGTATACGATATTATACGATATTATACGATATATACTACCTTAGTAAGATACTACCTTAGTAAGGCCCAGGAGATACTACCTATCTCCTATATACGATATTATACGATATTTAGTAAGTATCTCCTATATCTCCTATTAGTAAGGCCCAGGAGGCCCAGGAGATACTACCTATACGATATTATCTCCTAATACTACCTTAGTAAGGCCCAGGAGTATCTCCTATATACGATATTTAGTAAGTATCTCCTATATCTCCTAATACTACCTTAGTAAGTATCTCCTAGCCCAGGAGTTAGTAAGGCCCAGGAGTATACGATATGCCCAGGAGTATACGATATTATCTCCTATTAGTAAGTATACGATATGCCCAGGAGTTAGTAAGTTAGTAAGTATACGATATGCCCAGGAGATACTACCTATCTCCTATATCTCCTATATCTCCTAGCCCAGGAGTTAGTAAGGCCCAGGAGTATCTCCTAATACTACCTTAGTAAGATACTACCTTAGTAAGGCCCAGGAGTATCTCCTATTAGTAAGTATACGATATTATCTCCTAGCCCAGGAGTATACGATATTTAGTAAGTATACGATATTATACGATATGCCCAGGAGTATCTCCTAGCCCAGGAGTATCTCCTATATACGATAT"
         12
         1
        : string list)];
  [%expect {|
      (AATATACGATAT) |}]
;;
