open! Core
open Matplotlib

let read_file file =
  let lines = In_channel.read_lines file in
  let lines = List.map lines ~f:String.strip in
  String.concat lines
;;

let shift_genome genome change =
  String.sub genome ~pos:change ~len:(String.length genome - change)
  ^ String.sub genome ~pos:0 ~len:change
;;

let () =
  Pyplot.xlabel "Chromosome Index";
  Pyplot.ylabel "Skew";
  Pyplot.plot
    (Problems.Skew.gen_skews
       (shift_genome (read_file "../data/e-coli.txt") 4500_000));
  Mpl.show ()
;;

(*the e-coli genome is from: https://www.ncbi.nlm.nih.gov/nuccore/NZ_CP027599.1?report=fasta*)
