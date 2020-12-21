open! Core

(* http://rosalind.info/problems/ba1n/ *)

let%expect_test "" =
  Out_channel.write_lines
    "/home/snigbot/output.txt"
    (Ba1i.find_close_kmers "CGCGATGTGCTT" 2)
;;
