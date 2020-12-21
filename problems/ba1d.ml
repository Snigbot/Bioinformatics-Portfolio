open! Core

(* http://rosalind.info/problems/ba1d/ *)

let check list text pattern i =
  if String.equal (String.sub text ~pos:i ~len:(String.length pattern)) pattern
     (*if pattern = the k-mer in text where k is the length of pattern and it starts at i*)
  then i :: list (*then add i to list*)
  else list
;;

let main text pattern =
  let rec loop list i =
    if i > String.length text - String.length pattern - 1
    then List.rev list
    else loop (check list text pattern i) (i + 1)
  in
  loop [] 0
;;

let%expect_test "" =
  let result =
    main
      "TACTCGCGTCTCTCCAACTCTCCACTCTCCAAGCTCTCCACCGCGTCCTCTCCAGTCTCTCCACTCTCCACTCTCCACGCAGTGGCTGGCTCTCCATATTGTTGACTAAAGAAATCTCTCCACCCAAACCCCACTCCTCTCCACTCAACTCTCCAGCTCTCCACTTTTGCTCTCCATACCTCTCCAGCCTCTGTATTGGGCTCTCCAAGCTCTCCAACTCTCCAGAAACTCTCCAAACTCTCCACCATCTCTCCAACGACCTCTCCAGGTGCGATCTCTCCACTCTCCAGTAGCTCTCCACTATTCTCTCCAATCCTCTCTCCACCCTGCCTCTCCACTCTCCATTCAAATTTATGGAATCCTCTCCACGTTGTTGCTCTCCACTCTCCATCCTCTCCACCCCTCTCCATACGGTCTCTCCAACTCTCCAGAAATGATCACTCTGGATCCTCTCCAGGCCCTCTCCACTCTCCATTAGACTATGCCCTCTCCAATCTCTCCAACCTCTCCACTCTCCAGCTCTCCACTTCTCTCCAATGCTCTCCAGCCTGATCTCTCCATCTCTCCAGTCCGTCTCTCCAACCATTGCTCTCTCCACGTCGTCTCTCCACTCCTCTCCACTCTGCTCTCCACCGCTCTCCACTCTCCATTATCGATCTAACTCTCCAGTCGTACTCTCCAGCTCTCCACCTCTCCAGTACTCACCTCTCCACTCTCCAGGTTACCTCTCCACCTCTCTCCAACCTCTCCAACAGACCATATCTCTCCACAACTCTCCAACCTCTCCATAGCTCTCCATCTCTCCACTCTCCATCCTCTCCACTCTCCAATCTCTCCACCTCTCCAGTCCTCTCCAGCTCTCTCCAATGACTCTCCAGAATCCTCTCCAATCTCTCCAACTCTCCACTCTCCAATCTTTCGATCTCTCCAGCGCTCTCCACTCTCCACTCTCCAACCTCTCCATTCTCTCCAAGCTCTCCACTCTCCATAACTCTCCATCTCTCCACTCTCCACTCTCCACACTCTCCAAGCCTCTCCAGATCGTATGGACTCTCCAATTCCTCTCCAGCTCTCCATCTCTCCATGCTCTCCACCTCTCCATTCCTCTCCAGCCCTCTCCACTCTCCAGCTTCTCTCCACGCTCTCCAAACGCCTCTCCAAACCTCTCCACTCTCCAGCGCCCGAACTCTCCACTCTCCAAGCTCTCCACTCTCCAATACTCTCCACTCCTCTCCACCTCTCCATCTTCTCTCCAAGTGGCTCTCCACGGAAACTCTCCATAATCGCCTCTCCACTCTCCAGCTCTCCAGGCCTCTCCACATCATAGGTGAGAACTCTCCAGGCTCTCCATTCTTCTCTCCATCTCTCTCCAGCTCTCCATGGCTCTCCACTCTCCAGACCTCTCCATCTCTCCAAATCTCTCCAACTCTCCAGAGTACTCTCCACCTCTCCAGGACTCTCCACCACCATCTCTCCAGCCATCATATTTATCTCTCCACTCTCCACTCTCTCCACTCTCCAGCTCTCCAGCCTCTCCATCTCTCCAGCTCTCCAATTCTCTCCACTCTCCACTCTCCAGTCTCTCCACTCTCCAACTCTCCAACTCTCCAACTCTCCACGCTCTCCAGTGACTCTCCAGCTCTCCACATCCCTCTCCAGTAACACAAACTCTCCAAGGCGATTGTCTCTCCAGGTCTCTCCACTCTCCATCGCTCTCCACTTGTGCTCTCCACTCTCCACGTCTCTCCATCTCTCCACTCTCCAGTCTCTCCACCTCTCCACTCTCCATACTCTCCATCTCTCCAGTCTCCCGGCGTGATCTCTCCATACGCTCTCCATCTCTCCACTCTCCACTCTCCAGATACTCTCCACTTCTCTCCAGTCTCTCCACTCTCCATGCTCTCCACCTCTCCATCCTCTCCACACTCTCCACCGCCTCTCCAGGCTCTCCACCTCTCCACTCTCCACTCTCCACAACCTCTCCACGAGTCCATGCTCTCCACTCTCCACTCTCCACCGCTCTCCACTCTCCAGTCAGCTCTCCACTCTCCAGGACTCTCTCCACTCTCCAGCGAGCACCTCTCCACTCTCCACTCTCCACTCTCCAAGTCCTCTCCACTCTCCATCGATCTCTCCAGCCCCTCTCCACTCTCCAGAACTCTCCATGGCTCTCCACTCTCCATCTCTCCATCTCTCCACTCTCCAGGCCTCTCCAATCTCTCCAGACTCTCCACTCTCCATCTCTCCAGCCTCTCTCCATGTCCCCTCTCCATACCTACTCTCCAGTGCTTCTCTCCATCTCTCCAGGGCTCTCCAACGTCTCTCCAATGTATTCTCTCCAATTTGCTCTCCATCTCTCCACTCTCCAGCTACTCTCCATTACTCTCCACTCTCCACACTCTCCACTAGCTCTCCAGCTCTCCAATGTGACTCTCTCCACGCACCCTCTCCACTCTCCATTCTCTCCACTCTCCACTCTCCAACTCTCCATGCAAACTCTCCAACGATGCCTCTCCATATGAAAAGCTCTCCAGCTCTCCAACTCTCCACCTCTCCATGCTCTCCAGACTCTCCAGCTCTCCAGACTCTCCACTCTCCAGCTCTCCAGTCTCTCCAGCTCTCCACTCTCCACAATGTTCTCTCCACTCTCCACTCTCCAAAGCCTCTCCACTCTCCATCTCTCCAGCCTGGTCCTCTCCAGAAAATTCTCTCCACGCTCTCCAACAAACTCTCCAACTCTCCACTCTCCACTCTCCACTCTCCACTCTCCACCTCGCTCTCCAACTCTCCAACTCTCCATCTCTCCACTCTCCACTCTCCACCTCTCCACTCTCCACTCTCCAACTCTCCATTCTCTCCACTAAGCCTAGATTCTCTCCAGACTCTCCAATCTACACTCTCCAGACCTCTCCAAGCTCTCCATATGACGAGCTCTCCAACTCTCCAGCTCTCCACTCCTCTCCAAGAACTCTCCACTCTCCAAAAGTCTCTCCATACTCTCCACTCTCCACTCTCCATCTCTCCACCCCTCTCCACTCGTGATAGCTTGATCTCTCCAACTCTCCATGTAGAGTCTCTCCACATATTCCTCTCCACCTCTCCACTCTCTCCATACTCTCCAGTCTCTCCACACTCTCCATCTCTCCATTACTCTCCATCGGGGTCAACGTTCTCTCCACAACCTCTCCATACTCTCCACCTCTCCAGCTCTCCACTCTCCATACTCTCCACCTCTCCACTCACTCTCCAATCTCTCCAAGCCTCTCCACGTGCTCTCCAGCTCTCCACCTCTCCATCTCTCCACTCTCCATAGATCACTCTCCACAAAACTCTCCAAACCTGAAGCTCTCCACTCTCCATTTCAATCTCTCCACTCTCCACCGCTCTCCAACGACCTAAGCTCTCCATTGAGTATCTCTCCACCTCTCCACGATCCCTCTCCAGTCCTCTCCACAGCCTCTCCACTCTCCACTCTCCATGTTAGTCTCTCCACGTTCCTCTCCAAAGAGGCTCTCCACTCTCCAACTCTCCACTCTCCAACTCTCCACTCTCCACTACCCTCTCCACGTCTCTCCACTCTCCAACTCTCCACTCTCCATTTCTCCCTCTCCACTCTCCAAGCTCTCCAACTCTCCAACTCTCCATCATCCTCTCCAGTCTCTCCATTGCTCTCCAAACTCTCCACTCTCCAGACCTCTCTCCAACTCTCCAGAACAGATACTTTTCTCTCCAGCTTCTCTCCACTCTCCACTCTCCAGAAATTCTCTCCATGACCGCCTCTCCACGCTCTCCACTCTCCAATCTCTCCAGCTCTCCATCTCTCCACTCTCCACCTCTCCAAACGGGAATGGGGCGCTCTCCATGGCTCTCCAACTCTCCAAAGACTCTCCAGTCTCTCCACATGGCTCTCCAAGTTTCTCTCCACTCTCCATCTCTCCATCTCTCCAACGGACTCTCCACTCTCCACTCTCCACTCTCCAGCCTCTCCACTCTCTCCAGGCTCTCCACTCTCCACGACTCTCCAGCTCTCCACTCTCTCCACCTGACTCTCCAACTCTCCACTCTCCAGCCGCTCTCCAACTCTCCAGCTCTCCATGAACTCTCCACCCGCGCTCTCCACTCTCCAAACTCTCCATTCTCTCCAGCTCTCCACTCTCTCCAGCTCTCCACTCTCCACCTCTCCAAGAACCTCTCCATTCTCTCCACTCTCCAGCACTCTCCAACTCGTGCACTCTCCAACGATCCTCTCCACTCTCCAGTCTCTCCACTCTCCAGCTCTCCAGGTCTCTCCACTCTCCACTCTCCACTCTCCAGTTTGCGTCTCTCCACTCTCCACTCTCCACTTCCCTCCGCTTCACCTCTCCATCCTCTCCACTCTCCACTGCTCTCCACTCTCCACTCTCCACTCTCCATACTAGTACACTCTCCATCCTCTCCACTCTCCACTCTCCACTCTCCATGGTTAGACTCTCCACTCTCCAAATACTCTCCACTCTCCACTCTCTCCACCTCTCCACGCCAACTCTCCACTCTCCAATCCTCTCCACTCTCCAGGTAGTCTCTCCATGTCTCTCCAAAACTCTCCAGCTCTCCAGCTCTCCAAGCTCTCCACTCTCCACTCTCCATGTCCTCTCCATTCTCTCCACTCTCCAGAATACTCTCCATCCTCCTCTCCATACTCTCCAACTGGCTCTCCACTCTCCACTCTCCAAGCTAGTCTCTCCATCTCTCCACTCTCCAAGTCTCCTCTATACTCTCCAATCTCTCCATGCGCTCTCCAGACTCTCCACTCTCCATTGCCTCTCCACACTCTCCATGTACGCGTGACTCTCCAACATTCTCTCCAGGCCCTCTCCACTCTCCACTTGTCTCTCTCCATTAGGCTCTCTCCACCTCTCCACTCTCCACAGTGGTTCTCTCCACGTCTCTCCAACGAACTCTCCAGCGCGTTCTCTCCACTCTCCAGTCTTTCTCTCCAGCTCTCCACTCTCCATTTCTCTCCACTCTCCACTCTCCACATTGACCTCTCCACTCTCCAGAAACTCTCCAACGCCGCTCTCCATGACTCTCCACTCTCCAACTCTCCACTCTCCATGCTCTCCAGTACCACTCTCCACTCTCTCCAAAAAGCTCTCCAGGGACTCTCCACGCTCTCCAGCTCTCCAGCTCTCCACTCTCCACTCTCCACTCTCCACTCTCCAGCTCCTCTCCACTCTCCAGTCCCCTCTCCACCAAACTCTCCACTCTCCACTCTCCAGTAGCACTTTCCTCTCCACTCTCCACCTCTCCACACTCTCCATCTCTCCACTCTCCACGAGCTCTCCACTCTCCAACTCTCCAACTCTCCAACTCTCCACCACTCTCCACTCTCCAAGGACTCTCTCCACTCTCTCCAACTCTCCATTTCCAACTCTCCATAACGCTCTCCAGCTCTCCAACTCTCCACCAGCTCTCCACTCTCCAAAACTCTCCAGGGTCTCTCCAGGCTCTCCATCTCGCTCTCCACTCTCCAGCTCTCCACTCTCCACGAGTTCTTGTACTCTCCACTCTCCAGTCGACTCTCCATTGCTCTCCACTCTCCATCGCCCCTAATATCCTAGCTCTCCAGGGCTCTCTCCACCTCTCCAGGATCTCTCCAACTCTCCACTCTCCACCCTCTCCACTCTCCACCTCTCCACTCTCCACCTCTCCACCTCTCCAGCGAGACCGAGCCTCTCCAACTCTCCAGTGCTCTCCACTCTCCAACTCTCCACTCTCCATCTCTCCAAATCTCTCCAGCTCTCCACTCTCCAATACCCATCTCTCCACTCTCCAACTCTCCAGCCTCTCCATTCCCTCTCCACCTCTCTCCACTCTCCAGACTCTCCACTCTCCAGACTCTCCAGAGCCCGGACTCTCCACTCTCCAACCTCTCCATTTTATAGCTCTCCAACTCTCCACTCTCCACAGCTCTCCACGGGCTCTCCAGCTCTCCACTAACTCTCCATCTCTCCACCTCTCCACTCTCCAACGCGGTGCTCTCCATCTCTCCATTCCCTCTCCACTCTCCATCGCTGCTCTCCAGATCTCTCCACTCTCCATCTCTCCACTCTCCATACTCTCCACTCTCCACTCTCCAACTCTCCAGCTCTCCACTCTCCAAACTCTCCAAGGCGGCTCTCCACGATTACCCCTCTCCACTCTCCACCCTCTCCACTCTCCAAGGGACTCTCTCCACCTCTCCACTCTCCAACCACTTCTCTCCAGCCCTCTCCACTCTCCACCTCTCCAGATGCCTCTCCACTCTCCAACGGAAAGTTCTCTCCACTCTCCACACGAGCTCTCCACCTCTCCAGTGTCTACTCTCCACATACGGAGCCCTCTCCATTCTCTCCATCTCCTCTCCAGTCGCTCTCCAGATCTCTCCACTCTCCACTCTCCACATAGCTCTCCATAACTCTCCAGCTCTCCAACAATCTCTCCACTCTCCACTCTCCACTAACTCTCCAACCTCTCCATGTGGTCTCTCCATCGGGGCTCTCCACTCTCCACTCTCCATAAGGCTCTCCACGTCTCTCCACTCTCCATTCTCTCCATATTCTCTCCAGTGGTGCAAGACTCTCCAAGGTCCTCTCCAAAAGATCTCTCCAGAAGCTCTCCAACTCTCCAAGTCGACGCGTCATCTCTCCAAAGATCTCTCCACAAGTTACTCTCCAGACTCTCCATCTCTCCACTCTCCACTCTCCACTCTCCACTCTCCAGGCTGCTCTCCACTCTCCACTCTCCACTCTCCATAGACTCTCCATCAACTCTCCAGACTCTCCAGGCTCTCCAATGCTCTCCACTCTCCAGCTCTCCAGTCTCTCCAACTCTCCAAACTCTCCACCTCTCCACTCTCCACCTCTCTCCACTCTCCATCTCTCCACTCTCCACTCTCCAACTCTCCACTCTCCAAGCTCTCCATCTCTCCACTCTCCAACCTCTCCACTCTCCACGAGCACTCTCCACTCTCCAACCTCTCCACCTCTCTCCATAGGCCCTTACTCTCCAACCCTCTCCACTCTCCATTGTATCTCTCCAAACTCTCCACTCTCCACCTCTCCACGTAGTGCTCTCCACTCTCCAGCTCTCCAGGCTCTCCACTCTCCAAGCTCTCCAGGAGCTCTCCAGACACTCTCTCCAGTTAACTCTCCACTCTCCACGTCTCTCCATTCGGTTGCTCTCCAACTGCTCTCCACTCTCCATCTCTCCACTCTCCAACTCTCCACTTCTCTCCACACTCTCCATGACTCTCCATATCCTCTCCACGCTCTCCATCTCTCCATTATATCTCTCCATAACTCTCCAACTCTCCAGCCTCTCCACGCACCTCTCCATCGGTGATGCTCTCCAACCTCTCCATCTCTCCAGCACTCTCCATTTGCCTTACTCTCCACTCTCCATGGAGATCCTCTCCAGAGACTCTCCACCTCTCCACCACTCTCCAGCTCTCCACTCTCCACAGTCTCTCCAACCACTGGCTCTCCAGACCCTCTCCAGCCGGATCCCCTCTCCACTCTCCACTCTCCACAAACTCTCCACCTCTCCAGCTCTCCAACCGCTCTCCAGATCTCTCCACTCTCCACCACCGCTCTCCAATTGAGTCCCTCTCCAAGTTCTCTCCAGAATCACTCTCCAAAATCCTCTCCACTCTCCATACTCTCCAACTCTCCATCGTAATCTCTCCACCTCTCCACCATCCCTCTCCACTCTCCAACTCCTCTCCATCTTCGCTTAACTCTCCACTCTCCAAGCTCTCCACCCTCTCCATGCCTCTCCACCTCTCCACGCCTCTCTCCATTCTCTCCACCTCTCCACTCTCCATTCTCTCCACTCTCCAAACTCTCCAACACTGGATTCTCTCCACTCTCCAACGCTCTCCACACCTCTCCAACATGTCTCTCCACTCTCCACTCTCCACAACTCTCCATCACAGGGCTCTCCACTCTCCACCACCTCTCTCCATGTTGCTCTCCATCTCTCCACTCTCCACTTCCTAGCCTAAGCTCCTCTCCACTCTCCATAACAGAGCTCTCCAGCTCTCCACTTCTCTCCACGACTCTCCATCCTCTCCAGCAGTCGTCTCTCCATCCTCTCCAGGCCTCTCCATAGTGTCCTCTCCACTCTCCACTCTCCACTCTCCAATAACAAGCTCTCCACGCTCTCCAATAGCTCTCCATTCTCTCCATAGTCAACCGGACTCTCCACTCTCCAGACTCTCCATCCTCTCCATGATGTTTCTCTCCAAGCCTCTCCATACTCTCTCCAGTTGTCTCTCCATCCAATAAAACGTGTCTCTCCACCTCTCCAACAGCTCTCCACTCTCTCCACACTCTCCAACCTCTCCACCCTCTCCACTCTCCACTCTCCACTCTCCAGCCTACTCTCCACTCTCCACCTTTGTGCTCTCCATTCTCTCCATGCCTCTCCACGACACCTCCTCTCCATCTCTCCATTACTTTCTCTCCAGCACACTCTCCACTCTCCACTCCTCTCTCCAGAATGCCTCTCCATCATCTCTCTCCAGCTCTCCACTCTCCAGCTCTCCACTCTCCAGCTCTCCACTCTCCACTCTCCACTCTCCACACTCTCCATATCAGCTCTCCAAGCCTCTCCAAGCGCTCTCCAACTCTCTCCAATCTCTCCAGCTCCTCTCCAACTCTCCAACGCGAGTTTTGGCTCTCCAAGATAATACTCTCCACTTCCTCTCCAATACTCTCCACTCCCCTCTCCACTCTCCACTCCTCTCCATAGATGCTTTCTCTCCACTCTCCAGCTCTCCATCCACTCTCCACTCTCCATACGGCACTCTCCATACTCTCCAACCGCGCTCTCCATACTCTCCAGACCCTCTCCAGAACAACTCCTCTCCAAATTCTCTCCACACTCTCCATTGCCAACTCTCCACTCTCCACCTCTCCACTCTCCACGTACGCTCTCCAGGCTCTCCACTCTCCATCTCTCCAGCGCTCTCCATCTCTCCAACTCTCCAATGCTCTCCACTCTCCAGCCTCTCCACTCTCTCCACGGCCTCTCCACTCTCCAAAATTAGATCACTCTCCATCTCTCCACCTCTCCACTCTCCACTCTCCAAGCTCTCCACTCTCCACTCTCCACCTCTCCAGACTCTCCAGCTCTCCATCTCTCCAGTCCAACTCTCCACATACTCTCCACCTCTCCACCCTCTCCAACTCTCCAACTCTCCATTCTCTCCACTCTCTCCATACTCTCCACTCTCCACTGGGCTGAGCTCTCCACCTCTCCAAGAGCGAGTTACCTCTCTCCACTCTCTCCACTCTCCACCGGCTCTCCACCCGCTCTCCACTAGTCGCCTCTCCATAACTCTCCACTCTCCACTCTCCACCTCTCCACTCTCCATGGCCGTCTCTCCATCTCTCTCCACGGCTCTCCAAGCACGGCTCTCCACGGGTGCTCTCCAAACTCTCCATTCTCCTCTCCA"
      "CTCTCCACT"
  in
  print_endline (String.concat ~sep:" " (List.map ~f:Int.to_string result));
  [%expect
    {|
    17 56 63 136 156 275 293 330 376 460 504 519 603 613 635 705 799 815 899 933 940 974 999 1006 1114 1163 1186 1202 1219 1287 1383 1491 1498 1507 1557 1564 1580 1695 1712 1725 1750 1774 1839 1846 1864 1883 1953 1960 1995 2002 2019 2038 2057 2079 2086 2093 2111 2141 2168 2191 2226 2354 2382 2398 2445 2461 2468 2585 2617 2638 2645 2663 2736 2743 2750 2757 2800 2807 2822 2829 2853 2947 2968 2996 3003 3028 3096 3207 3231 3296 3345 3366 3457 3464 3510 3525 3540 3547 3569 3584 3605 3677 3736 3743 3786 3818 3917 3952 3959 3966 3982 4000 4025 4054 4112 4145 4162 4198 4244 4260 4285 4292 4299 4321 4328 4335 4367 4374 4384 4391 4398 4431 4438 4445 4467 4485 4492 4522 4539 4604 4611 4638 4690 4697 4726 4782 4850 4857 4893 4951 4979 4996 5003 5024 5065 5080 5109 5166 5173 5180 5187 5205 5236 5243 5268 5300 5318 5359 5379 5449 5498 5513 5539 5568 5639 5655 5670 5730 5745 5778 5800 5845 5861 5893 5932 5968 5995 6036 6066 6081 6097 6104 6127 6172 6188 6217 6248 6275 6299 6400 6407 6456 6463 6470 6516 6523 6552 6705 6712 6719 6726 6745 6752 6759 6816 6865 6882 6897 6904 6919 6943 6959 6979 7032 7061 7090 7114 7166 7209 7224 7239 7407 7466 7527 7534 7589 7661 7720 7756 7828 7844 7877 7917 7924 7956 7996 8003 8027 8057 8134 8141 8148 8218 8332 8368 8375 8382 8401 8492 8499 8545 8560 8575 8582 8589 8712 8733 8745 8752 8779 8805 8918 8933 8962 9013 9029 9049 9090 9097 9113 9120 9226 9244 9251 9297 9306 9335 9360 9367 9382 |}]
;;
