let () =
  let open implicit Imp.Any in
  let open implicit Imp.Data in
  let open Lens in
  assert (set T2._1 "lol" (1, "hello") = ("lol", "hello"));
  assert (get T2._1 (1, "hello") = 1);
  assert (get T2._2 (1, "hello") = "hello");
  assert (get ((T2._1 *** T2._2): _ lens) (("lol", 5), "hello") = 5);
  assert (set ((T2._1 *** T2._2): _ lens) 7 ((5, "lol"), "hello") = ((5, 7), "hello"));
  assert (set (traversed {Imp.Control.List}) 5 [1;2;3;4] = [5;5;5;5]);
  assert (get (traversed {Imp.Control.List}) ["hi"; "there"] = "hithere");
  assert (get ((traversed {Imp.Control.List} *** T2._1): _ traversal) [("hi", 5); ("there", 3)] = "hithere");
  assert (set (index 2) 9 [0;1;2;3;4] = [0;1;9;3;4]);
  assert (set (index 7) 9 [0;1;2;3;4] = [0;1;2;3;4]);
  assert (getMaybe (index 2) [0;1;2;3;4] = Some 2);
  assert (getMaybe (index 12) [0;1;2;3;4] = None);
  assert (getMaybe (index ~-1) [0;1;2;3;4] = None);
  assert (set (index 3) '!' "abcdef" = "abc!ef");
  assert (getMaybe (index 3) "abcdef" = Some 'd')
