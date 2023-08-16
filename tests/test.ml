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
  assert (getOption (index 2) [0;1;2;3;4] = Some 2);
  assert (getOption (index 12) [0;1;2;3;4] = None);
  assert (getOption (index ~-1) [0;1;2;3;4] = None);
  assert (set (index 3) '!' "abcdef" = "abc!ef");
  assert ("abcdef" ^? index 3 = Some 'd');
  assert (('a', 'b') ^? index 0 = Some 'a');
  assert (('a', 'b') ^? index 3 = None);
  assert (set (index 0) 'c' ('a', 'b') = ('c', 'b'));
  assert (set (index 3) 'c' ('a', 'b') = ('a', 'b'));
  assert (set (mapped {Imp.Control.List}) 4 [1;2;3] = [4;4;4]);
  assert (("hello", 5) |> T2._2 @~ ((+) 1) = ("hello", 6));
  assert (("hi", "five") |> T2._2 @. 5 = ("hi", 5));
  assert (("hi", 5) ^. T2._1 = "hi");
  assert (123 ^? empty = None);
  assert ("five" |> equality @. 5 = 5)
