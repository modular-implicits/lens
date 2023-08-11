let () =
  let open implicit Imp.Any in
  let open implicit Imp.Data in
  let open Lens in
  assert (set T2._1 "lol" (1, "hello") = ("lol", "hello"));
  assert (get T2._1 (1, "hello") = 1);
  assert (get T2._2 (1, "hello") = "hello");
  assert (get (T2._1 **** T2._2) (("lol", 5), "hello") = 5);
  assert (set (T2._1 *** T2._2) 7 ((5, "lol"), "hello") = ((5, 7), "hello"));
  assert (set (traversed {Imp.Control.List}) 5 [1;2;3;4] = [5;5;5;5]);
  assert (get (traversed {Imp.Control.List}) ["hi"; "there"] = "hithere");
  assert (get (traversed {Imp.Control.List} *** T2._1) [("hi", 5); ("there", 3)] = "hithere")
