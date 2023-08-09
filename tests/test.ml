let () =
  let open implicit Imp.Data in
  let open Lens in
  assert (set T2._1 "lol" (1, "hello") = ("lol", "hello"));
  assert (get T2._1 (1, "hello") = 1);
  assert (get T2._2 (1, "hello") = "hello");
  assert (get (T2._1 >< T2._2) ((5, "lol"), "hello") = "lol");
  assert (set (T2._1 >< T2._2) 7 ((5, "lol"), "hello") = ((5, 7), "hello"));
  assert (sett (traversed {Imp.Control.List}) 5 [1;2;3;4] = [5;5;5;5]);
  assert (gett (traversed {Imp.Control.List}) ["hi"; "there"] = "hithere")
