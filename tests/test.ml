let () =
  let open implicit Imp.Any in
  let open Lens in
  assert (set T2._1 "lol" (1, "hello") = ("lol", "hello"));
  assert (get T2._1 (1, "hello") = 1);
  assert (get T2._2 (1, "hello") = "hello");
  assert (get (T2._1 >< T2._2) ((5, "lol"), "hello") = "lol");
  assert (set (T2._1 >< T2._2) 7 ((5, "lol"), "hello") = ((5, 7), "hello"));
  assert (set (at 2) (Some 9) [0;1;2;3;4] = [0;1;9;3;4]);
  assert (set (at 2) None [0;1;2;3;4] = [0;1;3;4]);
  assert (get (at 2) [0;1;2;3;4] = Some 2);
  assert (get (at 9) [0;1;2;3;4] = None)
