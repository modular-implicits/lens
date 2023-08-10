let () =
  let open Imp.Any in
  let open implicit Imp.Data in
  let open Lens in
  assert (set T2._1 "lol" (1, "hello") = ("lol", "hello"));
  assert (get T2._1 (1, "hello") = 1);
  assert (get T2._2 (1, "hello") = "hello");
  assert (get (T2._1 >< T2._2) ((5, "lol"), "hello") = "lol");
  assert (set (T2._1 >< T2._2) 7 ((5, "lol"), "hello") = ((5, 7), "hello"));
  assert (set (traversed {Imp.Control.List}) 5 [1;2;3;4] = [5;5;5;5]);
  assert (get (traversed {Imp.Control.List}) ["hi"; "there"] = "hithere");
  let implicit module Any_Option {A: Any}: Any with type t_for_any = A.t_for_any option = struct
    type t_for_any = A.t_for_any option
  end in
  assert (set (at 2) (Some 9) [0;1;2;3;4] = [0;1;9;3;4]);
  assert (set (at 2) None [0;1;2;3;4] = [0;1;3;4]);
  assert (get (at 2) [0;1;2;3;4] = Some 2);
  assert (get (at 9) [0;1;2;3;4] = (None: int option))
