let () =
  let open implicit Imp.Any in
  let open Lens in
  assert (set _1 "lol" (1, "hello") = ("lol", "hello"));
  assert (get _1 (1, "hello") = 1);
  assert (get _2 (1, "hello") = "hello");
  assert (get (_1 >< _2) ((5, "lol"), "hello") = "lol");
  assert (set (_1 >< _2) 7 ((5, "lol"), "hello") = ((5, 7), "hello"))
