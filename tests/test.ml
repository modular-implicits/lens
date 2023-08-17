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
  assert (("hi", 5) |> T2._1 @? true = (Some true, 5));
  assert (123 ^? empty = None);
  assert ("five" |> equality @. 5 = 5);
  assert ([1;2;3] |> head @. 0 = [0;2;3]);
  assert ([1;2;3] |> tail @. 0 = [1;0;0])

let () =
  let open Imp.Any in
  let open Lens in
  let module IntOrd = struct
    type t = int
    let compare (x: int) y = compare x y
  end in
  let implicit module IntMap = Map.Make (IntOrd) in
  assert (IntMap.empty ^? index 3 = (None: char option));
  let myMap = IntMap.(empty |> add 1 "one" |> add 2 "two") in
  assert (myMap ^. at 1 = Some "one");
  assert (myMap ^. at 3 = (None: string option));
  assert (myMap |> at 1 @? "one" = myMap);
  let myMap' = myMap |> IntMap.add 3 "three" in
  assert (myMap |> at 3 @? "three" = myMap');
  assert (myMap' |> at 3 @. None = myMap);
  (* without this, we get an unused open Imp.Any;
     and changing that to open implicit Imp.Any gives a type error for some reason *)
  Any_Int.__any__

let () =
  let open Imp.Any in
  let open Imp.Control in
  let open Imp.Transformers in
  let open Lens in
  let module S = State {Any_Option {Any_Int}} in
  let (>>) a b = bind {S} a (fun _ -> b) in
  let t = traversed {Imp.Control.Option} in
  ignore (
    put (Some 0) >>
    (t =~ (+) 1) >>
    fmap (fun x -> assert (x = Some 1)) (Imp.Transformers.get {S}) >>
    (equality =. Some 35) >>
    fmap (fun x -> assert (x = Some 35)) (Imp.Transformers.get {S}) >>
    (equality =? ~-2) >>
    fmap (fun x -> assert (x = Some ~-2)) (Imp.Transformers.get {S})
  )
