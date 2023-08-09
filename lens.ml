open Imp.Any
open Imp.Control

type ('s, 't, 'a, 'b) lens = {F : Functor} -> ('a -> 'b F.t) -> ('s -> 't F.t)

let _1 {F: Functor} f (a, b) = F.fmap (fun a' -> (a', b)) (f a)
let _2 {F: Functor} f (a, b) = F.fmap (fun b' -> (a, b')) (f b)

let set (lens: ('s, 't, 'a, 'b) lens) (b: 'b) (s: 's) : 't =
  let Identity t = lens (fun _ -> Identity b) s
  in t

let get {A: Any} (lens: ('s, 't, A.t_for_any, A.t_for_any) lens) s =
  let Const a' = lens (fun a -> Const a) s
  in a'
