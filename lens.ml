open Imp.Any
open Imp.Control

type ('a, 's) lens = {F : Functor} -> ('a -> 'a F.t) -> ('s -> 's F.t)

let _1 {F: Functor} f (a, b) = F.fmap (fun a' -> (a', b)) (f a)
let _2 {F: Functor} f (a, b) = F.fmap (fun b' -> (a, b')) (f b)

let set (lens: ('a, 's) lens) (a: 'a) (s: 's) : 's =
  let Identity s' = lens (fun _ -> Identity a) s
  in s'

let get {A: Any} (lens: (A.t_for_any, 's) lens) s =
  let Const a' = lens (fun a -> Const a) s
  in a'
