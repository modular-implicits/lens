open Imp.Any
open Imp.Control
open Imp.Data

type ('s, 't, 'a, 'b) lens = {F : Functor} -> ('a -> 'b F.t) -> ('s -> 't F.t)
type ('s, 't, 'a, 'b) traversal = {F : Applicative} -> ('a -> 'b F.t) -> ('s -> 't F.t)

module T2 = struct
  let _1 {F: Functor} f (a, b) = F.fmap (fun a' -> (a', b)) (f a)
  let _2 {F: Functor} f (a, b) = F.fmap (fun b' -> (a, b')) (f b)
end

module T3 = struct
  let _1 {F: Functor} f (a, b, c) = F.fmap (fun a' -> (a', b, c)) (f a)
  let _2 {F: Functor} f (a, b, c) = F.fmap (fun b' -> (a, b', c)) (f b)
  let _3 {F: Functor} f (a, b, c) = F.fmap (fun c' -> (a, b, c')) (f c)
end

module T4 = struct
  let _1 {F: Functor} f (a, b, c, d) = F.fmap (fun a' -> (a', b, c, d)) (f a)
  let _2 {F: Functor} f (a, b, c, d) = F.fmap (fun b' -> (a, b', c, d)) (f b)
  let _3 {F: Functor} f (a, b, c, d) = F.fmap (fun c' -> (a, b, c', d)) (f c)
  let _4 {F: Functor} f (a, b, c, d) = F.fmap (fun d' -> (a, b, c, d')) (f d)
end

let get (type a) (lens: ('s, 't, a, a) lens) s =
  let implicit module A: Any with type t_for_any = a
    = struct type t_for_any = a end in
  let Const a' = lens (fun a -> Const a) s
  in a'

let gett {A: Monoid} (lens: ('s, 't, A.t, A.t) traversal) s =
  let implicit module A: Any with type t_for_any = A.t
    = struct type t_for_any = A.t end in
  let Const a' = lens (fun a -> Const a) s
  in a'

(* (l >< m) f = l (m (f)) *)
let (><) (l: ('s, 't, 'x, 'y) lens) (m: ('x, 'y, 'a, 'b) lens) : ('s, 't, 'a, 'b) lens =
  fun {F: Functor} f -> l (m f)

let set (l: ('s, 't, 'a, 'b) lens) (b: 'b) (s: 's) : 't =
  let Identity t = l {Identity} (fun _ -> Identity b) s
  in t

let sett (l: ('s, 't, 'a, 'b) traversal) (b: 'b) (s: 's) : 't =
  let Identity t = l (fun _ -> Identity b) s
  in t

let traversed {T: Traversable} : ('a T.t, 'b T.t, 'a, 'b) traversal =
  fun {A: Applicative} f -> T.traverse f
