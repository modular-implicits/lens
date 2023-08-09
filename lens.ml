open Imp.Any
open Imp.Control
open Imp.Data

(* TYPE SETUP AND COMBINATORS *)

type ('s, 't, 'a, 'b) lens = {F : Functor} -> ('a -> 'b F.t) -> ('s -> 't F.t)
type ('s, 't, 'a, 'b) traversal = {F : Applicative} -> ('a -> 'b F.t) -> ('s -> 't F.t)

type ('a, 's, 'x) getter = ('x -> ('a, 'x) const) -> ('s -> ('a, 's) const)

module type Getter = sig
  type ('s, 't, 'a, 'b) t
  type a
  val convert : ('s, 's, a, a) t -> (a, 's, a) getter
end

implicit module Lens_Getter {A: Any} : Getter
  with type ('s, 't, 'a, 'b) t = {F: Functor} -> ('a -> 'b F.t) -> ('s -> 't F.t)
  and type a = A.t_for_any
= struct
  type ('s, 't, 'a, 'b) t = {F: Functor} -> ('a -> 'b F.t) -> ('s -> 't F.t)
  type a = A.t_for_any
  let convert (l: ('s, 't, a, a) t): (a, 's, a) getter =
    l {Const {A}}
end

implicit module Traversal_Getter {A: Monoid} : Getter
  with type ('s, 't, 'a, 'b) t = {F: Applicative} -> ('a -> 'b F.t) -> ('s -> 't F.t)
  and type a = A.t
= struct
  type ('s, 't, 'a, 'b) t = {F: Applicative} -> ('a -> 'b F.t) -> ('s -> 't F.t)
  type a = A.t
  let convert (l: ('s, 't, a, a) t): (a, 's, a) getter =
    l {Const_Applicative {A}}
end

let get {L: Getter} (lens: ('s, 't, L.a, L.a) L.t) s =
  let Const a' = L.convert lens (fun a -> Const a) s
  in a'

(* (l >< m) f = l (m (f)) *)
let (><) (l: ('s, 't, 'x, 'y) lens) (m: ('x, 'y, 'a, 'b) lens) : ('s, 't, 'a, 'b) lens =
  fun {F: Functor} f -> l (m f)

type ('s, 't, 'a, 'b) setter = ('a -> 'b identity) -> ('s -> 't identity)

module type Setter = sig
  type ('s, 't, 'a, 'b) t
  val convert : ('s, 't, 'a, 'b) t -> ('s, 't, 'a, 'b) setter
end

implicit module Lens_Setter : Setter
  with type ('s, 't, 'a, 'b) t = {F: Functor} -> ('a -> 'b F.t) -> ('s -> 't F.t)
= struct
  type ('s, 't, 'a, 'b) t = {F: Functor} -> ('a -> 'b F.t) -> ('s -> 't F.t)
  let convert (l: ('s, 't, 'a, 'b) t): ('s, 't, 'a, 'b) setter = l {Identity}
end

implicit module Traversal_Setter : Setter
  with type ('s, 't, 'a, 'b) t = {F: Applicative} -> ('a -> 'b F.t) -> ('s -> 't F.t)
= struct
  type ('s, 't, 'a, 'b) t = {F: Applicative} -> ('a -> 'b F.t) -> ('s -> 't F.t)
  let convert (l: ('s, 't, 'a, 'b) t): ('s, 't, 'a, 'b) setter = l {Identity}
end

let set {L: Setter} (l: ('s, 't, 'a, 'b) L.t) (b: 'b) (s: 's) : 't =
  let Identity t = L.convert l (fun _ -> Identity b) s
  in t

(* THE LENSES THEMSELVES *)

let traversed {T: Traversable} : ('a T.t, 'b T.t, 'a, 'b) traversal =
  fun {A: Applicative} f -> T.traverse f

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
