module OCAML_String = String

open Imp.Any
open Imp.Control
open Imp.Data

(* TYPE SETUP AND COMBINATORS *)

type ('s, 't, 'a, 'b) lens = {F : Functor} -> ('a -> 'b F.t) -> ('s -> 't F.t)
type ('s, 'a) lens' = ('s, 's, 'a, 'a) lens
type ('s, 't, 'a, 'b) traversal = {F : Applicative} -> ('a -> 'b F.t) -> ('s -> 't F.t)
type ('s, 'a) traversal' = {F : Applicative} -> ('a -> 'a F.t) -> ('s -> 's F.t)

module type Composable = sig
  type ('s, 't, 'a, 'b) x
  type ('s, 't, 'a, 'b) y
  type ('s, 't, 'a, 'b) z
  val ( *** ) : ('s, 't, 'p, 'q) x -> ('p, 'q, 'a, 'b) y -> ('s, 't, 'a, 'b) z
end

implicit module Lens_Composable: Composable
  with type ('s, 't, 'a, 'b) x = ('s, 't, 'a, 'b) lens
  and type ('s, 't, 'a, 'b) y = ('s, 't, 'a, 'b) lens
  and type ('s, 't, 'a, 'b) z = ('s, 't, 'a, 'b) lens
= struct
  type ('s, 't, 'a, 'b) x = ('s, 't, 'a, 'b) lens
  type ('s, 't, 'a, 'b) y = ('s, 't, 'a, 'b) lens
  type ('s, 't, 'a, 'b) z = ('s, 't, 'a, 'b) lens
  let ( *** ) (f: ('s, 't, 'p, 'q) x) (g: ('p, 'q, 'a, 'b) y): ('s, 't, 'a, 'b) z =
    fun {F: Functor} a -> f {F} (g {F} a)
end

implicit module TraversalLens_Composable: Composable
  with type ('s, 't, 'a, 'b) x = ('s, 't, 'a, 'b) traversal
  and type ('s, 't, 'a, 'b) y = ('s, 't, 'a, 'b) lens
  and type ('s, 't, 'a, 'b) z = ('s, 't, 'a, 'b) traversal
= struct
  type ('s, 't, 'a, 'b) x = ('s, 't, 'a, 'b) traversal
  type ('s, 't, 'a, 'b) y = ('s, 't, 'a, 'b) lens
  type ('s, 't, 'a, 'b) z = ('s, 't, 'a, 'b) traversal
  let ( *** ) (f: ('s, 't, 'p, 'q) x) (g: ('p, 'q, 'a, 'b) y): ('s, 't, 'a, 'b) z =
    fun {F: Applicative} a -> f {F} (g {F} a)
end

implicit module LensTraversal_Composable: Composable
  with type ('s, 't, 'a, 'b) x = ('s, 't, 'a, 'b) lens
  and type ('s, 't, 'a, 'b) y = ('s, 't, 'a, 'b) traversal
  and type ('s, 't, 'a, 'b) z = ('s, 't, 'a, 'b) traversal
= struct
  type ('s, 't, 'a, 'b) x = ('s, 't, 'a, 'b) lens
  type ('s, 't, 'a, 'b) y = ('s, 't, 'a, 'b) traversal
  type ('s, 't, 'a, 'b) z = ('s, 't, 'a, 'b) traversal
  let ( *** ) (f: ('s, 't, 'p, 'q) x) (g: ('p, 'q, 'a, 'b) y): ('s, 't, 'a, 'b) z =
    fun {F: Applicative} a -> f {F} (g {F} a)
end

implicit module Traversal_Composable: Composable
  with type ('s, 't, 'a, 'b) x = ('s, 't, 'a, 'b) traversal
  and type ('s, 't, 'a, 'b) y = ('s, 't, 'a, 'b) traversal
  and type ('s, 't, 'a, 'b) z = ('s, 't, 'a, 'b) traversal
= struct
  type ('s, 't, 'a, 'b) x = ('s, 't, 'a, 'b) traversal
  type ('s, 't, 'a, 'b) y = ('s, 't, 'a, 'b) traversal
  type ('s, 't, 'a, 'b) z = ('s, 't, 'a, 'b) traversal
  let ( *** ) (f: ('s, 't, 'p, 'q) x) (g: ('p, 'q, 'a, 'b) y): ('s, 't, 'a, 'b) z =
    fun {F: Applicative} a -> f {F} (g {F} a)
end

let ( *** ) {C: Composable} = C.( *** )

type ('a, 's, 'x) getter = ('x -> ('a, 'x) const) -> ('s -> ('a, 's) const)

module type Getter = sig
  type a
  type 's t
  val convert : 's t -> (a, 's, a) getter
end

implicit module Lens_Getter {A: Any} : Getter
  with type a = A.t_for_any
  and type 's t = {F: Functor} -> (A.t_for_any -> A.t_for_any F.t) -> ('s -> 's F.t)
= struct
  type a = A.t_for_any
  type 's t = {F: Functor} -> (a -> a F.t) -> ('s -> 's F.t)
  let convert (l: 's t): (a, 's, a) getter =
    l {Const {A}}
end

implicit module Traversal_Getter {A: Monoid} : Getter
  with type a = A.t
  and type 's t = {F: Applicative} -> (A.t -> A.t F.t) -> ('s -> 's F.t)
= struct
  type a = A.t
  type 's t = {F: Applicative} -> (a -> a F.t) -> ('s -> 's F.t)
  let convert (l: 's t): (a, 's, a) getter =
    l {Const_Applicative {A}}
end

let get {L: Getter} (lens: 's L.t) s =
  let Const a' = L.convert lens (fun a -> Const a) s
  in a'

type ('s, 't, 'a, 'b) setter = ('a -> 'b identity) -> ('s -> 't identity)

module type Setter = sig
  type ('s, 't, 'a, 'b) t
  val convert : ('s, 't, 'a, 'b) t -> ('s, 't, 'a, 'b) setter
end

implicit module Lens_Setter : Setter
  with type ('s, 't, 'a, 'b) t = ('s, 't, 'a, 'b) lens
= struct
  type ('s, 't, 'a, 'b) t = ('s, 't, 'a, 'b) lens
  let convert (l: ('s, 't, 'a, 'b) t): ('s, 't, 'a, 'b) setter = l {Identity}
end

implicit module Traversal_Setter : Setter
  with type ('s, 't, 'a, 'b) t = ('s, 't, 'a, 'b) traversal
= struct
  type ('s, 't, 'a, 'b) t = ('s, 't, 'a, 'b) traversal
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

module type Indexed = sig
  type index
  type value
  type t
  val index : index -> (t, value) traversal'
end

(* warning: indexing takes linear time! *)
implicit module ListIndexed {A: Any}:
  Indexed with type index = int and type value = A.t_for_any and type t = A.t_for_any list
= struct
  type index = int
  type value = A.t_for_any
  type t = A.t_for_any list
    
  let index i : (t, value) traversal' =
    let rec go : ({F: Applicative} -> (value -> value F.t) -> value list * int -> (value list) F.t) = fun {F: Applicative} f -> function
      | ([], _) -> F.return []
      | (x :: xs, 0) -> F.fmap (fun x' -> x' :: xs) (f x)
      | (x :: xs, i) -> F.fmap (fun xs' -> x :: xs') (go {F} f (xs, i - 1))
    in
    let lens {F: Applicative} f xs = go {F} f (xs, i)
    in lens
end

(* warning: doesn't work with Unicode *)
implicit module BytesIndexed: Indexed
  with type index = int and type value = char and type t = bytes
= struct
  type index = int
  type value = char
  type t = bytes

  let index i : (t, value) traversal' =
    let open OCAML_String in
    let lens {F: Applicative} f s =
      let l = length s in
      if i < 0 || i >= l then F.return "" else
      let start = sub s 0 i in
      let middle = get s i in
      let end' = sub s (i+1) (l-i-1) in
      F.fmap (fun middle' -> start ^ make 1 middle' ^ end') (f middle)
    in lens
end

let index {I: Indexed} = I.index

let getMaybe (type a) (lens: ('s, 's, a, a) traversal) s =
  let implicit module First : Imp.Data.Monoid
     with type t = a option
  = struct
    type t = a option
    let empty = None
    let append x y = match x with
      | None -> y
      | Some x' -> Some x'
  end in
  let Const a' = lens (fun a -> Const (Some a)) s
  in a'
