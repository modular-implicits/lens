module OCAML_String = String

open Imp.Any
open Imp.Control
open Imp.Data

(* TYPE SETUP AND COMBINATORS *)

type ('s, 't, 'a, 'b) lens = {F : Functor} -> ('a -> 'b F.t) -> ('s -> 't F.t)
type ('s, 'a) lens' = ('s, 's, 'a, 'a) lens
type ('s, 't, 'a, 'b) traversal = {F : Applicative} -> ('a -> 'b F.t) -> ('s -> 't F.t)
type ('s, 'a) traversal' = ('s, 's, 'a, 'a) traversal

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

implicit module Getter_Getter {A: Any} : Getter
  with type a = A.t
  and type 's t = (A.t, 's, A.t) getter
= struct
  type a = A.t
  type 's t = (A.t, 's, A.t) getter
  let convert (l: 's t): (a, 's, a) getter = l
end

implicit module Lens_Getter {A: Any} : Getter
  with type a = A.t
  and type 's t = {F: Functor} -> (A.t -> A.t F.t) -> ('s -> 's F.t)
= struct
  type a = A.t
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

let (^.) {L: Getter} s l = get {L} l s

let getOption (type a) (lens: ('s, 's, a, a) traversal) (s: 's) : a option =
  let open Monoid in
  let implicit module A: Any
    with type t = a
  = struct
    type t = a
    let __any__ = ()
  end in
  let Const { first = a' } = lens {Const_Applicative {First {A}}} (fun a -> Const { first = Some a }) s
  in a'

let (^?) s l = getOption l s

type ('s, 't, 'a, 'b) setter = ('a -> 'b identity) -> ('s -> 't identity)

module type Setter = sig
  type ('s, 't, 'a, 'b) t
  val convert : ('s, 't, 'a, 'b) t -> ('s, 't, 'a, 'b) setter
end

implicit module Setter_Setter : Setter
  with type ('s, 't, 'a, 'b) t = ('s, 't, 'a, 'b) setter
= struct
  type ('s, 't, 'a, 'b) t = ('s, 't, 'a, 'b) setter
  let convert (l: ('s, 't, 'a, 'b) t): ('s, 't, 'a, 'b) setter = l
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

let (@.) = set

let (=.) {M: Imp.Transformers.MonadState} {L: Setter} (l: (M.s, M.s, 'a, 'b) L.t) (b: 'b) : unit M.t =
  Imp.Transformers.modify (set l b)

let (@?) {L: Setter} (l: ('s, 't, 'a, 'b option) L.t) (b: 'b) (s: 's) : 't =
  set l (Some b) s

let (=?) {M: Imp.Transformers.MonadState} {L: Setter} (l: (M.s, M.s, 'a, 'b option) L.t) (b: 'b) : unit M.t =
  Imp.Transformers.modify (l @? b)

let modify {L: Setter} (l: ('s, 't, 'a, 'b) L.t) (f: 'a -> 'b) (s: 's) : 't =
  let Identity t = L.convert l (fun a -> Identity (f a)) s
  in t

let (@~) = modify

let (=~) {M: Imp.Transformers.MonadState} {L: Setter} (l: (M.s, M.s, 'a, 'b) L.t) (f: 'a -> 'b) : unit M.t =
  Imp.Transformers.modify (modify l f)

module type Indexed = sig
  type index
  type value
  type t
  val index : index -> (t, value) traversal'
end

let index {I: Indexed} = I.index

module type At = sig
  type index
  type value
  type t
  val at : index -> (t, value option) lens'
end

let at {I: At} = I.at

let mapped {F: Functor} () : ('a F.t, 'b F.t, 'a, 'b) setter =
  fun f s ->
    let f' a = runIdentity (f a)
    in Identity (fmap f' s)

let traversed {T: Traversable} () : ('a T.t, 'b T.t, 'a, 'b) traversal =
  fun {A: Applicative} f -> T.traverse f

let empty {F: Applicative} (_: 'a -> 'b F.t) s = F.return s

let equality {F: Functor} (f: 'a -> 'b F.t) a = f a

(* THE LENSES THEMSELVES *)

let head {F: Applicative} f = function
  | a :: as_ -> F.fmap (fun a' -> a' :: as_) (f a)
  | [] -> F.return []

let tail {F: Applicative} f = function
  | a :: as_ -> F.fmap (fun as' -> a :: as') (traverse f as_)
  | [] -> F.return []

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

(* warning: indexing takes linear time! *)
implicit module ListIndexed {A: Any}:
  Indexed with type index = int and type value = A.t and type t = A.t list
= struct
  type index = int
  type value = A.t
  type t = A.t list
    
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

implicit module Tuple2Indexed {A: Any} : Indexed
  with type index = int and type value = A.t and type t = A.t * A.t
= struct
  type index = int
  type value = A.t
  type t = A.t * A.t
  let index = function
    | 0 -> fun {F: Applicative} f (a, b) -> F.fmap (fun a' -> (a', b)) (f a)
    | 1 -> fun {F: Applicative} f (a, b) -> F.fmap (fun b' -> (a, b')) (f b)
    | _ -> fun {F: Applicative} _ s      -> F.return s
end

implicit module Tuple3Indexed {A: Any} : Indexed
  with type index = int and type value = A.t and type t = A.t * A.t * A.t 
= struct
  type index = int
  type value = A.t
  type t = A.t * A.t * A.t
  let index = function
    | 0 -> fun {F: Applicative} f (a, b, c) -> F.fmap (fun a' -> (a', b, c)) (f a)
    | 1 -> fun {F: Applicative} f (a, b, c) -> F.fmap (fun b' -> (a, b', c)) (f b)
    | 2 -> fun {F: Applicative} f (a, b, c) -> F.fmap (fun c' -> (a, b, c')) (f c)
    | _ -> fun {F: Applicative} _ s      -> F.return s
end

implicit module Tuple4Indexed {A: Any} : Indexed
  with type index = int and type value = A.t and type t = A.t * A.t * A.t * A.t
= struct
  type index = int
  type value = A.t
  type t = A.t * A.t * A.t * A.t
  let index = function
    | 0 -> fun {F: Applicative} f (a, b, c, d) -> F.fmap (fun a' -> (a', b, c, d)) (f a)
    | 1 -> fun {F: Applicative} f (a, b, c, d) -> F.fmap (fun b' -> (a, b', c, d)) (f b)
    | 2 -> fun {F: Applicative} f (a, b, c, d) -> F.fmap (fun c' -> (a, b, c', d)) (f c)
    | 3 -> fun {F: Applicative} f (a, b, c, d) -> F.fmap (fun d' -> (a, b, c, d')) (f d)
    | _ -> fun {F: Applicative} _ s      -> F.return s
end

implicit module MapIndexed {M: Map.S} {V: Any} : Indexed
  with type index = M.key and type value = V.t and type t = V.t M.t
= struct
  type index = M.key
  type value = V.t
  type t = V.t M.t
  let index k = fun {F: Applicative} f m ->
    if M.mem k m
    then F.fmap (fun x' -> M.add k x' m) (f (M.find k m))
    else F.return m
end

implicit module MapAt {M: Map.S} {V: Any} : At
  with type index = M.key and type value = V.t and type t = V.t M.t
= struct
  type index = M.key
  type value = V.t
  type t = V.t M.t
  let at k = fun {F: Functor} f m ->
    let update = function
      | None -> M.remove k m
      | Some x' -> M.add k x' m
    in
    if M.mem k m
    then F.fmap update (f (Some (M.find k m)))
    else F.fmap update (f None)
end
