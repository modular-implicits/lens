open Imp.Any
open Imp.Control
open Imp.Data

type ('s, 't, 'a, 'b) lens = {F : Functor} -> ('a -> 'b F.t) -> ('s -> 't F.t)
(** A lens has the same power as a getter and setter combined *)

type ('s, 'a) lens' = {F : Functor} -> ('a -> 'a F.t) -> ('s -> 's F.t)
(** `lens'` is an alias for a lens which doesn't change types *)

type ('s, 't, 'a, 'b) traversal = {F : Applicative} -> ('a -> 'b F.t) -> ('s -> 't F.t)
(** A traversal is like a lens, but it can operate on more then one focus at once *)

type ('s, 'a) traversal' = {F : Applicative} -> ('a -> 'a F.t) -> ('s -> 's F.t)
(** `traversal'` is an alias for a traversal which doesn't change types *)

module type Composable = sig
  type ('s, 't, 'a, 'b) x
  type ('s, 't, 'a, 'b) y
  type ('s, 't, 'a, 'b) z
  val ( *** ) : ('s, 't, 'p, 'q) x -> ('p, 'q, 'a, 'b) y -> ('s, 't, 'a, 'b) z
end
(** This module exists as a workaround for a limitation in the OCaml type system.
    You can mostly ignore it. For more details, see ( *** )
 *)

implicit module Lens_Composable: Composable
  with type ('s, 't, 'a, 'b) x = ('s, 't, 'a, 'b) lens
  and type ('s, 't, 'a, 'b) y = ('s, 't, 'a, 'b) lens
  and type ('s, 't, 'a, 'b) z = ('s, 't, 'a, 'b) lens

implicit module TraversalLens_Composable: Composable
  with type ('s, 't, 'a, 'b) x = ('s, 't, 'a, 'b) traversal
  and type ('s, 't, 'a, 'b) y = ('s, 't, 'a, 'b) lens
  and type ('s, 't, 'a, 'b) z = ('s, 't, 'a, 'b) traversal

implicit module LensTraversal_Composable: Composable
  with type ('s, 't, 'a, 'b) x = ('s, 't, 'a, 'b) lens
  and type ('s, 't, 'a, 'b) y = ('s, 't, 'a, 'b) traversal
  and type ('s, 't, 'a, 'b) z = ('s, 't, 'a, 'b) traversal

implicit module Traversal_Composable: Composable
  with type ('s, 't, 'a, 'b) x = ('s, 't, 'a, 'b) traversal
  and type ('s, 't, 'a, 'b) y = ('s, 't, 'a, 'b) traversal
  and type ('s, 't, 'a, 'b) z = ('s, 't, 'a, 'b) traversal

val ( *** ) : {C: Composable} -> ('s, 't, 'p, 'q) C.x -> ('p, 'q, 'a, 'b) C.y -> ('s, 't, 'a, 'b) C.z
(** The operator *** performs composition of functional references.
    You can read it like `.` in a C-family imperative programming language.
    (In Haskell, this is also just `.` - function composition)

    For example, T2._1 *** T2._2 focuses on the second element of the first element of a pair.

    Due to limitations in the OCaml type system, *** must be implemented separately for each pair of input types.
    This is done by instances of the module type Composable.
 *)

type ('a, 's, 'x) getter = ('x -> ('a, 'x) const) -> ('s -> ('a, 's) const)
(** A getter is a read-only functional reference. It is a specialisation of a lens. *)

module type Getter = sig
  type a
  type 's t
  val convert : 's t -> (a, 's, a) getter
end
(** Class for types that can be converted to getters. This includes lenses, and some traversals. *)

implicit module Lens_Getter {A: Any} : Getter
  with type a = A.t_for_any
  and type 's t = {F: Functor} -> (A.t_for_any -> A.t_for_any F.t) -> ('s -> 's F.t)

implicit module Traversal_Getter {A: Monoid} : Getter
  with type a = A.t
  and type 's t = {F: Applicative} -> (A.t -> A.t F.t) -> ('s -> 's F.t)
(** A traversal can only be used as a getter if the type of its focus is a monoid.
    This is because the traversal might focus on any number of elements, but `get`
    can only return a single value, so we need some way to combine those elements.
 *)

val get : {L: Getter} -> 's L.t -> 's -> L.a
(** `get` applies a getter. For example, `get T2._2 ("hi", 5) = 5` *)

type ('s, 't, 'a, 'b) setter = ('a -> 'b identity) -> ('s -> 't identity)
(** A setter is a write-only functional reference. It is a specialisation of a lens. *)

module type Setter = sig
  type ('s, 't, 'a, 'b) t
  val convert : ('s, 't, 'a, 'b) t -> ('s, 't, 'a, 'b) setter
end
(** Class for types that can be converted to setters. This includes lenses and traversals. *)

implicit module Lens_Setter : Setter
  with type ('s, 't, 'a, 'b) t = ('s, 't, 'a, 'b) lens

implicit module Traversal_Setter : Setter
  with type ('s, 't, 'a, 'b) t = ('s, 't, 'a, 'b) traversal

val set : {L: Setter} -> ('s, 't, 'a, 'b) L.t -> 'b -> 's -> 't
(** `set` applies a setter. For example, `set T2._2 5 ("hi", "five") = ("hi", 5)` *)

val traversed : {T: Traversable} -> ('a T.t, 'b T.t, 'a, 'b) traversal
(** `traversed` constructs a traversal which focuses on every element of a `Traversable`. *)

(** Below are modules containing lenses which focus on specific elements of tuples.
    They have consistent names: to focus on the x'th element of a tuple of size y, use Ty._x
    (e.g. for the first element of a pair, T2._1)

    These are currently defined only up to tuples of size 4.
 *)

module T2: sig
  val _1: ('a * 'b, 'a2 * 'b, 'a, 'a2) lens
  (** T2._1 focuses on the first element of a pair *)

  val _2: ('a * 'b, 'a * 'b2, 'b, 'b2) lens
  (** T2._1 focuses on the second element of a pair *)
end
(** T2 contains lenses for focusing on elements of pairs (2-tuples) *)

module T3: sig
  val _1: ('a * 'b * 'c, 'a2 * 'b * 'c, 'a, 'a2) lens
  (** T3._1 focuses on the first element of a 3-tuple *)

  val _2: ('a * 'b * 'c, 'a * 'b2 * 'c, 'b, 'b2) lens
  (** T3._2 focuses on the second element of a 3-tuple *)

  val _3: ('a * 'b * 'c, 'a * 'b * 'c2, 'c, 'c2) lens
  (** T3._3 focuses on the third element of a 3-tuple *)
end
(** T3 contains lenses for focusing on elements of 3-tuples *)

module T4: sig
  val _1: ('a * 'b * 'c * 'd, 'a2 * 'b * 'c * 'd, 'a, 'a2) lens
  (** T4._1 focuses on the first element of a 4-tuple *)

  val _2: ('a * 'b * 'c * 'd, 'a * 'b2 * 'c * 'd, 'b, 'b2) lens
  (** T4._2 focuses on the second element of a 4-tuple *)

  val _3: ('a * 'b * 'c * 'd, 'a * 'b * 'c2 * 'd, 'c, 'c2) lens
  (** T4._3 focuses on the third element of a 4-tuple *)

  val _4: ('a * 'b * 'c * 'd, 'a * 'b * 'c * 'd2, 'd, 'd2) lens
  (** T4._4 focuses on the fourth element of a 4-tuple *)
end
(** T4 contains lenses for focusing on elements of 4-tuples *)

module type Indexed = sig
  type index
  (** `index` is the type used to index the container - e.g. for lists, that integers *)

  type value
  (** `value` is the type inside the container *)

  type t
  (** `t` is the type of the container itself *)

  val index : index -> (t, value) traversal'
  (** `index` takes an index and returns a traversal focusing on the referenced element of a container.
      It returns a traversal instead of a lens, as it can focus on 0 items if the index does not exist in the container *)
end
(** Indexed represents containers which use indexing to get values of the same type.
    This includes lists, strings, maps, homogeneous tuples, etc.
 *)

implicit module ListIndexed {A: Any}:
  Indexed with type index = int and type value = A.t_for_any and type t = A.t_for_any list
(** Allows lists to be indexed.
    Warning: indexing takes linear time!
 *)

implicit module BytesIndexed: Indexed
  with type index = int and type value = char and type t = bytes
(** Allows bytes/strings to be indexed.
    Warning: doesn't work with Unicode strings - indexing is done by bytes only!
 *)

val index : {I: Indexed} -> I.index -> (I.t, I.value) traversal'
(** `index` takes an index and returns a traversal focusing on the referenced element of a container.
    It returns a traversal instead of a lens, as it can focus on 0 items if the index does not exist in the container *)

val getMaybe : ('s, 's, 'a, 'a) traversal -> 's -> 'a option
(** Gets the first item focused on by a traversal, or None if the traversal finds none. *)
