module type S = sig
  type c
  type t = { x : c; y : c }

  val zero : t
  val create : c -> c -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val x : t -> c
  val y : t -> c
  val ( + ) : t -> c -> t
  val ( - ) : t -> c -> t
  val ( * ) : t -> c -> t
  val ( / ) : t -> c -> t
  val ( ++ ) : t -> t -> t
  val ( -- ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val ( // ) : t -> t -> t
  val of_pair : c * c -> t
  val to_pair : t -> c * c
  val to_string : t -> string
end

module Make (Coordinate : Coordinate.S) : sig
  include S with type c = Coordinate.t
end = struct
  module Coordinate = Coordinate
  open Coordinate

  type c = Coordinate.t
  type t = { x : c; y : c }

  let zero = { x = zero; y = zero }
  let create x y = { x; y }
  let x { x; _ } = x
  let y { y; _ } = y

  let compare left right =
    match compare left.x right.x with 0 -> compare left.y right.y | r -> r

  let equal left right = equal left.x right.x && equal left.y right.y
  let ( + ) { x; y } v = { x = add x v; y = add y v }
  let ( - ) { x; y } v = { x = sub x v; y = sub y v }
  let ( * ) { x; y } v = { x = mul x v; y = mul y v }
  let ( / ) { x; y } v = { x = div x v; y = div y v }
  let ( ++ ) left right = { x = add left.x right.x; y = add left.y right.y }
  let ( -- ) left right = { x = sub left.x right.x; y = sub left.y right.y }
  let ( ** ) left right = { x = mul left.x right.x; y = mul left.y right.y }
  let ( // ) left right = { x = div left.x right.x; y = div left.y right.y }

  let to_string { x; y } =
    Printf.sprintf "{ x = %s; y = %s }" (to_string x) (to_string y)

  let of_pair (x, y) = create x y
  let to_pair { x; y } = (x, y)
end
