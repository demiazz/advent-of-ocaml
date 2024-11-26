module type S = sig
  include Point.S

  val create : c -> c -> t
  val x : t -> c
  val y : t -> c
  val map : (c -> 'a) -> t -> 'a * 'a
end

module Make (Coordinate : Coordinate.S) : sig
  include S with type c = Coordinate.t
end = struct
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
  let map f { x; y } = (f x, f y)
  let ( + ) { x; y } v = { x = add x v; y = add y v }
  let ( - ) { x; y } v = { x = sub x v; y = sub y v }
  let ( * ) { x; y } v = { x = mul x v; y = mul y v }
  let ( / ) { x; y } v = { x = div x v; y = div y v }
  let ( ++ ) left right = { x = add left.x right.x; y = add left.y right.y }
  let ( -- ) left right = { x = sub left.x right.x; y = sub left.y right.y }
  let ( ** ) left right = { x = mul left.x right.x; y = mul left.y right.y }
  let ( // ) left right = { x = div left.x right.x; y = add left.y right.y }

  let to_string { x; y } =
    Printf.sprintf "{ x = %s; y = %s }" (to_string x) (to_string y)
end
