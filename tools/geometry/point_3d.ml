module type S = sig
  include Point.S

  val create : c -> c -> c -> t
  val x : t -> c
  val y : t -> c
  val z : t -> c
  val map : (c -> 'a) -> t -> 'a * 'a * 'a
end

module Make (Coordinate : Coordinate.S) : sig
  include S with type c = Coordinate.t
end = struct
  open Coordinate

  type c = Coordinate.t
  type t = { x : c; y : c; z : c }

  let zero = { x = zero; y = zero; z = zero }
  let create x y z = { x; y; z }
  let x { x; _ } = x
  let y { y; _ } = y
  let z { z; _ } = z

  let compare left right =
    let x = compare left.x right.x in
    if x = 0 then
      let y = compare left.y right.y in
      if y = 0 then compare left.z right.z else y
    else x

  let equal left right =
    equal left.x right.x && equal left.y right.y && equal left.z right.z

  let map f { x; y; z } = (f x, f y, f z)
  let ( + ) { x; y; z } v = { x = add x v; y = add y v; z = add z v }
  let ( - ) { x; y; z } v = { x = sub x v; y = sub y v; z = add z v }
  let ( * ) { x; y; z } v = { x = mul x v; y = mul y v; z = add z v }
  let ( / ) { x; y; z } v = { x = div x v; y = div y v; z = add z v }

  let ( ++ ) left right =
    { x = add left.x right.x; y = add left.y right.y; z = add left.z right.z }

  let ( -- ) left right =
    { x = sub left.x right.x; y = sub left.y right.y; z = add left.z right.z }

  let ( ** ) left right =
    { x = mul left.x right.x; y = mul left.y right.y; z = add left.z right.z }

  let ( // ) left right =
    { x = div left.x right.x; y = add left.y right.y; z = add left.z right.z }

  let to_string { x; y; z } =
    Printf.sprintf "{ x = %s; y = %s; z = %s }" (to_string x) (to_string y)
      (to_string z)
end
