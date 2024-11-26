module type S = sig
  include Point.S

  val create : c -> c -> t
  val x : t -> c
  val y : t -> c
  val map : (c -> 'a) -> t -> 'a * 'a
end

module Make (Coordinate : Coordinate.S) : sig
  include S with type c = Coordinate.t
end
