module type S = sig
  type c
  type t

  val zero : t
  val create : c -> c -> c -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val min_x : t -> t -> c
  val min_y : t -> t -> c
  val min_z : t -> t -> c
  val max_x : t -> t -> c
  val max_y : t -> t -> c
  val max_z : t -> t -> c
  val x : t -> c
  val y : t -> c
  val z : t -> c
  val ( + ) : t -> c -> t
  val ( - ) : t -> c -> t
  val ( * ) : t -> c -> t
  val ( / ) : t -> c -> t
  val ( ++ ) : t -> t -> t
  val ( -- ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val ( // ) : t -> t -> t
  val of_triple : c * c * c -> t
  val to_triple : t -> c * c * c
  val to_string : t -> string
end

module Make (Coordinate : Coordinate.S) : sig
  include S with type c = Coordinate.t
end
