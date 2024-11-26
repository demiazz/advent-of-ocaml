module type S = sig
  type c
  type t

  val zero : t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val ( + ) : t -> c -> t
  val ( - ) : t -> c -> t
  val ( * ) : t -> c -> t
  val ( / ) : t -> c -> t
  val ( ++ ) : t -> t -> t
  val ( -- ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val ( // ) : t -> t -> t
  val to_string : t -> string
end
