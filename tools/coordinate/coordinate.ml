module type S = sig
  type t

  val zero : t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val to_string : t -> string
end
