module Int_point_2d : sig
  type t = int * int

  val compare : t -> t -> int
  val map : (int -> 'a) -> t -> 'a * 'a
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
end
