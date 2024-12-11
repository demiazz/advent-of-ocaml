type t = string

val indices : t -> int Seq.t

val fold_lefti : f:('acc -> int -> char -> 'acc) -> initial:'acc -> string -> 'acc
