type 'a t = 'a Stdlib.Seq.t

val exists : f:('a -> bool) -> 'a t -> bool

val indices : 'a t -> int t

val sum : int t -> int

val fold_lefti : f:('acc -> int -> 'a -> 'acc) -> initial:'acc -> 'a t -> 'acc

val pairs_of : 'a t -> ('a * 'a) t

val triples_of : 'a t -> ('a * 'a * 'a) t
