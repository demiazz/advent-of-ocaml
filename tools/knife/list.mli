type 'a t = 'a list

val indices : 'a list -> int Seq.t

val find_first_opt : f:('a -> bool) -> 'a list -> 'a option

val find_last_opt : f:('a -> bool) -> 'a list -> 'a option

val remove_at : index:int -> 'a list -> 'a list
