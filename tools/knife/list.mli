type 'a t = 'a list

val indices : 'a list -> int Seq.t

val remove_at : index:int -> 'a list -> 'a list
