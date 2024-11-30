type 'a t

val empty : unit -> 'a t

val add : year:int -> day:int -> part:int -> value:'a -> 'a t -> 'a t

val all_years : 'a t -> int list

val all_days : year:int -> 'a t -> int list

val all_parts : year:int -> day:int -> 'a t -> bool * bool

val find_opt : year:int -> day:int -> part:int -> 'a t -> 'a option
