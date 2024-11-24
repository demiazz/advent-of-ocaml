val years : int list
val days : year:int -> int list
val parts : year:int -> day:int -> bool * bool
val solve : year:int -> day:int -> part:int -> Reader.t -> string
