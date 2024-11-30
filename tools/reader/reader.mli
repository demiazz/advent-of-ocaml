type t

val from_in_channel : in_channel -> t

val from_string : string -> t

val bytes_of : t -> int Seq.t

val chars_of : t -> char Seq.t

val lines_of : t -> string Seq.t
