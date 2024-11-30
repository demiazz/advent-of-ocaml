exception Invalid_input

val chars_of : (char -> 'a option) -> Reader.t -> 'a Seq.t

val lines_of : (string -> 'a option) -> Reader.t -> 'a Seq.t
