exception Invalid_input

val chars_of : (char -> 'a option) -> Reader.t -> 'a Seq.t
