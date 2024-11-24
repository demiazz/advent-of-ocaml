type t = In_channel of in_channel | String of string

let from_in_channel ic = In_channel ic
let from_string s = String s

let bytes_of input =
  match input with
  | In_channel ic -> File_reader.bytes_of ic
  | String s -> String_reader.bytes_of s

let chars_of input =
  match input with
  | In_channel ic -> File_reader.chars_of ic
  | String s -> String_reader.chars_of s

let lines_of input =
  match input with
  | In_channel ic -> File_reader.lines_of ic
  | String s -> String_reader.lines_of s
