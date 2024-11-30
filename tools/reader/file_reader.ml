let chars_of input =
  let read () =
    try
      let ch = input_char input in
      match ch with
      | '\n' -> None
      | _ -> Some (ch, ())
    with
    | End_of_file -> None
  in
  Seq.unfold read ()
;;

let bytes_of input = input |> chars_of |> Seq.map Char.code

let lines_of input =
  let read () =
    try
      let line = input_line input in
      Some (line, ())
    with
    | End_of_file -> None
  in
  Seq.unfold read ()
;;
