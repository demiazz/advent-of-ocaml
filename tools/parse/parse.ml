exception Invalid_input

let chars_of processor input =
  let process entry =
    match processor entry with
    | Some value -> value
    | None -> raise Invalid_input
  in
  input |> Reader.chars_of |> Seq.map process
