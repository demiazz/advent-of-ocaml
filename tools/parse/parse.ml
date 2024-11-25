exception Invalid_input

let parse split process input =
  let handle entry =
    match process entry with Some value -> value | None -> raise Invalid_input
  in
  input |> split |> Seq.map handle

let chars_of process input = parse Reader.chars_of process input
let lines_of process input = parse Reader.lines_of process input
