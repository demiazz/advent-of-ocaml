let parse input =
  let to_int char =
    match char with '(' -> 1 | ')' -> -1 | _ -> raise Parser.Invalid_input
  in
  input |> Reader.chars_of |> Seq.map to_int

let part_one input = input |> parse |> Seq.fold_left ( + ) 0 |> string_of_int

let part_two input =
  let rec fold_until acc idx seq =
    match seq () with
    | Seq.Nil -> acc
    | Seq.Cons (x, next) ->
        let acc = acc + x in
        let next_idx = idx + 1 in
        if acc = -1 then next_idx else fold_until acc next_idx next
  in
  input |> parse |> fold_until 0 0 |> string_of_int
