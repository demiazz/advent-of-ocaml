let parse input =
  let process = function '(' -> Some 1 | ')' -> Some (-1) | _ -> None in
  input |> Parse.chars_of process

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
