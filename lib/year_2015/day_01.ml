let parse input =
  let process = function
    | '(' -> Some 1
    | ')' -> Some (-1)
    | _ -> None
  in
  input |> Parse.chars_of process
;;

let part_one input = input |> parse |> Knife.Seq.sum |> string_of_int

let part_two input =
  let position =
    input |> parse |> Seq.scan ( + ) 0 |> Seq.find_index (Stdlib.( = ) (-1))
  in
  match position with
  | Some index -> index |> string_of_int
  | None -> failwith "Santa never enter the basement"
;;
