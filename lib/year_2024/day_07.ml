let parse input =
  let parse_args line =
    line |> String.trim |> String.split_on_char ' ' |> List.map int_of_string
  in
  let parse_equation line =
    match String.split_on_char ':' line with
    | [ left; right ] -> Some (int_of_string left, parse_args right)
    | _ -> None
  in
  input |> Parse.lines_of parse_equation
;;

let check_math (left, args) =
  let rec aux right args =
    match args with
    | [] -> left = right
    | arg :: rest ->
      if right > left then false else aux (right + arg) rest || aux (right * arg) rest
  in
  match args with
  | [] -> false
  | arg :: rest -> aux arg rest
;;

let check_math_and_concat (left, args) =
  let concat left right = string_of_int left ^ string_of_int right |> int_of_string in
  let rec aux right args =
    match args with
    | [] -> left = right
    | arg :: rest ->
      if right > left
      then false
      else aux (right + arg) rest || aux (right * arg) rest || aux (concat right arg) rest
  in
  match args with
  | [] -> false
  | arg :: rest -> aux arg rest
;;

let count check input =
  let aux acc (left, args) = if check (left, args) then acc + left else acc in
  input |> parse |> Seq.fold_left aux 0
;;

let part_one input = input |> count check_math |> string_of_int

let part_two input = input |> count check_math_and_concat |> string_of_int
