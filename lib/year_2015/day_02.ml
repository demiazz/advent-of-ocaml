let parse input =
  let process line =
    match String.split_on_char 'x' line with
    | [ l; w; h ] -> Some (int_of_string l, int_of_string w, int_of_string h)
    | _ -> None
  in
  input |> Parse.lines_of process
;;

let pipe f (l, w, h) = l |> f w |> f h

let sum = pipe ( + )

let total_of f input = input |> parse |> Seq.map f |> Knife.Seq.sum |> string_of_int

let part_one input =
  let min = pipe min in
  let sides_of (l, w, h) = 2 * l * w, 2 * w * h, 2 * h * l in
  let square_of gift =
    let sides = sides_of gift in
    sum sides + (min sides / 2)
  in
  input |> total_of square_of
;;

let part_two input =
  let max = pipe max in
  let mul = pipe ( * ) in
  let sides_of gift = (sum gift - max gift) * 2 in
  let ribbon_of gift = sides_of gift + mul gift in
  input |> total_of ribbon_of
;;
