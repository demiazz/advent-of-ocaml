let parse input =
  let process line =
    let xs = line |> String.split_on_char ' ' |> List.map int_of_string in
    Some xs
  in
  input |> Parse.lines_of process
;;

let validate_with_bounds min max =
  let rec is_valid index = function
    | [] -> true
    | _ :: [] -> true
    | x :: y :: xs ->
      let diff = x - y in
      if diff >= min && diff <= max then is_valid (index + 1) (y :: xs) else false
  in
  is_valid 0
;;

let validate min max xs =
  match xs with
  | [] -> true
  | _ :: [] -> true
  | x :: y :: _ when x = y -> false
  | x :: y :: _ when x < y -> validate_with_bounds (-1 * max) (-1 * min) xs
  | _ -> validate_with_bounds min max xs
;;

let to_tolerated xs =
  let remove target = List.filteri (fun index _ -> index <> target) xs in
  Seq.ints 0 |> Seq.take (List.length xs) |> Seq.map remove
;;

let check_without_tolerate min max xs = validate min max xs

let check_with_tolerate min max xs =
  match validate min max xs with
  | true -> true
  | false ->
    let some xxs = Seq.find (fun xs -> validate min max xs) xxs in
    (match to_tolerated xs |> some with
     | Some _ -> true
     | None -> false)
;;

let count min max tolerate input =
  let filter =
    if tolerate then check_with_tolerate min max else check_without_tolerate min max
  in
  input |> parse |> Seq.filter filter |> Seq.length |> string_of_int
;;

let part_one = count 1 3 false

let part_two = count 1 3 true
