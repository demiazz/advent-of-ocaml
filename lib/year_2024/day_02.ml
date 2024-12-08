let parse input =
  let process line =
    let xs = line |> String.split_on_char ' ' |> List.map int_of_string in
    Some xs
  in
  input |> Parse.lines_of process
;;

let validate_with_bounds min max xs =
  let is_invalid (x, y) =
    let diff = x - y in
    diff < min || diff > max
  in
  xs |> List.to_seq |> Knife.Seq.pairs_of |> Knife.Seq.exists ~f:is_invalid |> not
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
  let remove target = Knife.List.remove_at ~index:target xs in
  xs |> Knife.List.indices |> Seq.map remove
;;

let check_without_tolerate min max xs = validate min max xs

let check_with_tolerate min max xs =
  let check = check_without_tolerate min max in
  check xs || to_tolerated xs |> Knife.Seq.exists ~f:check
;;

let count min max tolerate input =
  let filter =
    if tolerate then check_with_tolerate min max else check_without_tolerate min max
  in
  input |> parse |> Seq.filter filter |> Seq.length |> string_of_int
;;

let part_one = count 1 3 false

let part_two = count 1 3 true
