module Point_set = Set.Make (Geometry.Int_point_2d)

let parse input =
  let to_point = function
    | '^' -> Some (0, 1)
    | 'v' -> Some (0, -1)
    | '>' -> Some (1, 0)
    | '<' -> Some (-1, 0)
    | _ -> None
  in
  input |> Parse.chars_of to_point

let visit input move =
  let initial = Point_set.singleton (0, 0) in
  let visited =
    input |> parse |> Seq.fold_left move initial |> Point_set.cardinal
  in
  string_of_int visited

let part_one input =
  let open Geometry.Int_point_2d in
  let santa = ref (0, 0) in
  let move set direction =
    let next = !santa + direction in
    santa := next;
    Point_set.add next set
  in
  visit input move

let part_two input =
  let open Geometry.Int_point_2d in
  let who = ref false in
  let santa = ref (0, 0) in
  let robo = ref (0, 0) in
  let move set direction =
    let position = if !who then santa else robo in
    let next = !position + direction in
    position := next;
    who := not !who;
    Point_set.add next set
  in
  visit input move
