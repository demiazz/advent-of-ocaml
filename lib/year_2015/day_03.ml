module Point = Geometry.Int_point_2d
module Point_set = Set.Make (Point)

let parse input =
  let to_point = function
    | '^' -> Some (Point.create 0 1)
    | 'v' -> Some (Point.create 0 (-1))
    | '>' -> Some (Point.create 1 0)
    | '<' -> Some (Point.create (-1) 0)
    | _ -> None
  in
  input |> Parse.chars_of to_point

let visit input move =
  let initial = Point_set.singleton Point.zero in
  let visited =
    input |> parse |> Seq.fold_left move initial |> Point_set.cardinal
  in
  string_of_int visited

let part_one input =
  let open Point in
  let santa = ref Point.zero in
  let move set direction =
    let next = !santa ++ direction in
    santa := next;
    Point_set.add next set
  in
  visit input move

let part_two input =
  let open Point in
  let who = ref false in
  let santa = ref Point.zero in
  let robo = ref Point.zero in
  let move set direction =
    let position = if !who then santa else robo in
    let next = !position ++ direction in
    position := next;
    who := not !who;
    Point_set.add next set
  in
  visit input move
