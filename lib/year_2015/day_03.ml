module Int_point_2d = struct
  type t = int * int

  let compare = Stdlib.compare
  let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  let sub (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
end

module Point_set = Set.Make (Int_point_2d)

let parse input =
  let to_point char =
    match char with
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
  let santa = ref (0, 0) in
  let move set direction =
    let next = Int_point_2d.add !santa direction in
    santa := next;
    set |> Point_set.add next
  in
  visit input move

let part_two input =
  let who = ref false in
  let santa = ref (0, 0) in
  let robo = ref (0, 0) in
  let move set direction =
    let position = if !who then santa else robo in
    let next = Int_point_2d.add !position direction in
    position := next;
    who := not !who;
    set |> Point_set.add next
  in
  visit input move
