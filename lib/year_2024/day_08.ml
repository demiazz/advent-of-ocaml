module Point = Point_2d.Make (Int)

module Antennas : sig
  type t =
    { width : int
    ; height : int
    ; antennas : (Point.t * Point.t) list
    }

  val parse : Reader.t -> t

  val in_bounds : int -> int -> Point.t -> bool

  val unique_count : Point.t list -> int
end = struct
  module Map = Map.Make (Char)
  module Set = Set.Make (Point)

  type t =
    { width : int
    ; height : int
    ; antennas : (Point.t * Point.t) list
    }

  let parse_map input =
    let parse_line initial y line =
      let aux (antennas, _, _) x char =
        let width = x + 1 in
        let height = y + 1 in
        match char with
        | '.' -> antennas, width, height
        | char -> Map.add_to_list char (Point.create x y) antennas, width, height
      in
      Knife.String.fold_lefti ~f:aux ~initial line
    in
    let antennas, width, height =
      input
      |> Reader.lines_of
      |> Knife.Seq.fold_lefti ~f:parse_line ~initial:(Map.empty, 0, 0)
    in
    let aux (_, points) = if List.length points > 0 then Some points else None in
    antennas |> Map.bindings |> List.filter_map aux, width, height
  ;;

  let rec pairs_of xs =
    match xs with
    | [] -> []
    | x :: xs -> List.map (fun y -> x, y) xs @ pairs_of xs
  ;;

  let parse input =
    let aux antennas = pairs_of antennas in
    let antennas, width, height = input |> parse_map in
    { width; height; antennas = List.concat_map aux antennas }
  ;;

  let in_bounds width height antenna =
    let x, y = Point.to_pair antenna in
    x >= 0 && x < width && y >= 0 && y < height
  ;;

  let unique_count antinodes =
    let aux antinodes antinode = Set.add antinode antinodes in
    antinodes |> List.fold_left aux Set.empty |> Set.elements |> List.length
  ;;
end

module Part_one = struct
  let antinodes (left, right) =
    let open Point in
    let delta = right -- left in
    [ left -- delta; right ++ delta ]
  ;;

  let count input =
    let map = Antennas.parse input in
    map.antennas
    |> List.concat_map antinodes
    |> List.filter (Antennas.in_bounds map.width map.height)
    |> Antennas.unique_count
  ;;
end

module Part_two = struct
  let antinodes width height (left, right) =
    let delta = Point.( -- ) right left in
    let produce (from, op) =
      let next = op from delta in
      if Antennas.in_bounds width height next then Some (next, (next, op)) else None
    in
    let lefts = Seq.unfold produce (left, Point.( -- )) |> List.of_seq in
    let rights = Seq.unfold produce (right, Point.( ++ )) |> List.of_seq in
    [ left; right ] |> List.append lefts |> List.append rights
  ;;

  let count input =
    let map = Antennas.parse input in
    map.antennas
    |> List.concat_map (antinodes map.width map.height)
    |> Antennas.unique_count
  ;;
end

let part_one input = input |> Part_one.count |> string_of_int

let part_two input = input |> Part_two.count |> string_of_int
