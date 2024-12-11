module Point = Point_2d.Make (Int)

type direction =
  | Top
  | Right
  | Bottom
  | Left

module Walk : sig
  type t

  val parse : Reader.t -> t

  val add_device : int -> int -> t -> t

  val walk : t -> (direction * Point.t * Point.t * bool) Seq.t

  val visited : t -> Point.t list
end = struct
  module Map = Map.Make (Int)
  module Set = Set.Make (Point)

  type t =
    { xs : int list Map.t
    ; ys : int list Map.t
    ; width : int
    ; height : int
    ; direction : direction
    ; position : Point.t
    }

  let empty =
    { xs = Map.empty
    ; ys = Map.empty
    ; direction = Top
    ; position = Point.zero
    ; width = 0
    ; height = 0
    }
  ;;

  let rec add_to_list n xs =
    match xs with
    | [] -> [ n ]
    | x :: xs ->
      if n = x then x :: xs else if n < x then n :: x :: xs else x :: add_to_list n xs
  ;;

  let add_to_map key data map =
    let aux = function
      | Some xs -> Some (add_to_list data xs)
      | None -> Some [ data ]
    in
    Map.update key aux map
  ;;

  let add_device x y state =
    { state with xs = add_to_map x y state.xs; ys = add_to_map y x state.ys }
  ;;

  let set_location direction position state =
    if state.direction = direction && Point.equal state.position position
    then state
    else { state with direction; position }
  ;;

  let set_width width state = if state.width = width then state else { state with width }

  let set_height height state =
    if state.height = height then state else { state with height }
  ;;

  let parse input =
    let aux x y symbol state =
      let next =
        match symbol with
        | '#' -> add_device x y state
        | '^' -> set_location Top (Point.create x y) state
        | _ -> state
      in
      set_width (x + 1) next
    in
    let parse_line state y line =
      Knife.String.fold_lefti
        ~f:(fun state x symbol -> aux x y symbol state)
        ~initial:state
        line
      |> set_height (y + 1)
    in
    input |> Reader.lines_of |> Knife.Seq.fold_lefti ~f:parse_line ~initial:empty
  ;;

  let backward_position main cross bounds =
    let bounds = Map.find_opt cross bounds in
    let find_bound = Knife.List.find_last_opt ~f:(Stdlib.( > ) main) in
    let bound = Option.bind bounds find_bound in
    match bound with
    | Some bound -> bound + 1, cross
    | None -> 0, cross
  ;;

  let top_position { position; xs; _ } =
    let x, y = Point.to_pair position in
    let main, cross = backward_position y x xs in
    Point.create cross main
  ;;

  let left_position { position; ys; _ } =
    let x, y = Point.to_pair position in
    let main, cross = backward_position x y ys in
    Point.create main cross
  ;;

  let forward_position main cross bounds size =
    let bounds = Map.find_opt cross bounds in
    let find_bound = Knife.List.find_first_opt ~f:(Stdlib.( < ) main) in
    let bound = Option.bind bounds find_bound in
    match bound with
    | Some bound -> bound - 1, cross
    | None -> size - 1, cross
  ;;

  let bottom_position { position; height; xs; _ } =
    let x, y = Point.to_pair position in
    let main, cross = forward_position y x xs height in
    Point.create cross main
  ;;

  let right_position { position; ys; width; _ } =
    let x, y = Point.to_pair position in
    let main, cross = forward_position x y ys width in
    Point.create main cross
  ;;

  let move state =
    let direction, position =
      match state.direction with
      | Top -> Right, top_position state
      | Right -> Bottom, right_position state
      | Bottom -> Left, bottom_position state
      | Left -> Top, left_position state
    in
    set_location direction position state
  ;;

  let is_out { height; position; width; _ } =
    let x, y = Point.to_pair position in
    x = 0 || x = width - 1 || y = 0 || y = height - 1
  ;;

  let walk initial =
    let aux state =
      if is_out state
      then None
      else (
        let next = move state in
        let is_out = is_out state in
        Some ((state.direction, state.position, next.position, is_out), next))
    in
    Seq.unfold aux initial
  ;;

  let visit (direction, initial, final, _) =
    let initial_x, initial_y = Point.to_pair initial in
    let final_x, final_y = Point.to_pair final in
    let distance, xs, ys =
      match direction with
      | Top -> initial_y - final_y, Seq.repeat initial_x, Seq.ints final_y
      | Right -> final_x - initial_x, Seq.ints initial_x, Seq.repeat initial_y
      | Bottom -> final_y - initial_y, Seq.repeat initial_x, Seq.ints initial_y
      | Left -> initial_x - final_x, Seq.ints final_x, Seq.repeat initial_y
    in
    Seq.zip xs ys |> Seq.take (distance + 1) |> Seq.map Point.of_pair
  ;;

  let visited initial =
    let aux visited position = Set.add position visited in
    initial
    |> walk
    |> Seq.map visit
    |> Seq.concat
    |> Seq.fold_left aux Set.empty
    |> Set.to_list
  ;;
end

module Part_one = struct
  let distance input = input |> Walk.parse |> Walk.visited |> List.length
end

module Part_two = struct
  module Transition = struct
    type t = direction * Point.t

    let compare = Stdlib.compare
  end

  module Set = Set.Make (Transition)

  let is_infinite initial =
    let visited = ref Set.empty in
    let check (direction, initial, _, _) =
      if Set.mem (direction, initial) !visited
      then true
      else (
        visited := Set.add (direction, initial) !visited;
        false)
    in
    Walk.walk initial |> Seq.exists check
  ;;

  let points height width =
    let ys = Seq.ints 0 |> Seq.take height in
    let xs y = Seq.ints 0 |> Seq.take width |> Seq.map (fun x -> x, y) in
    ys |> Seq.map xs |> Seq.concat
  ;;

  let count input =
    let initial = Walk.parse input in
    let add_device position =
      let x, y = Point.to_pair position in
      Walk.add_device x y initial
    in
    initial
    |> Walk.visited
    |> List.map add_device
    |> List.filter is_infinite
    |> List.length
  ;;
end

let part_one input = input |> Part_one.distance |> string_of_int

let part_two input = input |> Part_two.count |> string_of_int
