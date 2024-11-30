module Point = Point_3d.Make (Int)

let pair_t =
  let pp fmt point = Format.fprintf fmt "%s" (Point.to_string point) in
  Alcotest.testable pp Point.equal

let zero_test_cases =
  let samples = [ "is point with zero coordinates" ] in
  let make name =
    let run () =
      Alcotest.(check pair_t) "are equal" (Point.create 0 0 0) Point.zero
    in
    Alcotest.test_case name `Quick run
  in
  List.map make samples

let create_test_cases =
  let samples = [ ("creates a point with given coordinates", 6, 4, 2) ] in
  let make (name, x, y, z) =
    let run () =
      let actual = Point.create x y z in
      Alcotest.(check int) "are equal" x (Point.x actual);
      Alcotest.(check int) "are equal" y (Point.y actual);
      Alcotest.(check int) "are equal" z (Point.z actual)
    in
    Alcotest.test_case name `Quick run
  in
  List.map make samples

let compare_test_cases =
  let samples =
    [
      ( "returns 0 when coordinates of both points are equal",
        (4, 2, 5),
        (4, 2, 5),
        0 );
      ( "returns 1 when coordinates when x of first point is greater",
        (5, 2, 5),
        (4, 3, 5),
        1 );
      ( "returns 1 when coordinates when x of first point is greater",
        (5, 2, 5),
        (4, 2, 5),
        1 );
      ( "returns 1 when coordinates when x of first point is greater",
        (5, 3, 5),
        (4, 2, 5),
        1 );
      ( "returns 1 when coordinates when x of first point is greater",
        (5, 2, 5),
        (4, 2, 6),
        1 );
      ( "returns 1 when coordinates when x of first point is greater",
        (5, 2, 6),
        (4, 2, 5),
        1 );
      ( "returns -1 when coordinates when x of first point is lesser",
        (3, 2, 5),
        (4, 3, 5),
        -1 );
      ( "returns -1 when coordinates when x of first point is lesser",
        (3, 2, 5),
        (4, 2, 5),
        -1 );
      ( "returns -1 when coordinates when x of first point is lesser",
        (3, 3, 5),
        (4, 2, 5),
        -1 );
      ( "returns -1 when coordinates when x of first point is lesser",
        (3, 2, 5),
        (4, 2, 6),
        -1 );
      ( "returns -1 when coordinates when x of first point is lesser",
        (3, 2, 6),
        (4, 2, 5),
        -1 );
      ( "returns 1 when coordinates when y of first point is greater",
        (4, 3, 4),
        (4, 2, 5),
        1 );
      ( "returns 1 when coordinates when y of first point is greater",
        (4, 3, 5),
        (4, 2, 5),
        1 );
      ( "returns 1 when coordinates when y of first point is greater",
        (4, 3, 6),
        (4, 2, 5),
        1 );
      ( "returns -1 when coordinates when y of first point is lesser",
        (4, 1, 4),
        (4, 2, 5),
        -1 );
      ( "returns -1 when coordinates when y of first point is lesser",
        (4, 1, 5),
        (4, 2, 5),
        -1 );
      ( "returns -1 when coordinates when y of first point is lesser",
        (4, 1, 6),
        (4, 2, 5),
        -1 );
      ( "returns 1 when coordinates when z of first point is greater",
        (4, 2, 6),
        (4, 2, 5),
        1 );
      ( "returns -1 when coordinates when z of first point is lesser",
        (4, 2, 4),
        (4, 2, 5),
        -1 );
    ]
  in
  let make (name, left, right, expected) =
    let run () =
      let actual =
        Point.compare (Point.of_triple left) (Point.of_triple right)
      in
      Alcotest.(check int) "are equal" expected actual
    in
    Alcotest.test_case name `Quick run
  in
  List.map make samples

let equal_test_cases =
  let samples =
    [
      ( "returns true when coordinates of both points are equal",
        (4, 2, 6),
        (4, 2, 6),
        true );
      ( "returns false when x coordinates of both points aren't equal",
        (3, 3, 5),
        (4, 2, 5),
        false );
      ( "returns false when y coordinates of both points aren't equal",
        (4, 3, 5),
        (4, 2, 5),
        false );
      ( "returns false when z coordinates of both points aren't equal",
        (4, 2, 5),
        (4, 2, 6),
        false );
    ]
  in
  let make (name, left, right, expected) =
    let run () =
      let actual = Point.equal (Point.of_triple left) (Point.of_triple right) in
      Alcotest.(check bool) "are equal" expected actual
    in
    Alcotest.test_case name `Quick run
  in
  List.map make samples

let min_max_test_cases fn =
  let make (name, left, right, expected) =
    let run () =
      let actual = fn (Point.of_triple left) (Point.of_triple right) in
      Alcotest.(check int) "are equal" expected actual
    in
    Alcotest.test_case name `Quick run
  in
  List.map make

let min_x_test_cases =
  let samples =
    [
      ("returns x coordinate when both are equal", (4, 0, 0), (4, 0, 0), 4);
      ("returns left x coordinate when left is lesser", (3, 0, 0), (4, 0, 0), 3);
      ( "returns right x coordinate when right is lesser",
        (4, 0, 0),
        (3, 0, 0),
        3 );
    ]
  in
  min_max_test_cases Point.min_x samples

let min_y_test_cases =
  let samples =
    [
      ("returns y coordinate when both are equal", (0, 4, 0), (0, 4, 0), 4);
      ("returns left y coordinate when left is lesser", (0, 3, 0), (0, 4, 0), 3);
      ( "returns right y coordinate when right is lesser",
        (0, 4, 0),
        (0, 3, 0),
        3 );
    ]
  in
  min_max_test_cases Point.min_y samples

let min_z_test_cases =
  let samples =
    [
      ("returns z coordinate when both are equal", (0, 0, 4), (0, 0, 4), 4);
      ("returns left z coordinate when left is lesser", (0, 0, 3), (0, 0, 4), 3);
      ( "returns right z coordinate when right is lesser",
        (0, 0, 4),
        (0, 0, 3),
        3 );
    ]
  in
  min_max_test_cases Point.min_z samples

let max_x_test_cases =
  let samples =
    [
      ("returns x coordinate when both are equal", (4, 0, 0), (4, 0, 0), 4);
      ("returns right x coordinate when left is lesser", (3, 0, 0), (4, 0, 0), 4);
      ("returns left x coordinate when right is lesser", (4, 0, 0), (3, 0, 0), 4);
    ]
  in
  min_max_test_cases Point.max_x samples

let max_y_test_cases =
  let samples =
    [
      ("returns y coordinate when both are equal", (0, 4, 0), (0, 4, 0), 4);
      ("returns right y coordinate when left is lesser", (0, 3, 0), (0, 4, 0), 4);
      ("returns left y coordinate when right is lesser", (0, 4, 0), (0, 3, 0), 4);
    ]
  in
  min_max_test_cases Point.max_y samples

let max_z_test_cases =
  let samples =
    [
      ("returns z coordinate when both are equal", (0, 0, 4), (0, 0, 4), 4);
      ("returns right z coordinate when left is lesser", (0, 0, 3), (0, 0, 4), 4);
      ("returns left z coordinate when right is lesser", (0, 0, 4), (0, 0, 3), 4);
    ]
  in
  min_max_test_cases Point.max_z samples

let coordinate_test_cases fn =
  let make (name, initial, expected) =
    let run () =
      let actual = fn (Point.of_triple initial) in
      Alcotest.(check int) "are equal" expected actual
    in
    Alcotest.test_case name `Quick run
  in
  List.map make

let x_test_cases =
  let samples = [ ("returns x coordinate of point", (6, 4, 2), 6) ] in
  coordinate_test_cases Point.x samples

let y_test_cases =
  let samples = [ ("returns y coordinate of point", (6, 4, 2), 4) ] in
  coordinate_test_cases Point.y samples

let z_test_cases =
  let samples = [ ("returns z coordinate of point", (6, 4, 2), 2) ] in
  coordinate_test_cases Point.z samples

let point_value_test_cases fn =
  let make (name, initial, value, expected) =
    let run () =
      let actual = fn (Point.of_triple initial) value in
      Alcotest.(check pair_t) "are equal" (Point.of_triple expected) actual
    in
    Alcotest.test_case name `Quick run
  in
  List.map make

let add_value_test_cases =
  let samples =
    [ ("adds a value to both coordinates", (2, 0, -4), 2, (4, 2, -2)) ]
  in
  point_value_test_cases Point.( + ) samples

let sub_value_test_cases =
  let samples =
    [ ("substracts a value to both coordinates", (4, 2, -4), 2, (2, 0, -6)) ]
  in
  point_value_test_cases Point.( - ) samples

let mul_value_test_cases =
  let samples =
    [ ("multiplies both coordinates on a value", (4, 2, -4), 2, (8, 4, -8)) ]
  in
  point_value_test_cases Point.( * ) samples

let div_value_test_cases =
  let samples =
    [ ("divides both coordinates on a value", (4, 2, -4), 2, (2, 1, -2)) ]
  in
  point_value_test_cases Point.( / ) samples

let points_test_cases fn =
  let make (name, left, right, expected) =
    let run () =
      let actual = fn (Point.of_triple left) (Point.of_triple right) in
      Alcotest.(check pair_t) "are equal" (Point.of_triple expected) actual
    in
    Alcotest.test_case name `Quick run
  in
  List.map make

let add_points_test_cases =
  let samples =
    [ ("adds a value to both coordinates", (2, 0, -4), (-4, 2, 8), (-2, 2, 4)) ]
  in
  points_test_cases Point.( ++ ) samples

let sub_points_test_cases =
  let samples =
    [
      ( "substracts a value to both coordinates",
        (2, 0, -4),
        (-2, -2, 8),
        (4, 2, -12) );
    ]
  in
  points_test_cases Point.( -- ) samples

let mul_points_test_cases =
  let samples =
    [
      ( "multiplies a value to both coordinates",
        (2, 4, -4),
        (-4, 2, 8),
        (-8, 8, -32) );
    ]
  in
  points_test_cases Point.( ** ) samples

let div_points_test_cases =
  let samples =
    [
      ( "divides a value to both coordinates",
        (8, 4, -4),
        (-4, 2, 2),
        (-2, 2, -2) );
    ]
  in
  points_test_cases Point.( // ) samples

let of_triple_test_cases =
  let samples = [ ("creates point from triple of coordinates", (8, 3, 4)) ] in
  let make (name, initial) =
    let run () =
      let actual = Point.of_triple initial in
      let x, y, z = initial in
      Alcotest.(check int) "are equal" x (Point.x actual);
      Alcotest.(check int) "are equal" y (Point.y actual);
      Alcotest.(check int) "are equal" z (Point.z actual)
    in
    Alcotest.test_case name `Quick run
  in
  List.map make samples

let to_triple_test_cases =
  let samples = [ ("creates point from triple of coordinates", (8, 3, 4)) ] in
  let make (name, initial) =
    let run () =
      let actual = initial |> Point.of_triple |> Point.to_triple in
      Alcotest.(check (triple int int int)) "are equal" initial actual
    in
    Alcotest.test_case name `Quick run
  in
  List.map make samples

let to_string_test_cases =
  let samples =
    [
      ( "creates string representation of point",
        (8, 5, -4),
        "{ x = 8; y = 5; z = -4 }" );
    ]
  in
  let make (name, initial, expected) =
    let run () =
      let actual = initial |> Point.of_triple |> Point.to_string in
      Alcotest.(check string) "are equal" expected actual
    in
    Alcotest.test_case name `Quick run
  in
  List.map make samples

let () =
  let open Alcotest in
  run "Point_3d"
    [
      ("zero      : t", zero_test_cases);
      ("create    : c -> c -> t", create_test_cases);
      ("compare   : t -> t -> int", compare_test_cases);
      ("equal     : t -> t -> bool", equal_test_cases);
      ("min_x     : t -> t -> c", min_x_test_cases);
      ("min_y     : t -> t -> c", min_y_test_cases);
      ("min_z     : t -> t -> c", min_z_test_cases);
      ("max_x     : t -> t -> c", max_x_test_cases);
      ("max_y     : t -> t -> c", max_y_test_cases);
      ("max_z     : t -> t -> c", max_z_test_cases);
      ("x         : t -> c", x_test_cases);
      ("y         : t -> c", y_test_cases);
      ("z         : t -> c", z_test_cases);
      ("add       : t -> c -> t", add_value_test_cases);
      ("sub       : t -> c -> t", sub_value_test_cases);
      ("mul       : t -> c -> t", mul_value_test_cases);
      ("div       : t -> c -> t", div_value_test_cases);
      ("add       : t -> t -> t", add_points_test_cases);
      ("sub       : t -> t -> t", sub_points_test_cases);
      ("mul       : t -> t -> t", mul_points_test_cases);
      ("div       : t -> t -> t", div_points_test_cases);
      ("of_triple : (c * c * c) -> t", of_triple_test_cases);
      ("to_triple : t -> (c * c * c)", to_triple_test_cases);
      ("to_string : t -> string", to_string_test_cases);
    ]
