module Point = Point_3d.Make (Int)

let pair_t =
  let pp fmt point = Format.fprintf fmt "%s" (Point.to_string point) in
  Alcotest.testable pp Point.equal

let test_zero name =
  let run () =
    Alcotest.(check pair_t) "are equal" (Point.create 0 0 0) Point.zero
  in
  Alcotest.test_case name `Quick run

let test_create name x y z =
  let run () =
    let actual = Point.create x y z in
    Alcotest.(check pair_t) "are equal" { x; y; z } actual
  in
  Alcotest.test_case name `Quick run

let test_compare name left right expected =
  let run () =
    let actual = Point.compare left right in
    Alcotest.(check int) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_equal name left right expected =
  let run () =
    let actual = Point.equal left right in
    Alcotest.(check bool) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_x name initial expected =
  let run () =
    let actual = Point.x initial in
    Alcotest.(check int) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_y name initial expected =
  let run () =
    let actual = Point.y initial in
    Alcotest.(check int) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_z name initial expected =
  let run () =
    let actual = Point.z initial in
    Alcotest.(check int) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_add_value name initial value expected =
  let run () =
    let actual = Point.( + ) initial value in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_sub_value name initial value expected =
  let run () =
    let actual = Point.( - ) initial value in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_mul_value name initial value expected =
  let run () =
    let actual = Point.( * ) initial value in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_div_value name initial value expected =
  let run () =
    let actual = Point.( / ) initial value in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_add_points name left right expected =
  let run () =
    let actual = Point.( ++ ) left right in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_sub_points name left right expected =
  let run () =
    let actual = Point.( -- ) left right in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_mul_points name left right expected =
  let run () =
    let actual = Point.( ** ) left right in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_div_points name left right expected =
  let run () =
    let actual = Point.( // ) left right in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_of_triple name initial expected =
  let run () =
    let actual = Point.of_triple initial in
    Alcotest.(check pair_t) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_to_triple name initial expected =
  let run () =
    let actual = Point.to_triple initial in
    Alcotest.(check (triple int int int)) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let test_to_string name initial expected =
  let run () =
    let actual = Point.to_string initial in
    Alcotest.(check string) "are equal" expected actual
  in
  Alcotest.test_case name `Quick run

let () =
  let open Alcotest in
  run "Point_3d"
    [
      ("zero      : t", [ test_zero "is point with zero coordinates" ]);
      ( "create    : c -> c -> c-> t",
        [ test_create "creates a point with given coordinates" 8 4 2 ] );
      ( "compare   : t -> t -> int (x = x and y = y and z = z)",
        [
          test_compare "returns 0 when coordinates of both points are equal"
            { x = 4; y = 2; z = 5 } { x = 4; y = 2; z = 5 } 0;
        ] );
      ( "compare   : t -> t -> int (x > x and y < y and z = z )",
        [
          test_compare
            "returns 1 when coordinates when x of first point is greater"
            { x = 5; y = 2; z = 5 } { x = 4; y = 3; z = 5 } 1;
        ] );
      ( "compare   : t -> t -> int (x > x and y = y and z = z )",
        [
          test_compare
            "returns 1 when coordinates when x of first point is greater"
            { x = 5; y = 2; z = 5 } { x = 4; y = 2; z = 5 } 1;
        ] );
      ( "compare   : t -> t -> int (x > x and y > y and z = z )",
        [
          test_compare
            "returns 1 when coordinates when x of first point is greater"
            { x = 5; y = 3; z = 5 } { x = 4; y = 2; z = 5 } 1;
        ] );
      ( "compare   : t -> t -> int (x > x and y = y and z < z )",
        [
          test_compare
            "returns 1 when coordinates when x of first point is greater"
            { x = 5; y = 2; z = 5 } { x = 4; y = 2; z = 6 } 1;
        ] );
      ( "compare   : t -> t -> int (x > x and y = y and z > z )",
        [
          test_compare
            "returns 1 when coordinates when x of first point is greater"
            { x = 5; y = 2; z = 6 } { x = 4; y = 2; z = 5 } 1;
        ] );
      ( "compare   : t -> t -> int (x < x and y < y and z = z )",
        [
          test_compare
            "returns -1 when coordinates when x of first point is lesser"
            { x = 3; y = 2; z = 5 } { x = 4; y = 3; z = 5 } (-1);
        ] );
      ( "compare   : t -> t -> int (x < x and y = y and z = z )",
        [
          test_compare
            "returns -1 when coordinates when x of first point is lesser"
            { x = 3; y = 2; z = 5 } { x = 4; y = 2; z = 5 } (-1);
        ] );
      ( "compare   : t -> t -> int (x < x and y > y and z = z )",
        [
          test_compare
            "returns -1 when coordinates when x of first point is lesser"
            { x = 3; y = 3; z = 5 } { x = 4; y = 2; z = 5 } (-1);
        ] );
      ( "compare   : t -> t -> int (x < x and y = y and z < z )",
        [
          test_compare
            "returns -1 when coordinates when x of first point is lesser"
            { x = 3; y = 2; z = 5 } { x = 4; y = 2; z = 6 } (-1);
        ] );
      ( "compare   : t -> t -> int (x < x and y = y and z > z )",
        [
          test_compare
            "returns -1 when coordinates when x of first point is lesser"
            { x = 3; y = 2; z = 6 } { x = 4; y = 2; z = 5 } (-1);
        ] );
      ( "compare   : t -> t -> int (x = x and y > y and z < z )",
        [
          test_compare
            "returns 1 when coordinates when y of first point is greater"
            { x = 4; y = 3; z = 4 } { x = 4; y = 2; z = 5 } 1;
        ] );
      ( "compare   : t -> t -> int (x = x and y > y and z = z )",
        [
          test_compare
            "returns 1 when coordinates when y of first point is greater"
            { x = 4; y = 3; z = 5 } { x = 4; y = 2; z = 5 } 1;
        ] );
      ( "compare   : t -> t -> int (x = x and y > y and z > z )",
        [
          test_compare
            "returns 1 when coordinates when y of first point is greater"
            { x = 4; y = 3; z = 6 } { x = 4; y = 2; z = 5 } 1;
        ] );
      ( "compare   : t -> t -> int (x = x and y < y and z < z )",
        [
          test_compare
            "returns -1 when coordinates when y of first point is lesser"
            { x = 4; y = 1; z = 4 } { x = 4; y = 2; z = 5 } (-1);
        ] );
      ( "compare   : t -> t -> int (x = x and y < y and z = z )",
        [
          test_compare
            "returns -1 when coordinates when y of first point is lesser"
            { x = 4; y = 1; z = 5 } { x = 4; y = 2; z = 5 } (-1);
        ] );
      ( "compare   : t -> t -> int (x = x and y < y and z > z )",
        [
          test_compare
            "returns -1 when coordinates when y of first point is lesser"
            { x = 4; y = 1; z = 6 } { x = 4; y = 2; z = 5 } (-1);
        ] );
      ( "compare   : t -> t -> int (x = x and y = y and z > z )",
        [
          test_compare
            "returns 1 when coordinates when y of first point is greater"
            { x = 4; y = 2; z = 6 } { x = 4; y = 2; z = 5 } 1;
        ] );
      ( "compare   : t -> t -> int (x = x and y = y and z < z )",
        [
          test_compare
            "returns -1 when coordinates when y of first point is lesser"
            { x = 4; y = 2; z = 4 } { x = 4; y = 2; z = 5 } (-1);
        ] );
      ( "equal     : t -> t -> bool (x = x and y = y and z = z)",
        [
          test_equal "returns true when coordinates of both points are equal"
            { x = 4; y = 2; z = 6 } { x = 4; y = 2; z = 6 } true;
        ] );
      ( "equal     : t -> t -> bool (x != x and y = y and z = z)",
        [
          test_equal
            "returns false when x coordinates of both points aren't equal"
            { x = 3; y = 3; z = 5 } { x = 4; y = 2; z = 5 } false;
        ] );
      ( "equal     : t -> t -> bool (x = x and y != y z = z)",
        [
          test_equal
            "returns false when y coordinates of both points aren't equal"
            { x = 4; y = 3; z = 5 } { x = 4; y = 2; z = 5 } false;
        ] );
      ( "equal     : t -> t -> bool (x = x and y = y and z != z)",
        [
          test_equal
            "returns false when z coordinates of both points aren't equal"
            { x = 4; y = 2; z = 5 } { x = 4; y = 2; z = 6 } false;
        ] );
      ( "x         : t -> c",
        [ test_x "returns x coordinate of point" { x = 8; y = 4; z = 2 } 8 ] );
      ( "y         : t -> c",
        [ test_y "returns y coordinate of point" { x = 8; y = 4; z = 2 } 4 ] );
      ( "z         : t -> c",
        [ test_z "returns z coordinate of point" { x = 8; y = 4; z = 2 } 2 ] );
      ( "add       : t -> c -> t",
        [
          test_add_value "adds a value to both coordinates"
            { x = 2; y = 0; z = -4 } 2 { x = 4; y = 2; z = -2 };
        ] );
      ( "sub       : t -> c -> t",
        [
          test_sub_value "adds a value to both coordinates"
            { x = 4; y = 2; z = -4 } 2 { x = 2; y = 0; z = -6 };
        ] );
      ( "mul       : t -> c -> t",
        [
          test_mul_value "multiplies both coordinates on a value"
            { x = 4; y = 2; z = -4 } 2 { x = 8; y = 4; z = -8 };
        ] );
      ( "div       : t -> c -> t",
        [
          test_div_value "divides both coordinates on a value"
            { x = 4; y = 2; z = -4 } 2 { x = 2; y = 1; z = -2 };
        ] );
      ( "add       : t -> t -> t",
        [
          test_add_points "adds a value to both coordinates"
            { x = 2; y = 0; z = -4 } { x = -4; y = 2; z = 8 }
            { x = -2; y = 2; z = 4 };
        ] );
      ( "sub       : t -> t -> t",
        [
          test_sub_points "substracts a value to both coordinates"
            { x = 2; y = 0; z = -4 } { x = -2; y = -2; z = 8 }
            { x = 4; y = 2; z = -12 };
        ] );
      ( "mul       : t -> t -> t",
        [
          test_mul_points "multiplies a value to both coordinates"
            { x = 2; y = 4; z = -4 } { x = -4; y = 2; z = 8 }
            { x = -8; y = 8; z = -32 };
        ] );
      ( "div       : t -> t -> t",
        [
          test_div_points "divides a value to both coordinates"
            { x = 8; y = 4; z = -4 } { x = -4; y = 2; z = 2 }
            { x = -2; y = 2; z = -2 };
        ] );
      ( "of_triple : (c * c * c) -> t",
        [
          test_of_triple "creates point from pair of coordinates" (8, 4, -4)
            { x = 8; y = 4; z = -4 };
        ] );
      ( "to_triple : t -> (c * c * c)",
        [
          test_to_triple "creates point from pair of coordinates"
            { x = 8; y = 4; z = -4 } (8, 4, -4);
        ] );
      ( "to_string : t -> string",
        [
          test_to_string "creates string representation of point"
            { x = 8; y = -4; z = 3 } "{ x = 8; y = -4; z = 3 }";
        ] );
    ]
